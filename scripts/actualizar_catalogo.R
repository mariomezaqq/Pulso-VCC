# =============================================================================
# ACTUALIZAR CATALOGO DE FONDOS CMF  (workflow mensual catalogo.yml, o manual)
# Lee de la CMF la lista de fondos VIGENTES (FIRES, FINRE, RGFMU) con su run,
# nombre y row, y las series vigentes de cada uno; reescribe
# fondos/data/catalogo_fondos_limpio.csv (buscador del panel de administracion).
# La categoria se mantiene del catalogo anterior (por run + serie, o por run);
# los fondos nuevos quedan "Sin clasificacion".
# Si la CMF no responde o fallan demasiados fondos, NO sobrescribe el catalogo.
# Correr desde la raiz del proyecto:  Rscript scripts/actualizar_catalogo.R
# =============================================================================
suppressMessages({ library(httr); library(xml2) })
source("R/admin_fondos.R")   # series_fondo_cmf()

RUTA  <- "fondos/data/catalogo_fondos_limpio.csv"
TIPOS <- c("FIRES", "FINRE", "RGFMU")
viejo <- read.csv(RUTA, colClasses = "character", fileEncoding = "UTF-8")

# ---- 1) Lista de fondos vigentes por tipo ----
listar_fondos <- function(tipo) {
  url <- paste0("https://www.cmfchile.cl/institucional/mercados/consulta.php?mercado=V&Estado=VI&entidad=", tipo)
  r <- NULL
  for (i in 1:3) {
    r <- tryCatch(GET(url, user_agent("Mozilla/5.0"), timeout(90)), error = function(e) NULL)
    if (!is.null(r) && status_code(r) == 200) break
    Sys.sleep(5 * i)
  }
  if (is.null(r) || status_code(r) != 200) stop("no se pudo leer la lista de fondos ", tipo)
  a <- xml_find_all(read_html(content(r, "text", encoding = "UTF-8")), "//a[contains(@href,'entidad.php')]")
  href <- xml_attr(a, "href"); txt <- trimws(gsub("\\s+", " ", xml_text(a)))
  ok <- grepl("[?&]rut=[0-9]+", href)
  df <- data.frame(run = sub(".*[?&]rut=([0-9]+).*", "\\1", href[ok]),
                   row = gsub(" ", "+", vapply(sub(".*[?&]row=([^&]*).*", "\\1", href[ok]),
                                               utils::URLdecode, "")),
                   txt = txt[ok])
  # cada fondo trae 2 links: el RUN ("9152-9") y el nombre
  nom <- df[!grepl("^[0-9]+-[0-9Kk]$", df$txt) & nzchar(df$txt), ]
  nom <- nom[!duplicated(nom$run), ]
  data.frame(run = nom$run, row = nom$row, nombre = nom$txt, tipoentidad = tipo)
}
fondos <- do.call(rbind, lapply(TIPOS, listar_fondos))
fondos <- fondos[!duplicated(paste(fondos$run, fondos$tipoentidad)), ]
message("Fondos vigentes en la CMF: ", nrow(fondos), " (",
        paste(names(table(fondos$tipoentidad)), table(fondos$tipoentidad), collapse = ", "), ")")
if (nrow(fondos) < 0.8 * length(unique(viejo$run)))
  stop("La CMF devolvio muy pocos fondos (", nrow(fondos), "): no se toca el catalogo.")

# ---- 2) Series vigentes de cada fondo ----
filas <- list(); fallas <- character()
cat_de <- function(run, serie) {
  k <- viejo$categoria[viejo$run == run & viejo$serie == serie]
  if (!length(k)) k <- viejo$categoria[viejo$run == run]
  if (length(k) && nzchar(k[1])) k[1] else "Sin clasificacion"
}
# 4 consultas en paralelo (~1,3 s cada una): ~10 min para ~1.400 fondos en vez de ~45
leer_series <- function(i) {
  f <- as.list(fondos[i, ])
  for (intento in 1:2) {
    s <- tryCatch(series_fondo_cmf(f), error = function(e) NULL)
    if (!is.null(s)) return(s)
    Sys.sleep(3)
  }
  NULL
}
res <- parallel::mclapply(seq_len(nrow(fondos)), leer_series, mc.cores = 4, mc.preschedule = FALSE)
for (i in seq_len(nrow(fondos))) {
  f <- as.list(fondos[i, ]); series <- res[[i]]
  if (inherits(series, "try-error")) series <- NULL
  if (is.null(series)) {
    fallas <- c(fallas, paste0(f$nombre, " (", f$run, ")"))
    previas <- viejo[viejo$run == f$run & viejo$tipoentidad == f$tipoentidad, ]   # se conservan
    if (nrow(previas)) filas[[length(filas) + 1]] <- previas
  } else if (length(series)) {
    filas[[length(filas) + 1]] <- data.frame(
      nombre = f$nombre, run = f$run, serie = series, tipoentidad = f$tipoentidad, row = f$row,
      categoria = vapply(series, function(s) cat_de(f$run, s), ""),
      instrumento = if (f$tipoentidad == "RGFMU") "CFM" else "CFI")
  }
}
if (length(fallas) > 0.15 * nrow(fondos))
  stop("Fallaron ", length(fallas), " fondos (", round(100 * length(fallas) / nrow(fondos)),
       "%): no se toca el catalogo. Ej: ", paste(head(fallas, 5), collapse = "; "))

nuevo <- do.call(rbind, filas)
nuevo <- nuevo[order(nuevo$nombre, nuevo$serie), names(viejo)]
nuevo <- nuevo[!duplicated(paste(nuevo$run, nuevo$serie, nuevo$tipoentidad)), ]

# ---- 3) Resumen y guardado ----
k_v <- paste(viejo$run, viejo$serie); k_n <- paste(nuevo$run, nuevo$serie)
nombre_v <- setNames(viejo$nombre, viejo$run)
renombrados <- unique(nuevo$run[nuevo$run %in% names(nombre_v) &
                                toupper(nuevo$nombre) != toupper(nombre_v[nuevo$run])])
message("Catalogo: ", nrow(viejo), " -> ", nrow(nuevo), " series | fondos ",
        length(unique(viejo$run)), " -> ", length(unique(nuevo$run)),
        " | series nuevas: ", sum(!k_n %in% k_v), " | que ya no estan: ", sum(!k_v %in% k_n),
        " | fondos renombrados: ", length(renombrados),
        if (length(fallas)) paste0(" | sin series (se conservaron las previas): ", length(fallas)) else "")
write.csv(nuevo, RUTA, row.names = FALSE, fileEncoding = "UTF-8")
message("Guardado ", RUTA)
