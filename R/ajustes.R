# =============================================================================
# AJUSTES DE RENTABILIDAD: dividendos (FIRES/FINRE) + factor de reparto (RGFMU)
# Produce valor_cuota_ajustado en cada serie; compute.R lo usa automaticamente.
#
# - Dividendos: del Boletin Bolsa (hoja "Dividendos CFI nacionales"), cruzados
#   por TICKER SEBRA via MAPEO_SEBRA (mismo metodo que el Excel del Comite),
#   + correcciones manuales (dividendos_overrides.csv).
# - Factor de reparto: viene en la propia serie scrapeada (RGFMU).
# =============================================================================
suppressMessages({ library(readxl); library(dplyr); library(lubridate); library(tibble) })

# Mapas script -> nombre_excel y script -> tipoentidad. Se construyen en tiempo
# de ejecucion (no al cargar) para no depender del orden de source().
.mapas_fondos <- function() {
  me <- list(); for (cat in CATEGORIAS) for (f in cat$fondos) me[[f$nombre_script]] <- f$nombre_excel
  td <- list(); for (fd in FONDOS) td[[fd$nombre]] <- if (!is.null(fd$tipoentidad)) fd$tipoentidad else "FIRES"
  list(excel = me, tipo = td)
}

.parse_fecha_div <- function(x) {
  x <- trimws(as.character(x)); out <- as.Date(rep(NA_real_, length(x)), origin = "1970-01-01")
  num <- suppressWarnings(as.numeric(x)); ser <- !is.na(num) & num > 30000 & num < 80000
  out[ser] <- as.Date(num[ser], origin = "1899-12-30")
  # OJO: los formatos día-primero van ANTES que los año-primero. Si no, "%Y-%m-%d"
  # consume parcialmente un "13-06-2026" y lo parsea mal como 0013-06-20.
  for (fmt in c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d","%Y/%m/%d","%m/%d/%Y")) {
    f <- is.na(out) & nzchar(x) & x != "NA"; if (!any(f)) break
    cand <- suppressWarnings(as.Date(x[f], format = fmt)); out[which(f)[!is.na(cand)]] <- cand[!is.na(cand)]
  }
  out
}
.parse_monto_div <- function(x) {
  x <- gsub("\\s", "", as.character(x)); tc <- grepl(",", x, fixed = TRUE)
  x[tc] <- gsub("\\.", "", x[tc]); x[tc] <- gsub(",", ".", x[tc], fixed = TRUE)
  suppressWarnings(as.numeric(x))
}

# Lookup (nombre_excel, fecha_limite) -> monto_final desde dividendos_calculados.csv.
# Resuelve el caso "monto total" (reparto SIN monto por-cuota en el boletin), que
# procesar_dividendos.R ya calculo como total/cuotas del dia limite. Mapea las filas
# calculadas (run+serie) al nombre_excel del pulso via la lista curada (solo FIRES/
# FINRE; los RGFMU se ajustan por factor de reparto de la serie, no por el boletin).
.calc_lookup_div <- function() {
  # En la app lo mantiene al dia sincronizar_datos(); en el cron viene del checkout.
  ruta <- if (file.exists("data/dividendos_calculados.csv")) "data/dividendos_calculados.csv" else NULL
  if (is.null(ruta)) return(list())
  df <- tryCatch(read.csv(ruta, stringsAsFactors = FALSE, fileEncoding = "UTF-8", check.names = FALSE),
                 error = function(e) NULL)
  if (is.null(df) || !nrow(df) || !all(c("run","serie","fecha_limite","monto_final") %in% names(df)))
    return(list())
  ex <- list(); for (cat in CATEGORIAS) for (f in cat$fondos) ex[[f$nombre_script]] <- f$nombre_excel
  rs <- list()
  for (fd in FONDOS) {
    tipo <- if (!is.null(fd$tipoentidad)) fd$tipoentidad else "FIRES"
    if (identical(tipo, "RGFMU")) next
    run <- as.character(fd$run %||% ""); if (!nzchar(run)) next
    rs[[paste0(run, "|", as.character(fd$serie %||% ""))]] <- ex[[fd$nombre]] %||% fd$nombre
  }
  out <- list()
  for (i in seq_len(nrow(df))) {
    nm <- rs[[paste0(as.character(df$run[i]), "|", as.character(df$serie[i]))]]
    if (is.null(nm)) next
    fl <- suppressWarnings(as.Date(as.character(df$fecha_limite[i])))
    mo <- suppressWarnings(as.numeric(df$monto_final[i]))
    if (is.na(fl) || is.na(mo) || mo <= 0) next
    out[[paste0(nm, "|", as.character(fl))]] <- mo
  }
  out
}

# Extrae dividendos de UNA hoja del boletin, cruzando por TICKER SEBRA (columna
# "Pesos"/"Dolares"). Toma el monto "Por Accion / cuota"; si viene vacio (reparto
# como monto total), lo rescata de dividendos_calculados.csv (arg calc).
.leer_hoja_div <- function(ruta_xlsx, hoja, skip, lookup, fecha_inicio, calc) {
  if (!hoja %in% tryCatch(excel_sheets(ruta_xlsx), error = function(e) character(0))) return(list())
  df <- tryCatch(suppressMessages(read_excel(ruta_xlsx, sheet = hoja, skip = skip,
                                             col_names = TRUE, col_types = "text")),
                 error = function(e) NULL)
  if (is.null(df) || nrow(df) == 0) return(list())
  cols <- tolower(colnames(df))
  ic <- which(grepl("pesos", cols))[1];   if (is.na(ic)) ic <- 3L
  iu <- which(grepl("d.lar", cols))[1];   if (is.na(iu)) iu <- 4L
  im <- which(grepl("por acci|cuota", cols))[1]; if (is.na(im)) im <- 9L
  il <- which(grepl("l.mi", cols))[1];    if (is.na(il)) il <- 11L
  res <- list()
  for (i in seq_len(nrow(df))) {
    nc <- toupper(trimws(as.character(df[[ic]][i]))); nu <- toupper(trimws(as.character(df[[iu]][i])))
    nombre <- if (nzchar(nc) && !is.null(lookup[[nc]])) lookup[[nc]]
              else if (nzchar(nu) && !is.null(lookup[[nu]])) lookup[[nu]] else NULL
    if (is.null(nombre)) next
    fl <- .parse_fecha_div(df[[il]][i]); if (is.na(fl) || fl < fecha_inicio) next
    mo <- .parse_monto_div(df[[im]][i])
    if (is.na(mo) || mo <= 0) {                       # sin por-cuota -> usar el calculado
      mo <- calc[[paste0(nombre, "|", as.character(fl))]]
      if (is.null(mo) || is.na(mo) || mo <= 0) next
    }
    res[[nombre]] <- rbind(res[[nombre]], data.frame(fecha_limite = fl, monto = mo))
  }
  res
}

# Hojas del Boletin que se leen, con las filas a saltar hasta la cabecera.
HOJAS_BOLETIN <- list(list("Dividendos CFI nacionales", 11L), list("Repartos CFI-CFM", 10L))

#' Deja del Boletin solo lo que se usa: las 2 hojas de HOJAS_BOLETIN, como valores
#' (sin formatos/tablas/nombres), filas con fecha limite desde `desde` y sin las
#' columnas de texto largo (lugar de pago, observaciones). La cabecera queda en la
#' misma fila, asi cargar_dividendos_pulso() lee igual el original y el recortado.
#' Pesa ~1/3 y se sube y lee mas rapido. Error si no encuentra la estructura.
recortar_boletin <- function(src, dest, desde = as.Date("2024-01-01")) {
  wb <- openxlsx::createWorkbook()
  for (hs in HOJAS_BOLETIN) {
    d <- read_excel(src, sheet = hs[[1]], col_names = FALSE, col_types = "text",
                    .name_repair = "minimal")
    hdr <- which(trimws(d[[1]]) == "Nombre Emisor")[1]
    if (is.na(hdr)) stop("no se encontro la cabecera de la hoja '", hs[[1]], "'")
    cab <- tolower(unlist(d[hdr, ]))
    il <- which(grepl("l.mi", cab))[1]
    if (is.na(il)) stop("no se encontro la columna de fecha limite en '", hs[[1]], "'")
    datos <- d[-seq_len(hdr), ]
    fl <- .parse_fecha_div(datos[[il]])
    datos <- datos[is.na(fl) | fl >= desde, ]
    for (j in which(grepl("lugar|observ", cab))) datos[[j]] <- NA_character_
    openxlsx::addWorksheet(wb, hs[[1]])
    # read_excel salta las filas vacias iniciales: se escribe desde la fila que
    # deja la cabecera justo despues de las `skip` filas que espera el lector
    openxlsx::writeData(wb, hs[[1]], rbind(d[seq_len(hdr), ], datos), colNames = FALSE,
                        startRow = hs[[2]] + 2L - hdr)
  }
  openxlsx::saveWorkbook(wb, dest, overwrite = TRUE)
  dest
}

#' Tickers del Boletin por run + serie. El Boletin trae nombre + serie + ticker
#' (columnas Pesos/Dolares) pero no el run; dividendos_calculados.csv trae
#' nombre + serie + run. Cruzando ambos sale run|serie -> ticker (se prefiere el
#' de pesos). Se cachea hasta que cambie alguno de los dos archivos.
#' @return data.frame(run, serie, ticker, otros) - `otros`: el otro ticker valido
.cache_tickers <- new.env()
tickers_boletin <- function(ruta_xlsx = "data/dividendos.xlsx",
                            ruta_calc = "data/dividendos_calculados.csv") {
  vacio <- data.frame(run = character(), serie = character(), ticker = character(), otros = character())
  if (!file.exists(ruta_xlsx) || !file.exists(ruta_calc)) return(vacio)
  clave <- paste(file.info(c(ruta_xlsx, ruta_calc))$mtime, collapse = "|")
  if (identical(.cache_tickers$clave, clave)) return(.cache_tickers$df)
  b <- do.call(rbind, lapply(HOJAS_BOLETIN, function(hs) {
    d <- tryCatch(suppressMessages(read_excel(ruta_xlsx, sheet = hs[[1]], skip = hs[[2]],
                                              col_types = "text")), error = function(e) NULL)
    if (is.null(d) || ncol(d) < 4) return(NULL)
    data.frame(nombre = trimws(d[[1]]), serie = trimws(d[[2]]),
               pesos = trimws(d[[3]]), dolares = trimws(d[[4]]))
  }))
  calc <- tryCatch(read.csv(ruta_calc, colClasses = "character", fileEncoding = "UTF-8"),
                   error = function(e) NULL)
  if (is.null(b) || is.null(calc) || !all(c("nombre", "serie", "run") %in% names(calc))) return(vacio)
  calc <- unique(calc[!is.na(calc$run) & nzchar(calc$run) & calc$run != "NA", c("nombre", "serie", "run")])
  m <- unique(merge(unique(b), calc, by = c("nombre", "serie")))
  ok <- function(x) !is.na(x) & nzchar(x) & x != "-"
  m$ticker <- ifelse(ok(m$pesos), m$pesos, m$dolares)
  m$otros  <- ifelse(ok(m$pesos) & ok(m$dolares), m$dolares, "")
  m <- m[ok(m$ticker), ]
  df <- m[!duplicated(paste(m$run, m$serie)), c("run", "serie", "ticker", "otros")]
  .cache_tickers$clave <- clave; .cache_tickers$df <- df
  df
}

#' Ticker sugerido para un fondo (run + serie), o "" si el Boletin no lo trae.
ticker_sugerido <- function(run, serie) {
  t <- tickers_boletin()
  i <- which(t$run == as.character(run) & t$serie == as.character(serie))[1]
  if (is.na(i)) "" else t$ticker[i]
}

#' Revisa los tickers de la lista curada contra el Boletin: fondos de inversion
#' sin ticker cuando el Boletin si trae uno para su run + serie, o con un ticker
#' que el Boletin asigna a otra serie. Sin estos, los dividendos no se suman
#' (o se suman los de otra serie) y la rentabilidad sale mal.
#' @return data.frame(Fondo, Run, Serie, Actual, Boletin, Problema)
revisar_tickers <- function(cur) {
  t <- tickers_boletin()
  out <- data.frame(Fondo = character(), Run = character(), Serie = character(),
                    Actual = character(), Boletin = character(), Problema = character())
  if (is.null(cur) || !nrow(cur) || !nrow(t)) return(out)
  es_man <- tolower(trimws(cur$es_manual)) %in% c("true", "1", "yes", "si", "sí")
  for (i in which(!es_man & cur$tipoentidad != "RGFMU" & nzchar(cur$run))) {
    j <- which(t$run == cur$run[i] & t$serie == cur$serie[i])[1]
    if (is.na(j)) next
    actual <- toupper(trimws(cur$ticker_sebra[i]))
    validos <- toupper(c(t$ticker[j], t$otros[j]))
    problema <- if (!nzchar(actual)) "Sin ticker: no se suman sus dividendos"
                else if (!actual %in% validos) "El ticker no corresponde a esta serie en el Boletín"
                else ""
    if (nzchar(problema)) out[nrow(out) + 1, ] <- list(cur$nombre_excel[i], cur$run[i], cur$serie[i],
                                                       cur$ticker_sebra[i], t$ticker[j], problema)
  }
  unique(out)
}

#' Carga dividendos del Boletin y los devuelve por nombre_excel. Lee AMBAS hojas
#' relevantes ("Dividendos CFI nacionales" + "Repartos CFI-CFM"); los repartos que
#' vienen solo como monto total se resuelven con dividendos_calculados.csv.
#' @return named list nombre_excel -> tibble(fecha_limite, monto)
cargar_dividendos_pulso <- function(ruta_xlsx, fecha_inicio = as.Date("2025-12-31")) {
  if (is.null(ruta_xlsx) || !file.exists(ruta_xlsx)) return(list())
  lookup <- list()
  for (m in MAPEO_SEBRA) { t <- m$ticker_sebra
    if (!is.null(t) && !is.na(t) && nzchar(t)) lookup[[toupper(trimws(t))]] <- m$nombre_excel }
  if (!length(lookup)) return(list())
  calc <- tryCatch(.calc_lookup_div(), error = function(e) list())

  res <- list()
  for (hs in HOJAS_BOLETIN) {
    parcial <- .leer_hoja_div(ruta_xlsx, hs[[1]], hs[[2]], lookup, fecha_inicio, calc)
    for (nm in names(parcial)) res[[nm]] <- rbind(res[[nm]], parcial[[nm]])
  }
  lapply(res, function(d) distinct(arrange(d, fecha_limite), fecha_limite, .keep_all = TRUE))
}

#' Mezcla correcciones manuales (data.frame Fondo, `Fecha limite`, Monto) en la lista.
fusionar_overrides <- function(div_por_fondo, ov_df) {
  if (is.null(ov_df) || !nrow(ov_df)) return(div_por_fondo)
  # La columna de fecha puede llegar como "Fecha limite" o "Fecha.limite"
  # (read.csv con check.names=TRUE convierte el espacio en punto).
  col_fl <- if (!is.null(ov_df[["Fecha limite"]])) ov_df[["Fecha limite"]] else ov_df[["Fecha.limite"]]
  for (i in seq_len(nrow(ov_df))) {
    nm <- trimws(as.character(ov_df$Fondo[i])); if (length(nm) != 1 || !nzchar(nm)) next
    fl <- .parse_fecha_div(if (is.null(col_fl)) NA else col_fl[i])
    mo <- suppressWarnings(as.numeric(ov_df$Monto[i]))
    if (length(fl) != 1 || is.na(fl) || length(mo) != 1 || is.na(mo) || mo <= 0) next
    nuevo <- rbind(div_por_fondo[[nm]], data.frame(fecha_limite = fl, monto = mo))
    div_por_fondo[[nm]] <- distinct(arrange(nuevo, fecha_limite), fecha_limite, .keep_all = TRUE)
  }
  div_por_fondo
}

#' Agrega valor_cuota_ajustado a cada serie: RGFMU por factor de reparto,
#' FIRES/FINRE por dividendos aditivos.
enriquecer_series_ajustadas <- function(series, div_por_fondo = list()) {
  mp <- .mapas_fondos()
  for (nm in names(series)) {
    h <- series[[nm]]; if (is.null(h) || !nrow(h)) next
    tipo <- if (!is.null(mp$tipo[[nm]])) mp$tipo[[nm]] else "FIRES"
    if (identical(tipo, "RGFMU")) {
      series[[nm]] <- aplicar_factor_reparto(h)
    } else {
      excel <- if (!is.null(mp$excel[[nm]])) mp$excel[[nm]] else nm
      ev <- div_por_fondo[[excel]]
      ev_t <- if (!is.null(ev)) tibble(fecha_limite = as.Date(ev$fecha_limite),
                                       monto = as.numeric(ev$monto), moneda = NA_character_) else NULL
      series[[nm]] <- aplicar_dividendos_a_historico(h, ev_t, moneda_vc = "$")
    }
  }
  series
}
