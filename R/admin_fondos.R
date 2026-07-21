# =============================================================================
# ADMIN DE FONDOS (panel): agregar / quitar fondos desde el catalogo CMF.
# La lista viva de fondos vive en data/fondos_curados.csv (1 fila por fondo).
# R/fondos_pulso.R la lee con .aplicar_curados() y reconstruye FONDOS/CATEGORIAS/
# DATOS_FONDO/MAPEO_SEBRA. Aqui solo editamos ese CSV y lo persistimos a GitHub.
# El catalogo (run/serie/row correctos de la CMF) es el mismo del comparador.
# =============================================================================
if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# Columnas del CSV curado, en orden (debe calzar con .aplicar_curados / build_fondos_curados.R)
COLS_CURADOS <- c("orden","hoja","titulo","nombre_script","nombre_excel","es_manual",
                  "run","serie","row","tipoentidad","ticker_sebra",
                  "rent2024","rent2025","duracion","liquidez","moneda","tac")

# Sugerencia de nombre a mostrar a partir del nombre CMF (MAYUSCULAS y largo).
# Quita sufijos tipicos y pasa a Mayuscula Inicial; el usuario lo puede editar.
.nombre_sugerido <- function(nombre) {
  x <- trimws(nombre %||% "")
  if (!nzchar(x)) return("")
  x <- gsub("FONDO DE INVERSION", "", x, ignore.case = TRUE)
  x <- gsub("FONDO MUTUO", "", x, ignore.case = TRUE)
  x <- trimws(gsub("\\s+", " ", x))
  conectores <- c("de","del","la","el","los","las","y","e","en","a")
  palabras <- strsplit(x, " ")[[1]]
  palabras <- vapply(seq_along(palabras), function(i) {
    w <- palabras[i]
    if (!nzchar(w)) return(w)
    if (nchar(w) <= 4 && toupper(w) == w && grepl("[A-Z]", w)) return(w)  # siglas: AGF, USA, ETF, UF
    if (i > 1 && tolower(w) %in% conectores) return(tolower(w))           # conectores en minuscula
    paste0(toupper(substr(w, 1, 1)), tolower(substr(w, 2, nchar(w))))
  }, character(1))
  paste(palabras, collapse = " ")
}

# ---- Catalogo del comparador (todos los fondos CMF, con row correcto) ----
cargar_catalogo <- function(ruta = "fondos/data/catalogo_fondos_limpio.csv") {
  if (!file.exists(ruta)) return(NULL)
  df <- tryCatch(read.csv(ruta, stringsAsFactors = FALSE, fileEncoding = "UTF-8",
                          colClasses = "character"), error = function(e) NULL)
  if (is.null(df) || !nrow(df)) return(NULL)
  df$key   <- as.character(seq_len(nrow(df)))   # clave sintetica unica por fila
  df$label <- sprintf("%s — serie %s (%s)", df$nombre, df$serie, df$tipoentidad)
  df
}

# ---- Lista curada actual (la fuente de verdad de los fondos del dashboard) ----
cargar_curados <- function(ruta = "data/fondos_curados.csv") {
  if (!file.exists(ruta)) return(NULL)
  df <- tryCatch(read.csv(ruta, stringsAsFactors = FALSE, fileEncoding = "UTF-8",
                          colClasses = "character"), error = function(e) NULL)
  if (is.null(df) || !all(COLS_CURADOS %in% names(df))) return(NULL)
  df[, COLS_CURADOS, drop = FALSE]
}

# Titulo consistente por hoja (todas las filas de una hoja comparten el mismo
# titulo = el primer no-vacio; fallback al nombre de la hoja). Evita que al mover
# un fondo de categoria queden titulos distintos en una misma hoja.
normalizar_titulos <- function(cur) {
  if (is.null(cur) || !nrow(cur)) return(cur)
  for (h in unique(cur$hoja)) {
    idx <- which(cur$hoja == h)
    tit <- cur$titulo[idx]; tit <- tit[!is.na(tit) & nzchar(trimws(tit))]
    cur$titulo[idx] <- if (length(tit)) tit[1] else h
  }
  cur
}

# ---- Persistir la lista curada (local + commit a GitHub) ----
guardar_curados <- function(df, mensaje = "Panel admin: actualizar lista de fondos") {
  df <- df[, COLS_CURADOS, drop = FALSE]
  tf <- tempfile(fileext = ".csv")
  utils::write.csv(df, tf, row.names = FALSE, fileEncoding = "UTF-8")
  txt <- paste(readLines(tf, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  unlink(tf)
  store_write_text("fondos_curados.csv", txt, mensaje)
}

# ---- Re-armar los globals (FONDOS/CATEGORIAS/...) en la sesion viva ----
# Tras editar el CSV, re-fuente fondos_pulso.R para que el dashboard refleje el
# cambio sin reiniciar el proceso. Lee data/fondos_curados.csv (ya escrito local).
refrescar_fondos_globales <- function() {
  tryCatch({ source("R/fondos_pulso.R", local = FALSE); TRUE },
           error = function(e) { message("[admin] refrescar globals fallo: ", e$message); FALSE })
}

# ---- Construir la fila nueva a partir de una fila del catalogo + metadata ----
# Devuelve list(ok, df|msg). Valida duplicados y campos minimos.
agregar_fondo_curado <- function(cur, fila_cat, hoja, titulo, nombre, moneda = "CLP",
                                 rent2024 = "", rent2025 = "", duracion = "",
                                 liquidez = "", tac = "", ticker_sebra = "") {
  hoja   <- trimws(hoja %||% "")
  nombre <- trimws(nombre %||% "")
  if (!nzchar(hoja))   return(list(ok = FALSE, msg = "Falta la categoria (hoja)."))
  if (!nzchar(nombre)) return(list(ok = FALSE, msg = "Falta el nombre a mostrar."))
  na0 <- function(x) { x <- x %||% ""; if (length(x) != 1 || is.na(x)) "" else as.character(x) }
  crun <- na0(fila_cat$run); cserie <- na0(fila_cat$serie)
  if (is.null(fila_cat) || !nzchar(crun))
    return(list(ok = FALSE, msg = "Elige un fondo válido del catálogo."))
  # Comparaciones seguras (NA/"" -> FALSE; nunca NA que rompe el if).
  run_v   <- ifelse(is.na(cur$run), "", cur$run)
  serie_v <- ifelse(is.na(cur$serie), "", cur$serie)
  hoja_v  <- ifelse(is.na(cur$hoja), "", cur$hoja)
  mismo_fondo <- nzchar(run_v) & run_v == crun & serie_v == cserie
  # (1) mismo fondo en la MISMA hoja -> bloquear (con mensaje claro)
  dup_hoja <- mismo_fondo & hoja_v == hoja
  if (any(dup_hoja)) return(list(ok = FALSE, msg = paste0(
    "Ese fondo ya está en la hoja '", hoja, "' como '", cur$nombre_excel[which(dup_hoja)[1]], "'.")))
  # (2) mismo fondo en OTRA hoja -> permitido (comparable): reusa el nombre existente
  if (any(mismo_fondo)) {
    nombre <- cur$nombre_excel[which(mismo_fondo)[1]]
  } else if (nombre %in% c(cur$nombre_script, cur$nombre_excel)) {
    # (3) fondo nuevo pero el nombre choca con otro fondo distinto
    return(list(ok = FALSE, msg = paste0("Ya existe otro fondo llamado '", nombre, "'. Usa otro nombre.")))
  }

  orden_new <- suppressWarnings(max(as.integer(cur$orden), na.rm = TRUE))
  if (!is.finite(orden_new)) orden_new <- nrow(cur)
  titulo <- trimws(titulo %||% ""); if (!nzchar(titulo)) titulo <- hoja

  nueva <- data.frame(
    orden = as.character(orden_new + 1L), hoja = hoja, titulo = titulo,
    nombre_script = nombre, nombre_excel = nombre, es_manual = "FALSE",
    run = fila_cat$run, serie = fila_cat$serie, row = fila_cat$row,
    tipoentidad = fila_cat$tipoentidad %||% "", ticker_sebra = trimws(ticker_sebra %||% ""),
    rent2024 = trimws(rent2024 %||% ""), rent2025 = trimws(rent2025 %||% ""),
    duracion = trimws(duracion %||% ""), liquidez = trimws(liquidez %||% ""),
    moneda = moneda %||% "CLP", tac = trimws(tac %||% ""),
    stringsAsFactors = FALSE)
  list(ok = TRUE, df = rbind(cur, nueva[, COLS_CURADOS]))
}
