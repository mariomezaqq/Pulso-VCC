# =============================================================================
# MIGRACION: aplana las 4 listas hardcodeadas de R/fondos_pulso.R
# (FONDOS, CATEGORIAS, DATOS_FONDO, MAPEO_SEBRA) a un unico CSV editable:
#   data/fondos_curados.csv  ->  1 fila por (categoria/hoja, fondo)
# Este CSV pasa a ser la fuente unica que leen la app y el cron.
# Correr desde la raiz del proyecto:  Rscript scripts/build_fondos_curados.R
# =============================================================================
setwd_ok <- file.exists("R/fondos_pulso.R")
if (!setwd_ok) stop("Corre este script desde la raiz del proyecto pulso-vcc-cloud.")

# Cargar SOLO las listas hardcodeadas (sin fallback al CSV: forzamos el hardcode)
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
options(pulso.hardcode_only = TRUE)
source("R/fondos_pulso.R", local = TRUE)

# Indices por clave para los joins
fondos_by_nombre <- list()
for (fd in FONDOS) fondos_by_nombre[[fd$nombre]] <- fd

sebra_by_excel <- list()
for (m in MAPEO_SEBRA) sebra_by_excel[[m$nombre_excel]] <- m$ticker_sebra

# DATOS_FONDO ya viene keyeado por nombre_excel
get_dato <- function(nombre_excel, campo) {
  d <- DATOS_FONDO[[nombre_excel]]
  if (is.null(d) || is.null(d[[campo]])) return("")
  as.character(d[[campo]])
}

filas <- list()
orden_global <- 0L
for (cat in CATEGORIAS) {
  hoja   <- cat$hoja   %||% ""
  titulo <- cat$titulo %||% hoja
  for (f in cat$fondos) {
    orden_global <- orden_global + 1L
    ns <- f$nombre_script %||% ""
    ne <- f$nombre_excel  %||% ""
    es_manual <- identical(ns, "__MANUAL__")
    fd <- if (!es_manual) fondos_by_nombre[[ns]] else NULL

    tk <- sebra_by_excel[[ne]]
    tk <- if (is.null(tk) || (length(tk) == 1 && is.na(tk))) "" else as.character(tk)

    filas[[length(filas) + 1L]] <- data.frame(
      orden        = orden_global,
      hoja         = hoja,
      titulo       = titulo,
      nombre_script= ns,
      nombre_excel = ne,
      es_manual    = es_manual,
      run          = if (!is.null(fd)) as.character(fd$run %||% "") else "",
      serie        = if (!is.null(fd)) as.character(fd$serie %||% "") else "",
      row          = if (!is.null(fd)) as.character(fd$row %||% "") else "",
      tipoentidad  = if (!is.null(fd)) as.character(fd$tipoentidad %||% "FIRES") else "",
      ticker_sebra = tk,
      rent2024     = get_dato(ne, "rent2024"),
      rent2025     = get_dato(ne, "rent2025"),
      duracion     = get_dato(ne, "duracion"),
      liquidez     = get_dato(ne, "liquidez"),
      moneda       = get_dato(ne, "moneda"),
      tac          = get_dato(ne, "tac"),
      stringsAsFactors = FALSE
    )
  }
}
df <- do.call(rbind, filas)

dir.create("data", showWarnings = FALSE)
write.csv(df, "data/fondos_curados.csv", row.names = FALSE, fileEncoding = "UTF-8")

cat("OK fondos_curados.csv:\n")
cat("  filas (categoria x fondo):", nrow(df), "\n")
cat("  fondos scrapeables unicos (run/serie):", length(unique(paste(df$run[df$run!=""], df$serie[df$run!=""]))), "\n")
cat("  hojas:", length(unique(df$hoja)), "->", paste(unique(df$hoja), collapse=" | "), "\n")
cat("  manuales:", sum(df$es_manual), "\n")
