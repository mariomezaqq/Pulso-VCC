# =============================================================================
# PUSH VALOR CUOTA -> SUPABASE DEL BLOTTER
# Toma el ultimo valor cuota de cada fondo scrapeado hoy (data/series_vc.rds) y
# lo sube a la tabla `valores_cuota` (nemo, fecha, valor, fondo_nombre) que usa
# el Blotter para el precio "a NAV" de las ordenes.
#
# Cruce nombre_script -> nombre_excel -> ticker_sebra: mismo metodo que usa
# R/ajustes.R (.mapas_fondos) para cruzar dividendos por ticker SEBRA.
# =============================================================================
suppressMessages({ library(httr); library(jsonlite) })

base <- tryCatch(dirname(dirname(normalizePath(sys.frame(1)$ofile))), error = function(e) getwd())
if (!dir.exists(file.path(base, "R"))) base <- getwd()
setwd(base)
source("R/fondos_pulso.R")   # FONDOS, CATEGORIAS, MAPEO_SEBRA (hardcode o desde fondos_curados.csv)

url <- Sys.getenv("SUPABASE_BLOTTER_URL", "")
key <- Sys.getenv("SUPABASE_BLOTTER_KEY", "")
if (!nzchar(url) || !nzchar(key)) {
  message("Faltan SUPABASE_BLOTTER_URL / SUPABASE_BLOTTER_KEY. Nada que hacer.")
  quit(save = "no", status = 0)
}

ruta_rds <- file.path(base, "data", "series_vc.rds")
if (!file.exists(ruta_rds)) {
  message("No existe data/series_vc.rds todavia. Nada que subir.")
  quit(save = "no", status = 0)
}
datos  <- readRDS(ruta_rds)
series <- datos$series
if (is.null(series) || !length(series)) {
  message("series_vc.rds sin series. Nada que subir.")
  quit(save = "no", status = 0)
}

excel_por_script <- list()
for (cat in CATEGORIAS) for (f in cat$fondos) excel_por_script[[f$nombre_script]] <- f$nombre_excel
sebra_por_excel <- list()
for (m in MAPEO_SEBRA) sebra_por_excel[[m$nombre_excel]] <- m$ticker_sebra

filas <- list()
for (fd in FONDOS) {
  h <- series[[fd$nombre]]
  if (is.null(h) || !nrow(h)) next

  nombre_excel <- excel_por_script[[fd$nombre]]
  if (is.null(nombre_excel)) nombre_excel <- fd$nombre
  nemo <- sebra_por_excel[[nombre_excel]]
  if (is.null(nemo) || is.na(nemo) || !nzchar(nemo)) next

  ult <- h[order(h$fecha), ][nrow(h), ]
  filas[[length(filas) + 1]] <- list(
    nemo         = nemo,
    fecha        = as.character(ult$fecha[1]),
    valor        = as.numeric(ult$valor_cuota[1]),
    fondo_nombre = nombre_excel
  )
}

if (!length(filas)) {
  message("Ningun fondo con ticker_sebra mapeado en esta corrida. Nada que subir.")
  quit(save = "no", status = 0)
}

resp <- tryCatch(
  POST(
    url = paste0(url, "/rest/v1/valores_cuota?on_conflict=nemo,fecha"),
    add_headers(
      apikey        = key,
      Authorization = paste("Bearer", key),
      `Content-Type`= "application/json",
      Prefer        = "resolution=merge-duplicates,return=minimal"
    ),
    body = toJSON(filas, auto_unbox = TRUE)
  ),
  error = function(e) { message("Error de red subiendo a Supabase: ", e$message); NULL }
)

if (is.null(resp)) quit(save = "no", status = 1)
if (status_code(resp) >= 300) {
  message("Error Supabase (", status_code(resp), "): ", content(resp, "text", encoding = "UTF-8"))
  quit(save = "no", status = 1)
}
message("OK: valor cuota de ", length(filas), " fondos subido a valores_cuota (", format(Sys.time(), "%Y-%m-%d %H:%M"), ").")
