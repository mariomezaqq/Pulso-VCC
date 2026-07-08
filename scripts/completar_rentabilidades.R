# =============================================================================
# COMPLETAR RENTABILIDADES 2024/2025 (lo corre el cron tras el scrape diario)
# Para los fondos de data/fondos_curados.csv que tengan rent2024 o rent2025
# VACIO (tipicamente un fondo recien agregado desde el panel), calcula la
# rentabilidad por anio calendario a partir del VALOR CUOTA, con el MISMO ajuste
# del dashboard (dividendos FIRES/FINRE + factor de reparto RGFMU):
#     rent_anio = VC_ajust(31-dic-anio) / VC_ajust(31-dic-(anio-1)) - 1
# Escribe el resultado de vuelta en el CSV (solo rellena lo vacio, no pisa lo
# curado a mano). El commit lo hace el workflow.
# =============================================================================
suppressMessages({ library(httr); library(rvest); library(dplyr); library(tidyr)
                   library(stringr); library(lubridate); library(tibble) })

base <- tryCatch(dirname(dirname(normalizePath(sys.frame(1)$ofile))), error = function(e) getwd())
if (!dir.exists(file.path(base, "R"))) base <- getwd()
setwd(base)
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
source("R/fondos_pulso.R")   # FONDOS/CATEGORIAS/MAPEO_SEBRA desde fondos_curados.csv
source("R/scraper.R"); source("R/credenciales.R")
source("R/ajustes.R"); source("R/dividendos.R"); source("R/compute.R")

RUTA <- "data/fondos_curados.csv"
if (!file.exists(RUTA)) { message("[rent] sin fondos_curados.csv; nada que hacer."); quit(save = "no") }
cur <- read.csv(RUTA, stringsAsFactors = FALSE, fileEncoding = "UTF-8", colClasses = "character")

# fondos scrapeables con al menos un anio de rent VACIO
vacio <- function(x) is.na(x) | !nzchar(trimws(x))
es_man <- tolower(trimws(cur$es_manual)) %in% c("true","1","yes","si","sí")
idx <- which(!es_man & nzchar(cur$run) & (vacio(cur$rent2024) | vacio(cur$rent2025)))
if (!length(idx)) { message("[rent] no hay fondos con rent vacia. Nada que completar."); quit(save = "no") }
message("[rent] fondos a completar: ", length(idx), " -> ",
        paste(cur$nombre_excel[idx], collapse = " | "))

cred <- cargar_credenciales()
# dividendos del boletin (para el ajuste FIRES/FINRE), desde 2024 hacia adelante
div <- tryCatch(cargar_dividendos_pulso("data/dividendos.xlsx", fecha_inicio = as.Date("2024-01-01")),
                error = function(e) list())

fmt_pct <- function(x, dec = 2) if (is.na(x)) "" else
  paste0(formatC(x * 100, format = "f", digits = dec, decimal.mark = ","), "%")

d2023 <- as.Date("2023-12-31"); d2024 <- as.Date("2024-12-31"); d2025 <- as.Date("2025-12-31")
cambios <- 0L
for (i in idx) {
  ns   <- cur$nombre_script[i]
  fondo <- list(nombre = ns, run = cur$run[i], serie = cur$serie[i],
                row = cur$row[i], tipoentidad = cur$tipoentidad[i] %||% "FIRES")
  h <- tryCatch(scrapear_fondo(fondo, d2023 - 15, d2025, cred$token, cred$cookies),
                error = function(e) { message("[rent] scrape err ", ns, ": ", e$message); NULL })
  if (is.null(h) || !nrow(h)) { message("[rent] sin serie para ", ns); next }
  # mismo ajuste que el dashboard (RGFMU factor reparto; FIRES/FINRE dividendos)
  series_aj <- tryCatch(enriquecer_series_ajustadas(setNames(list(h), ns), div),
                        error = function(e) setNames(list(h), ns))
  hh  <- series_aj[[ns]]
  col <- if ("valor_cuota_ajustado" %in% names(hh)) "valor_cuota_ajustado" else "valor_cuota"
  vc  <- function(dd) .vc_a_fecha(hh, dd, col)
  v23 <- vc(d2023); v24 <- vc(d2024); v25 <- vc(d2025)
  r24 <- if (!is.na(v23) && !is.na(v24) && v23 != 0) v24 / v23 - 1 else NA_real_
  r25 <- if (!is.na(v24) && !is.na(v25) && v24 != 0) v25 / v24 - 1 else NA_real_
  if (vacio(cur$rent2024[i]) && !is.na(r24)) { cur$rent2024[i] <- fmt_pct(r24); cambios <- cambios + 1L }
  if (vacio(cur$rent2025[i]) && !is.na(r25)) { cur$rent2025[i] <- fmt_pct(r25); cambios <- cambios + 1L }
  message(sprintf("[rent] %-30s 2024=%s  2025=%s  (col=%s)", ns,
                  fmt_pct(r24) %||% "NA", fmt_pct(r25) %||% "NA", col))
  Sys.sleep(0.8)
}

if (cambios > 0) {
  write.csv(cur, RUTA, row.names = FALSE, fileEncoding = "UTF-8")
  message("[rent] fondos_curados.csv actualizado (", cambios, " valores).")
} else {
  message("[rent] nada que escribir (sin series validas o ya completos).")
}
