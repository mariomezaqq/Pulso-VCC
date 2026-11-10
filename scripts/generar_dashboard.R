# =============================================================================
# GENERAR EL DASHBOARD ESTATICO  (lo corre .github/workflows/dashboard.yml, o a mano)
# Arma index.html (en la raiz del repo) con los mismos calculos que la app Shiny
# (R/preparar.R). GitHub Pages lo sirve en https://mariomezaqq.github.io/Pulso-VCC/:
# abre al instante (sin esperar a que arranque R) y se puede instalar como app.
# Correr desde la raiz:  Rscript scripts/generar_dashboard.R [archivo_salida]
# =============================================================================
suppressMessages({ library(dplyr); library(lubridate); library(tibble); library(openxlsx)
                   library(jsonlite); library(readxl); library(httr) })
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
source("R/github_store.R")
for (f in c("fondos_pulso", "calculos", "dividendos", "ajustes", "compute", "ui_dashboard", "preparar"))
  source(file.path("R", paste0(f, ".R")))

salida <- commandArgs(trailingOnly = TRUE)[1]; if (is.na(salida)) salida <- "index.html"
URL_ADMIN <- "https://mariomezaqq.shinyapps.io/pulso-vcc/"

leer <- function(ruta, def) if (file.exists(ruta))
  tryCatch(read.csv(ruta, stringsAsFactors = FALSE, fileEncoding = "UTF-8", check.names = FALSE,
                    colClasses = c(Fecha = "character")), error = function(e) def) else def
vc <- leer("data/manuales_vc.csv", default_vc())
ov <- tryCatch(read.csv("data/dividendos_overrides.csv", stringsAsFactors = FALSE, fileEncoding = "UTF-8",
                        check.names = FALSE), error = function(e) default_ov())

t <- preparar_todo(vc, ov)
if (is.null(t)) stop("No hay data/series_vc.rds: no se puede generar el dashboard.")
logo_b64 <- jsonlite::base64_enc(readBin("www/logo.jpg", "raw", file.info("www/logo.jpg")$size))
gen <- substr(gsub("T", " ", t$datos$generado %||% ""), 1, 16)
html <- render_dashboard_html(t$pd, logo_b64, gen, estatico = TRUE, url_admin = URL_ADMIN)

# Si el contenido es igual al anterior (salvo nada), no se reescribe: el workflow no hace commit.
if (!file.exists(salida) || !identical(readLines(salida, warn = FALSE, encoding = "UTF-8"), strsplit(html, "\n", fixed = TRUE)[[1]]))
  writeLines(enc2utf8(html), salida, useBytes = TRUE)
message("Dashboard generado: ", salida, " (", round(file.size(salida) / 1024), " KB) | cierre ", t$pd$fecha_cierre, " | datos ", gen)
