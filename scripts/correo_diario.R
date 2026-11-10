# =============================================================================
# CORREO DIARIO DEL PULSO  (lo corre .github/workflows/correo.yml, o a mano)
# Genera en la carpeta de salida (default "correo/"):
#   asunto.txt, cuerpo.html, cuerpo.txt  -> texto del correo (R/correo.R)
#   adjunto.xlsx                          -> reporte de rentabilidad (file_show)
#   meta.json                             -> cierre, nombre del adjunto, avisos
# Usa los mismos datos y calculos que el dashboard (R/preparar.R). NO envia nada:
# el envio lo hace el workflow con estos archivos.
# Correr desde la raiz:  Rscript scripts/correo_diario.R [carpeta_salida]
# =============================================================================
suppressMessages({ library(dplyr); library(lubridate); library(tibble); library(openxlsx)
                   library(jsonlite); library(readxl); library(httr) })
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
source("R/github_store.R")   # solo por store_path / %||% que usan los demas (no escribe nada)
for (f in c("fondos_pulso", "calculos", "dividendos", "ajustes", "compute", "excel_export", "preparar", "correo"))
  source(file.path("R", paste0(f, ".R")))

out <- commandArgs(trailingOnly = TRUE)[1]; if (is.na(out)) out <- "correo"
dir.create(out, showWarnings = FALSE, recursive = TRUE)

leer <- function(ruta, def) if (file.exists(ruta))
  tryCatch(read.csv(ruta, stringsAsFactors = FALSE, fileEncoding = "UTF-8", check.names = FALSE,
                    colClasses = c(Fecha = "character")), error = function(e) def) else def
vc <- leer("data/manuales_vc.csv", default_vc())
ov <- tryCatch(read.csv("data/dividendos_overrides.csv", stringsAsFactors = FALSE, fileEncoding = "UTF-8",
                        check.names = FALSE), error = function(e) default_ov())

t <- preparar_todo(vc, ov)
if (is.null(t)) stop("No hay data/series_vc.rds: no se puede armar el correo.")
txt <- texto_correo(t$pd)
cuerpo <- cuerpo_correo(txt, firma = Sys.getenv("CORREO_FIRMA", ""))
fc <- txt$fecha_cierre

adjunto <- sprintf("Pulso VCC - Cierre %s.xlsx", format(fc, "%d-%m-%Y"))
generar_file_show(t$pd, t$datos, t$series_aj, t$div, vc, file.path(out, "adjunto.xlsx"))

writeLines(txt$asunto, file.path(out, "asunto.txt"), useBytes = TRUE)
writeLines(enc2utf8(cuerpo$html), file.path(out, "cuerpo.html"), useBytes = TRUE)
writeLines(enc2utf8(cuerpo$texto), file.path(out, "cuerpo.txt"), useBytes = TRUE)
avisos <- c(t$pd$alertas$global %||% character(0),
            if (length(txt$parrafos) < 4) "Falta algun parrafo por datos faltantes (indices, Treasury o instrumentos de la casa).")
write_json(list(cierre = format(fc), asunto = txt$asunto, adjunto = adjunto,
                datos_generados = t$datos$generado %||% "", avisos = avisos),
           file.path(out, "meta.json"), auto_unbox = TRUE, pretty = TRUE)
message("Correo generado en ", out, "/ | ", txt$asunto, if (length(avisos)) paste0(" | AVISOS: ", paste(avisos, collapse = " ")) else "")
