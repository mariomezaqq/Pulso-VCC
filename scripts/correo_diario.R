# =============================================================================
# CORREO DIARIO DEL PULSO  (lo corre .github/workflows/correo.yml, o a mano)
# Genera en la carpeta de salida (default "correo/"):
#   asunto.txt, cuerpo.html (logo cid:), vista-previa.html (logo incrustado), cuerpo.txt
#   meta.json                             -> cierre, avisos (sin adjunto: el reporte es el link al Pulso)
# Usa los mismos datos y calculos que el dashboard (R/preparar.R). NO envia nada:
# el envio lo hace el workflow con estos archivos.
# Correr desde la raiz:  Rscript scripts/correo_diario.R [carpeta_salida]
# =============================================================================
suppressMessages({ library(dplyr); library(lubridate); library(tibble); library(openxlsx)
                   library(jsonlite); library(readxl); library(httr) })
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
source("R/github_store.R")   # solo por store_path / %||% que usan los demas (no escribe nada)
for (f in c("fondos_pulso", "calculos", "dividendos", "ajustes", "compute", "preparar", "correo"))
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
# Aviso de primer envio: si existe correo_assets/primer_envio.txt (parrafos separados por una linea en blanco) se
# antepone al reporte. El workflow lo borra solo tras el primer envio real al equipo.
if (file.exists("correo_assets/primer_envio.txt")) {
  ip <- trimws(strsplit(paste(readLines("correo_assets/primer_envio.txt", encoding = "UTF-8", warn = FALSE), collapse = "\n"), "\n\\s*\n")[[1]])
  txt$intro <- ip[nzchar(ip)]
}
# Firma: "saludo \n nombre \n cargo". Se puede cambiar con la variable CORREO_FIRMA (usar \n).
firma <- gsub("\\n", "\n", Sys.getenv("CORREO_FIRMA", "Un cordial saludo,\nMario Meza\nAnalista\nVizcaya Capital"), fixed = TRUE)
cuerpo <- cuerpo_correo(txt, firma = firma, logo_src = "cid:logo-pulso")          # para enviar (logo adjunto en linea)
logo_b64 <- jsonlite::base64_enc(readBin("correo_assets/logo-vizcaya-capital-10-blanco.png", "raw", file.info("correo_assets/logo-vizcaya-capital-10-blanco.png")$size))
vista <- cuerpo_correo(txt, firma = firma, logo_src = paste0("data:image/png;base64,", logo_b64))   # para abrirlo en el navegador
fc <- txt$fecha_cierre


writeLines(txt$asunto, file.path(out, "asunto.txt"), useBytes = TRUE)
writeLines(enc2utf8(cuerpo$html), file.path(out, "cuerpo.html"), useBytes = TRUE)
writeLines(enc2utf8(vista$html), file.path(out, "vista-previa.html"), useBytes = TRUE)
file.copy("correo_assets/logo-vizcaya-capital-10-blanco.png", file.path(out, "logo-pulso.png"), overwrite = TRUE)
writeLines(enc2utf8(cuerpo$texto), file.path(out, "cuerpo.txt"), useBytes = TRUE)
avisos <- c(t$pd$alertas$global %||% character(0),
            if (length(txt$parrafos) < 4) "Falta algun parrafo por datos faltantes (indices, Treasury o instrumentos de la casa).")
write_json(list(cierre = format(fc), asunto = txt$asunto,
                atrasado = !is.null(t$pd$alertas$global),   # cierre anterior al ultimo dia habil
                fondos_atrasados = vapply(t$pd$alertas$fondos, function(x) paste0(x$nombre, " (", x$fecha, ")"), ""),
                datos_generados = t$datos$generado %||% "", avisos = avisos),
           file.path(out, "meta.json"), auto_unbox = TRUE, pretty = TRUE)
message("Correo generado en ", out, "/ | ", txt$asunto, if (length(avisos)) paste0(" | AVISOS: ", paste(avisos, collapse = " ")) else "")
