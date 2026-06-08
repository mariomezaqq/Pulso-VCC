# =============================================================================
# IMPORTAR MANUALES DESDE EL EXCEL VIEJO  (una sola vez, como base)
# Lee las hojas "Fondos Manuales" e "Indices Manuales" de un file_show.xlsx
# y las vuelca al formato largo data/manuales_vc.csv (Nombre,Tipo,Fecha,Valor).
# Uso:  Rscript scripts/importar_manuales_excel.R  [ruta_excel]
# =============================================================================
suppressMessages({ library(readxl); library(dplyr) })

args <- commandArgs(trailingOnly = TRUE)
EXCEL <- if (length(args) >= 1) args[1] else
  file.path(Sys.getenv("USERPROFILE"), "Desktop", "Carpeta Pulso Claude",
            "Pulso VCC Dashboard", "file_show.xlsx")
if (!file.exists(EXCEL)) stop("No existe el Excel: ", EXCEL)

source("R/fondos_pulso.R")  # NOMBRES_FONDOS_MANUALES / NOMBRES_INDICES_MANUALES

a_largo <- function(sheet, nombres, tipo) {
  if (!sheet %in% excel_sheets(EXCEL)) { message("Sin hoja: ", sheet); return(NULL) }
  df <- suppressMessages(read_excel(EXCEL, sheet = sheet))
  fechas <- as.Date(df[[1]])
  out <- do.call(rbind, lapply(nombres, function(nm) {
    if (!nm %in% colnames(df)) return(NULL)
    data.frame(Nombre = nm, Tipo = tipo, Fecha = as.character(fechas),
               Valor = suppressWarnings(as.numeric(df[[nm]])), stringsAsFactors = FALSE)
  }))
  out[!is.na(out$Valor) & !is.na(out$Fecha), , drop = FALSE]
}

largo <- rbind(
  a_largo("Fondos Manuales",  NOMBRES_FONDOS_MANUALES,  "Fondo"),
  a_largo("Indices Manuales", NOMBRES_INDICES_MANUALES, "Indice")
)
largo <- largo[order(largo$Tipo, largo$Nombre, largo$Fecha), ]

dir.create("data", showWarnings = FALSE)
write.csv(largo, "data/manuales_vc.csv", row.names = FALSE, fileEncoding = "UTF-8")
cat("Escrito data/manuales_vc.csv con", nrow(largo), "filas.\n")
print(table(largo$Nombre))
