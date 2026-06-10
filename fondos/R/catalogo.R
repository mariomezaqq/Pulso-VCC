# =============================================================================
# CATALOGO DE FONDOS
# Lee data/catalogo_fondos.csv (generado a partir del Excel de la CMF).
# Cubre TODOS los fondos vigentes: FIRES + FINRE + RGFMU (~1000 fondos).
#
# Estructura del CSV:
#   nombre, run, serie, tipoentidad, row, categoria, instrumento
# =============================================================================

# Usa el catalogo LIMPIO (solo series que la CMF realmente devuelve) si existe;
# si no, cae al catalogo completo. El limpio se genera con los scripts de
# validacion (limpiar_catalogo + validar_series_rgfmu).
.ruta_catalogo <- if (file.exists("data/catalogo_fondos_limpio.csv"))
  "data/catalogo_fondos_limpio.csv" else "data/catalogo_fondos.csv"
message("Catalogo fuente: ", .ruta_catalogo)

CATALOGO_FONDOS <- tibble::as_tibble(
  read.csv(.ruta_catalogo,
           stringsAsFactors = FALSE,
           fileEncoding     = "UTF-8",
           colClasses       = c(run = "character", serie = "character"))
)

# Tratar NA / strings vacios en row de forma uniforme
CATALOGO_FONDOS$row[is.na(CATALOGO_FONDOS$row)] <- ""

# Etiqueta para mostrar en el buscador (debe ser unica por fila)
CATALOGO_FONDOS$etiqueta <- paste0(
  CATALOGO_FONDOS$nombre,
  " - Serie ", CATALOGO_FONDOS$serie,
  " [", CATALOGO_FONDOS$tipoentidad, "]"
)

# ID unico para cache (run + serie + tipoentidad)
CATALOGO_FONDOS$id <- paste0(
  CATALOGO_FONDOS$run, "_",
  CATALOGO_FONDOS$serie, "_",
  CATALOGO_FONDOS$tipoentidad
)

message(sprintf("Catalogo cargado: %d series (%d fondos unicos)",
                nrow(CATALOGO_FONDOS),
                length(unique(CATALOGO_FONDOS$run))))
