# Lista los fondos (run-serie) cuyo ultimo valor cuota es anterior al dia habil previo a hoy (Chile),
# o que aun no tienen serie: los que "a las 11 no tienen el NAV". Escribe data/faltantes_nav.txt:
#   linea 1 = fecha de hoy (Chile, YYYY-MM-DD); siguientes = run-serie faltantes (vacio = nada falta).
# Lo usa refresh.yml: los reintentos de las 11:30, 12:00, 12:30 y 13:00 corren solo para esos fondos.
# Correr desde la raiz:  Rscript scripts/faltantes_nav.R
suppressMessages(source("R/fondos_pulso.R"))
hoy <- as.Date(format(Sys.time(), tz = "America/Santiago"))
esperado <- hoy - 1
while (as.integer(format(esperado, "%u")) > 5) esperado <- esperado - 1   # dia habil anterior (lun-vie)

prev <- tryCatch(readRDS("data/series_vc.rds"), error = function(e) NULL)
series <- if (!is.null(prev) && !is.null(prev$series)) series_por_nombre(prev$series) else list()
faltan <- character()
for (fd in FONDOS) {
  h <- series[[fd$nombre]]
  ult <- if (!is.null(h) && nrow(h) > 0) max(as.Date(h$fecha)) else as.Date(NA)
  if (is.na(ult) || ult < esperado) faltan <- c(faltan, clave_fondo(fd))
}
writeLines(c(format(hoy), faltan), "data/faltantes_nav.txt")
message("NAV esperado: ", format(esperado), " | faltan ", length(faltan), if (length(faltan)) paste0(": ", paste(faltan, collapse = ", ")) else "")
