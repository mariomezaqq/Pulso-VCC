# =============================================================================
# NUMERO DE CUOTAS DEL FONDO A UNA FECHA (desde la CMF)
# Para repartos que el Boletin informa como MONTO TOTAL (no por cuota), hay que
# dividir el total por el numero de cuotas del fondo a la FECHA LIMITE.
#
# La tabla de valor cuota de la CMF (pestania=7) trae, por serie y por fecha:
#   "Valor Libro" (= valor cuota)  y  "Patrimonio Neto".
# => cuotas_serie(fecha) = Patrimonio Neto / Valor Libro
#    cuotas_fondo(fecha)  = suma sobre todas las series
# Se toma la fecha disponible mas cercana <= fecha_limite.
#
# Requiere R/scraper.R (obtener_datos_cmf) cargado.
# =============================================================================
suppressMessages({ library(dplyr); library(stringr); library(lubridate); library(tibble) })

if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# Parser de numero en formato chileno: "176.015.805.599" -> 176015805599 ;
# "1.719,846" -> 1719.846
.num_cl <- function(x) suppressWarnings(as.numeric(
  gsub(",", ".", gsub("\\.", "", trimws(as.character(x))))))

#' Numero TOTAL de cuotas de un fondo a una fecha (cuotas = Pat.Neto / Valor cuota).
#'
#' @param run,tipoentidad,row  identificadores CMF del fondo
#' @param fecha_limite Date  fecha a la que se quieren las cuotas
#' @param token reCAPTCHA (necesario para FIRES/FINRE), cookies (FFMM)
#' @return list(total_cuotas, fecha_usada, detalle) o NULL si no se pudo
cuotas_fondo_a_fecha <- function(run, tipoentidad, row, fecha_limite,
                                 token = "", cookies = "") {
  fecha_limite <- as.Date(fecha_limite)
  fondo <- list(nombre = paste0("run=", run), run = run, serie = "A",
                row = row %||% "", tipoentidad = tipoentidad)
  df <- tryCatch(obtener_datos_cmf(fondo, fecha_limite - 150, fecha_limite, token, cookies),
                 error = function(e) { message("[cuotas] err scrape: ", e$message); NULL })
  if (is.null(df)) return(NULL)

  cn <- colnames(df)
  ci <- function(p) { w <- which(grepl(p, cn, ignore.case = TRUE)); if (length(w)) w[1] else NA_integer_ }
  c_fecha <- ci("fecha"); c_serie <- ci("serie")
  c_pat   <- ci("patrimonio")
  c_vl    <- ci("valor libro"); if (is.na(c_vl)) c_vl <- ci("valor.*econ"); if (is.na(c_vl)) c_vl <- ci("valor cuota")
  if (any(is.na(c(c_fecha, c_pat, c_vl)))) {
    message("[cuotas] no se hallaron columnas Patrimonio/Valor cuota. Cols: ", paste(cn, collapse = " | "))
    return(NULL)
  }

  d <- tibble(
    fecha      = lubridate::dmy(as.character(df[[c_fecha]])),
    serie      = if (!is.na(c_serie)) trimws(as.character(df[[c_serie]])) else "UNICA",
    patrimonio = .num_cl(df[[c_pat]]),
    valor      = .num_cl(df[[c_vl]])
  ) %>% filter(!is.na(fecha), !is.na(patrimonio), !is.na(valor), valor > 0)

  # EXACTAMENTE el dia limite (es el ex-date: el VC cae ese dia). Ni antes ni
  # despues. Si el limite es futuro o no habil (sin VC publicado), queda pendiente.
  snap <- d %>% filter(fecha == fecha_limite) %>%
    distinct(serie, .keep_all = TRUE) %>%
    mutate(cuotas = patrimonio / valor)

  if (nrow(snap) == 0) {
    return(list(total_cuotas = NA_real_, fecha_usada = as.Date(NA), disponible = FALSE,
                motivo = "sin VC publicado en la fecha limite (futura o no habil)",
                detalle = NULL))
  }
  list(total_cuotas = sum(snap$cuotas, na.rm = TRUE),
       fecha_usada  = fecha_limite, disponible = TRUE, motivo = NA_character_,
       detalle      = snap %>% select(serie, patrimonio, valor, cuotas))
}
