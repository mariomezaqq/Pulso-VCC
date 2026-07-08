# =============================================================================
# PARSERS DEL FOLLETO INFORMATIVO (CMF pestania 68) -> datos para el dashboard
# Extrae del texto del PDF del folleto:
#   - Rentabilidad "Tot. Anual" (%) por anio calendario (tabla de rentabilidad).
#   - Remuneracion Maxima Anual (%) = comision fija de gestion (el "TAC" que
#     usa el pulso; NO el "TAC serie" que se infla con la comision de exito).
# El folleto NO requiere reCAPTCHA (a diferencia del valor cuota) -> se puede
# bajar sin el token CMF. Requiere R/folletos.R (obtener_folleto_fondo).
# =============================================================================
if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

.folleto_num <- function(x) suppressWarnings(as.numeric(gsub(",", ".", x)))

# Formatea un numero (%) al estilo del dashboard: 68.07 -> "68,07%"
fmt_pct_cl <- function(x, dec = 2) {
  if (is.null(x) || length(x) != 1 || is.na(x)) return("")
  paste0(formatC(x, format = "f", digits = dec, decimal.mark = ","), "%")
}

# Rentabilidad "Tot. Anual" (%) del anio. Filas del folleto:
#   "Serie <X> <ANIO>  ene feb ... dic  Total  Tot.Anual  Volatilidad"
# Se extraen los numeros d,dd (con signo) y se toma el 2do desde el final.
folleto_rent_anual <- function(texto, anio) {
  ls <- unlist(strsplit(texto %||% "", "\n"))
  cand <- ls[grepl(paste0("(?i)\\bserie\\b.*\\b", anio, "\\b"), ls, perl = TRUE)]
  for (ln in cand) {
    nums <- regmatches(ln, gregexpr("-?\\d+,\\d+", ln, perl = TRUE))[[1]]
    if (length(nums) >= 3) return(.folleto_num(nums[length(nums) - 1]))
  }
  NA_real_
}

# Remuneracion Maxima Anual (%) de una serie. El folleto la trae de dos formas:
#   - RiskAmerica: un solo valor bajo "Remunerac. Máxima Anual (%)" (fondo/serie unica).
#   - LVA: una FILA multi-serie ("Remunerac. Máxima  1,4300  0,3000  0,9500 ...")
#     donde las columnas siguen el orden de series de la pestania 68.
# @param serie serie del fondo; @param series_orden vector de series en orden de
#   columnas (de folleto_series_disponibles). Si hay 1 solo numero, se usa ese.
folleto_remun_maxima <- function(texto, serie = NULL, series_orden = NULL) {
  ls <- unlist(strsplit(texto %||% "", "\n"))
  i <- grep("(?i)remunerac.*m.xima", ls, perl = TRUE)
  if (!length(i)) return(NA_real_)
  # junta los numeros de la 1a linea (desde la etiqueta) que tenga alguno
  nums <- character()
  for (j in i[1]:min(i[1] + 4, length(ls))) {
    nums <- regmatches(ls[j], gregexpr("\\d+,\\d+", ls[j], perl = TRUE))[[1]]
    if (length(nums)) break
  }
  if (!length(nums)) return(NA_real_)
  if (length(nums) == 1) return(.folleto_num(nums[1]))
  # multi-serie: mapear por la posicion de la serie en el orden de columnas
  if (!is.null(serie) && !is.null(series_orden)) {
    k <- match(toupper(trimws(serie)), toupper(trimws(series_orden)))
    if (!is.na(k) && k <= length(nums)) return(.folleto_num(nums[k]))
  }
  .folleto_num(nums[1])   # fallback: primera columna
}

#' Baja el folleto de un fondo y devuelve rent por anio + TAC (remuneracion max).
#' @param fondo list(run, serie, tipoentidad, row)
#' @param anios years a extraer (default 2024, 2025)
#' @return list(rent = named vector por anio (%), tac = remuneracion max (%),
#'              moneda, duracion, ok, motivo) — valores NA si no se hallaron.
datos_folleto_fondo <- function(fondo, anios = c(2024, 2025), dir = "data/folletos",
                                cookies = "", reusar = TRUE) {
  camp <- tryCatch(obtener_folleto_fondo(fondo, dir = dir, cookies = cookies, reusar = reusar),
                   error = function(e) { message("[folleto] err: ", e$message); NULL })
  # obtener_folleto_fondo devuelve campos ya parseados; para rent necesitamos el
  # texto crudo -> re-leemos el PDF cacheado.
  dest <- file.path(dir, paste0(fondo$run, "_", gsub("[^A-Za-z0-9]", "", fondo$serie), ".pdf"))
  if (!file.exists(dest)) return(list(ok = FALSE, motivo = "sin folleto", rent = setNames(rep(NA_real_, length(anios)), anios), tac = NA_real_))
  txt <- tryCatch(paste(pdftools::pdf_text(dest), collapse = "\n"), error = function(e) NULL)
  if (is.null(txt)) return(list(ok = FALSE, motivo = "pdf ilegible", rent = setNames(rep(NA_real_, length(anios)), anios), tac = NA_real_))
  rent <- setNames(vapply(anios, function(a) folleto_rent_anual(txt, a), numeric(1)), as.character(anios))
  # Para el TAC multi-serie (LVA) necesitamos el orden de columnas = orden de
  # series en la pestania 68.
  orden <- tryCatch(folleto_series_disponibles(fondo$run, fondo$tipoentidad, fondo$row %||% "", cookies)$serie,
                    error = function(e) NULL)
  list(ok = TRUE, motivo = NA_character_,
       rent = rent, tac = folleto_remun_maxima(txt, fondo$serie, orden),
       moneda = camp$moneda %||% NA_character_, duracion = camp$duracion %||% NA_character_)
}
