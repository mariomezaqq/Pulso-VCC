# =============================================================================
# AJUSTES DE RENTABILIDAD: dividendos (FIRES/FINRE) + factor de reparto (RGFMU)
# Produce valor_cuota_ajustado en cada serie; compute.R lo usa automaticamente.
#
# - Dividendos: del Boletin Bolsa (hoja "Dividendos CFI nacionales"), cruzados
#   por TICKER SEBRA via MAPEO_SEBRA (mismo metodo que el Excel del Comite),
#   + correcciones manuales (dividendos_overrides.csv).
# - Factor de reparto: viene en la propia serie scrapeada (RGFMU).
# =============================================================================
suppressMessages({ library(readxl); library(dplyr); library(lubridate); library(tibble) })

# Mapas script -> nombre_excel y script -> tipoentidad. Se construyen en tiempo
# de ejecucion (no al cargar) para no depender del orden de source().
.mapas_fondos <- function() {
  me <- list(); for (cat in CATEGORIAS) for (f in cat$fondos) me[[f$nombre_script]] <- f$nombre_excel
  td <- list(); for (fd in FONDOS) td[[fd$nombre]] <- if (!is.null(fd$tipoentidad)) fd$tipoentidad else "FIRES"
  list(excel = me, tipo = td)
}

.parse_fecha_div <- function(x) {
  x <- trimws(as.character(x)); out <- as.Date(rep(NA_real_, length(x)), origin = "1970-01-01")
  num <- suppressWarnings(as.numeric(x)); ser <- !is.na(num) & num > 30000 & num < 80000
  out[ser] <- as.Date(num[ser], origin = "1899-12-30")
  # OJO: los formatos día-primero van ANTES que los año-primero. Si no, "%Y-%m-%d"
  # consume parcialmente un "13-06-2026" y lo parsea mal como 0013-06-20.
  for (fmt in c("%d/%m/%Y","%d-%m-%Y","%Y-%m-%d","%Y/%m/%d","%m/%d/%Y")) {
    f <- is.na(out) & nzchar(x) & x != "NA"; if (!any(f)) break
    cand <- suppressWarnings(as.Date(x[f], format = fmt)); out[which(f)[!is.na(cand)]] <- cand[!is.na(cand)]
  }
  out
}
.parse_monto_div <- function(x) {
  x <- gsub("\\s", "", as.character(x)); tc <- grepl(",", x, fixed = TRUE)
  x[tc] <- gsub("\\.", "", x[tc]); x[tc] <- gsub(",", ".", x[tc], fixed = TRUE)
  suppressWarnings(as.numeric(x))
}

# Lookup (nombre_excel, fecha_limite) -> monto_final desde dividendos_calculados.csv.
# Resuelve el caso "monto total" (reparto SIN monto por-cuota en el boletin), que
# procesar_dividendos.R ya calculo como total/cuotas del dia limite. Mapea las filas
# calculadas (run+serie) al nombre_excel del pulso via la lista curada (solo FIRES/
# FINRE; los RGFMU se ajustan por factor de reparto de la serie, no por el boletin).
.calc_lookup_div <- function() {
  ruta <- tryCatch(store_sync("dividendos_calculados.csv"), error = function(e) NULL)
  if (is.null(ruta) || !file.exists(ruta))
    ruta <- if (file.exists("data/dividendos_calculados.csv")) "data/dividendos_calculados.csv" else NULL
  if (is.null(ruta)) return(list())
  df <- tryCatch(read.csv(ruta, stringsAsFactors = FALSE, fileEncoding = "UTF-8", check.names = FALSE),
                 error = function(e) NULL)
  if (is.null(df) || !nrow(df) || !all(c("run","serie","fecha_limite","monto_final") %in% names(df)))
    return(list())
  ex <- list(); for (cat in CATEGORIAS) for (f in cat$fondos) ex[[f$nombre_script]] <- f$nombre_excel
  rs <- list()
  for (fd in FONDOS) {
    tipo <- if (!is.null(fd$tipoentidad)) fd$tipoentidad else "FIRES"
    if (identical(tipo, "RGFMU")) next
    run <- as.character(fd$run %||% ""); if (!nzchar(run)) next
    rs[[paste0(run, "|", as.character(fd$serie %||% ""))]] <- ex[[fd$nombre]] %||% fd$nombre
  }
  out <- list()
  for (i in seq_len(nrow(df))) {
    nm <- rs[[paste0(as.character(df$run[i]), "|", as.character(df$serie[i]))]]
    if (is.null(nm)) next
    fl <- suppressWarnings(as.Date(as.character(df$fecha_limite[i])))
    mo <- suppressWarnings(as.numeric(df$monto_final[i]))
    if (is.na(fl) || is.na(mo) || mo <= 0) next
    out[[paste0(nm, "|", as.character(fl))]] <- mo
  }
  out
}

# Extrae dividendos de UNA hoja del boletin, cruzando por TICKER SEBRA (columna
# "Pesos"/"Dolares"). Toma el monto "Por Accion / cuota"; si viene vacio (reparto
# como monto total), lo rescata de dividendos_calculados.csv (arg calc).
.leer_hoja_div <- function(ruta_xlsx, hoja, skip, lookup, fecha_inicio, calc) {
  if (!hoja %in% tryCatch(excel_sheets(ruta_xlsx), error = function(e) character(0))) return(list())
  df <- tryCatch(suppressMessages(read_excel(ruta_xlsx, sheet = hoja, skip = skip,
                                             col_names = TRUE, col_types = "text")),
                 error = function(e) NULL)
  if (is.null(df) || nrow(df) == 0) return(list())
  cols <- tolower(colnames(df))
  ic <- which(grepl("pesos", cols))[1];   if (is.na(ic)) ic <- 3L
  iu <- which(grepl("d.lar", cols))[1];   if (is.na(iu)) iu <- 4L
  im <- which(grepl("por acci|cuota", cols))[1]; if (is.na(im)) im <- 9L
  il <- which(grepl("l.mi", cols))[1];    if (is.na(il)) il <- 11L
  res <- list()
  for (i in seq_len(nrow(df))) {
    nc <- toupper(trimws(as.character(df[[ic]][i]))); nu <- toupper(trimws(as.character(df[[iu]][i])))
    nombre <- if (nzchar(nc) && !is.null(lookup[[nc]])) lookup[[nc]]
              else if (nzchar(nu) && !is.null(lookup[[nu]])) lookup[[nu]] else NULL
    if (is.null(nombre)) next
    fl <- .parse_fecha_div(df[[il]][i]); if (is.na(fl) || fl < fecha_inicio) next
    mo <- .parse_monto_div(df[[im]][i])
    if (is.na(mo) || mo <= 0) {                       # sin por-cuota -> usar el calculado
      mo <- calc[[paste0(nombre, "|", as.character(fl))]]
      if (is.null(mo) || is.na(mo) || mo <= 0) next
    }
    res[[nombre]] <- rbind(res[[nombre]], data.frame(fecha_limite = fl, monto = mo))
  }
  res
}

#' Carga dividendos del Boletin y los devuelve por nombre_excel. Lee AMBAS hojas
#' relevantes ("Dividendos CFI nacionales" + "Repartos CFI-CFM"); los repartos que
#' vienen solo como monto total se resuelven con dividendos_calculados.csv.
#' @return named list nombre_excel -> tibble(fecha_limite, monto)
cargar_dividendos_pulso <- function(ruta_xlsx, fecha_inicio = as.Date("2025-12-31")) {
  if (is.null(ruta_xlsx) || !file.exists(ruta_xlsx)) return(list())
  lookup <- list()
  for (m in MAPEO_SEBRA) { t <- m$ticker_sebra
    if (!is.null(t) && !is.na(t) && nzchar(t)) lookup[[toupper(trimws(t))]] <- m$nombre_excel }
  if (!length(lookup)) return(list())
  calc <- tryCatch(.calc_lookup_div(), error = function(e) list())

  res <- list()
  for (hs in list(list("Dividendos CFI nacionales", 11L), list("Repartos CFI-CFM", 10L))) {
    parcial <- .leer_hoja_div(ruta_xlsx, hs[[1]], hs[[2]], lookup, fecha_inicio, calc)
    for (nm in names(parcial)) res[[nm]] <- rbind(res[[nm]], parcial[[nm]])
  }
  lapply(res, function(d) distinct(arrange(d, fecha_limite), fecha_limite, .keep_all = TRUE))
}

#' Mezcla correcciones manuales (data.frame Fondo, `Fecha limite`, Monto) en la lista.
fusionar_overrides <- function(div_por_fondo, ov_df) {
  if (is.null(ov_df) || !nrow(ov_df)) return(div_por_fondo)
  # La columna de fecha puede llegar como "Fecha limite" o "Fecha.limite"
  # (read.csv con check.names=TRUE convierte el espacio en punto).
  col_fl <- if (!is.null(ov_df[["Fecha limite"]])) ov_df[["Fecha limite"]] else ov_df[["Fecha.limite"]]
  for (i in seq_len(nrow(ov_df))) {
    nm <- trimws(as.character(ov_df$Fondo[i])); if (length(nm) != 1 || !nzchar(nm)) next
    fl <- .parse_fecha_div(if (is.null(col_fl)) NA else col_fl[i])
    mo <- suppressWarnings(as.numeric(ov_df$Monto[i]))
    if (length(fl) != 1 || is.na(fl) || length(mo) != 1 || is.na(mo) || mo <= 0) next
    nuevo <- rbind(div_por_fondo[[nm]], data.frame(fecha_limite = fl, monto = mo))
    div_por_fondo[[nm]] <- distinct(arrange(nuevo, fecha_limite), fecha_limite, .keep_all = TRUE)
  }
  div_por_fondo
}

#' Agrega valor_cuota_ajustado a cada serie: RGFMU por factor de reparto,
#' FIRES/FINRE por dividendos aditivos.
enriquecer_series_ajustadas <- function(series, div_por_fondo = list()) {
  mp <- .mapas_fondos()
  for (nm in names(series)) {
    h <- series[[nm]]; if (is.null(h) || !nrow(h)) next
    tipo <- if (!is.null(mp$tipo[[nm]])) mp$tipo[[nm]] else "FIRES"
    if (identical(tipo, "RGFMU")) {
      series[[nm]] <- aplicar_factor_reparto(h)
    } else {
      excel <- if (!is.null(mp$excel[[nm]])) mp$excel[[nm]] else nm
      ev <- div_por_fondo[[excel]]
      ev_t <- if (!is.null(ev)) tibble(fecha_limite = as.Date(ev$fecha_limite),
                                       monto = as.numeric(ev$monto), moneda = NA_character_) else NULL
      series[[nm]] <- aplicar_dividendos_a_historico(h, ev_t, moneda_vc = "$")
    }
  }
  series
}
