# =============================================================================
# CALCULOS DE RENTABILIDAD
# Replica la logica del script original: rentabilidad = (VC_final / VC_ini) - 1
# El VC_ini depende del periodo elegido (WTD, MTD, YTD, 1M, 3M, 6M, 12M, custom)
#
# Si el historico tiene la columna `valor_cuota_ajustado` (poblada por
# dividendos.R::aplicar_dividendos_a_historico), las funciones agregan columnas
# "Aj." en la tabla de rentabilidades y permiten graficar la serie ajustada.
# =============================================================================

#' Devuelve la fecha de referencia para un periodo dado
fecha_inicio_periodo <- function(historico, fecha_final, periodo,
                                  fecha_inicio_custom = NULL) {
  if (is.null(historico) || nrow(historico) == 0) return(NA)
  if (identical(periodo, "MAX")) return(min(historico$fecha, na.rm = TRUE))

  target <- switch(periodo,
    "WTD"    = fecha_final - lubridate::wday(fecha_final, week_start = 1) + 1 - 7,
    "MTD"    = lubridate::floor_date(fecha_final, "month") - 1,
    "YTD"    = lubridate::floor_date(fecha_final, "year") - 1,
    "1M"     = fecha_final - 30,
    "3M"     = fecha_final - 91,
    "6M"     = fecha_final - 182,
    "12M"    = fecha_final - 365,
    "Custom" = fecha_inicio_custom,
    NA
  )
  if (is.na(target)) return(NA)

  candidatos <- historico %>%
    dplyr::filter(fecha <= target) %>%
    dplyr::arrange(dplyr::desc(fecha))
  if (nrow(candidatos) > 0) return(candidatos$fecha[1])
  historico$fecha[1]
}

#' Calcula la rentabilidad de un fondo en el periodo (sin ajustar)
calcular_rentabilidad <- function(historico, periodo,
                                  fecha_fin = NULL,
                                  fecha_inicio_custom = NULL,
                                  ajustado = FALSE) {
  col_vc <- if (ajustado && "valor_cuota_ajustado" %in% colnames(historico)) "valor_cuota_ajustado" else "valor_cuota"

  if (is.null(historico) || nrow(historico) < 1) {
    return(list(vc_inicio = NA, vc_fin = NA,
                fecha_inicio = NA, fecha_fin = NA, rent_pct = NA))
  }

  if (is.null(fecha_fin)) fecha_fin <- max(historico$fecha)
  fila_fin <- historico %>%
    dplyr::filter(fecha <= fecha_fin) %>%
    dplyr::arrange(dplyr::desc(fecha)) %>%
    dplyr::slice(1)

  if (nrow(fila_fin) == 0) return(list(vc_inicio=NA, vc_fin=NA, fecha_inicio=NA, fecha_fin=NA, rent_pct=NA))

  f_ini <- fecha_inicio_periodo(historico, fila_fin$fecha, periodo, fecha_inicio_custom)
  if (is.na(f_ini)) {
    return(list(vc_inicio=NA, vc_fin=fila_fin[[col_vc]],
                fecha_inicio=NA, fecha_fin=fila_fin$fecha, rent_pct=NA))
  }

  fila_ini <- historico %>% dplyr::filter(fecha == f_ini) %>% dplyr::slice(1)
  if (nrow(fila_ini) == 0) {
    return(list(vc_inicio=NA, vc_fin=fila_fin[[col_vc]],
                fecha_inicio=NA, fecha_fin=fila_fin$fecha, rent_pct=NA))
  }

  list(
    vc_inicio    = fila_ini[[col_vc]],
    vc_fin       = fila_fin[[col_vc]],
    fecha_inicio = fila_ini$fecha,
    fecha_fin    = fila_fin$fecha,
    rent_pct     = (fila_fin[[col_vc]] / fila_ini[[col_vc]]) - 1
  )
}

#' Normaliza una serie a base 100 desde una fecha
#' Si ajustado=TRUE y la columna valor_cuota_ajustado existe, normaliza esa.
normalizar_base_100 <- function(historico, fecha_base, ajustado = FALSE) {
  if (is.null(historico) || nrow(historico) == 0) return(NULL)
  col_vc <- if (ajustado && "valor_cuota_ajustado" %in% colnames(historico)) "valor_cuota_ajustado" else "valor_cuota"

  base <- historico %>%
    dplyr::filter(fecha <= fecha_base) %>%
    dplyr::arrange(dplyr::desc(fecha)) %>%
    dplyr::slice(1)

  if (nrow(base) == 0 || is.na(base[[col_vc]]) || base[[col_vc]] == 0) return(NULL)

  historico %>%
    dplyr::filter(fecha >= base$fecha) %>%
    dplyr::mutate(valor_normalizado = (.data[[col_vc]] / base[[col_vc]][1]) * 100)
}

#' Rango (desde, hasta) a consultar a la CMF
rango_consulta_cmf <- function(periodo, fecha_fin = Sys.Date(),
                                fecha_inicio_custom = NULL) {
  hasta <- fecha_fin
  desde_periodo <- switch(periodo,
    "WTD"    = hasta - 30,
    "MTD"    = lubridate::floor_date(hasta, "month") - 7,
    "YTD"    = lubridate::floor_date(hasta, "year") - 7,
    "1M"     = hasta - 45,
    "3M"     = hasta - 100,
    "6M"     = hasta - 190,
    "12M"    = hasta - 375,
    "Custom" = if (!is.null(fecha_inicio_custom)) fecha_inicio_custom - 7 else hasta - 30,
    hasta - 30
  )
  desde_anio <- as.Date(paste0(lubridate::year(hasta) - 1, "-12-24"))
  desde <- min(desde_periodo, desde_anio)
  list(desde = as.Date(desde), hasta = as.Date(hasta))
}

# =============================================================================
# TABLA MATRIZ DE RENTABILIDADES POR PERIODO
# =============================================================================

MESES_ES <- c("Enero","Febrero","Marzo","Abril","Mayo","Junio",
              "Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")

#' Devuelve el VC mas reciente con fecha <= fecha_ref (de la columna pedida)
get_vc_a_fecha <- function(historico, fecha_ref, col_vc = "valor_cuota") {
  if (is.null(historico) || nrow(historico) == 0 || is.na(fecha_ref)) return(NA_real_)
  if (!col_vc %in% colnames(historico)) col_vc <- "valor_cuota"
  fila <- historico %>%
    dplyr::filter(fecha <= fecha_ref) %>%
    dplyr::arrange(dplyr::desc(fecha)) %>%
    dplyr::slice(1)
  if (nrow(fila) == 0) return(NA_real_)
  fila[[col_vc]][1]
}

#' Calcula rentabilidad entre dos fechas
calc_rent <- function(historico, fecha_ini, fecha_fin, col_vc = "valor_cuota") {
  vi <- get_vc_a_fecha(historico, fecha_ini, col_vc)
  vf <- get_vc_a_fecha(historico, fecha_fin, col_vc)
  if (is.na(vi) || is.na(vf) || vi == 0) return(NA_real_)
  (vf / vi) - 1
}

#' Periodos estandar a mostrar en la tabla
periodos_estandar <- function(fecha_hoy = Sys.Date()) {
  yr <- lubridate::year(fecha_hoy)
  mes_actual <- lubridate::month(fecha_hoy)
  fin_mes_anterior <- lubridate::floor_date(fecha_hoy, "month") - 1
  hace_30d <- fecha_hoy - 30
  fin_anio_anterior <- as.Date(paste0(yr - 1, "-12-31"))

  ult_dia_mes <- function(year, mes)
    as.Date(lubridate::ceiling_date(as.Date(paste(year, mes, 1, sep = "-")), "month") - 1)
  primer_dia_mes_anterior <- function(year, mes)
    as.Date(paste(year, mes, 1, sep = "-")) - 1

  periodos <- list(
    list(etiqueta = "MTD", fecha_ini = fin_mes_anterior, fecha_fin = fecha_hoy),
    list(etiqueta = "30d", fecha_ini = hace_30d, fecha_fin = fecha_hoy)
  )
  for (k in 1:3) {
    mes_obj <- mes_actual - k; yr_obj <- yr
    if (mes_obj <= 0) { mes_obj <- mes_obj + 12; yr_obj <- yr_obj - 1 }
    fin_mes_obj <- ult_dia_mes(yr_obj, mes_obj)
    ini_mes_obj <- primer_dia_mes_anterior(yr_obj, mes_obj)
    etiqueta_mes <- if (yr_obj == yr) MESES_ES[mes_obj] else paste0(MESES_ES[mes_obj], " ", yr_obj)
    periodos[[length(periodos) + 1]] <- list(etiqueta = etiqueta_mes,
                                              fecha_ini = ini_mes_obj, fecha_fin = fin_mes_obj)
  }
  periodos[[length(periodos) + 1]] <- list(etiqueta = "YTD",
                                            fecha_ini = fin_anio_anterior, fecha_fin = fecha_hoy)
  periodos
}

#' Para un fondo: tabla de rentabilidades por periodo.
#' Si el historico tiene `valor_cuota_ajustado`, calcula AMBAS (sin/con ajuste)
#' y devuelve dos columnas por periodo: ej. "MTD" y "MTD aj.".
tabla_rentabilidades_fila <- function(historico, nombre_fondo, serie,
                                       tipo, fecha_hoy = Sys.Date()) {
  periodos <- periodos_estandar(fecha_hoy)
  etiquetas <- sapply(periodos, function(p) p$etiqueta)

  rents_sin <- sapply(periodos, function(p) calc_rent(historico, p$fecha_ini, p$fecha_fin, "valor_cuota"))

  tiene_ajuste <- !is.null(historico) &&
                  "valor_cuota_ajustado" %in% colnames(historico) &&
                  any(historico$valor_cuota_ajustado != historico$valor_cuota, na.rm = TRUE)

  ultimo_vc <- get_vc_a_fecha(historico, fecha_hoy, "valor_cuota")

  out <- tibble::tibble(
    Fondo       = nombre_fondo,
    Serie       = serie,
    Tipo        = tipo,
    `Ultimo VC` = ultimo_vc
  )

  if (tiene_ajuste) {
    rents_aj <- sapply(periodos, function(p) calc_rent(historico, p$fecha_ini, p$fecha_fin, "valor_cuota_ajustado"))
    div_total <- tail(historico$div_acum, 1)
    out[["Div Acum."]] <- if (length(div_total) > 0) div_total else 0
    for (i in seq_along(etiquetas)) {
      out[[etiquetas[i]]]            <- rents_sin[i]
      out[[paste0(etiquetas[i], " aj.")]] <- rents_aj[i]
    }
  } else {
    for (i in seq_along(etiquetas)) {
      out[[etiquetas[i]]] <- rents_sin[i]
    }
  }
  out
}

# =============================================================================
# METRICAS SELECCIONABLES (rentabilidades por periodo + volatilidad)
# Reemplaza la tabla fija: el usuario elige que columnas ver (multi-seleccion).
# =============================================================================

if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a

#' Catalogo de metricas disponibles para un dia dado.
#' key -> list(etiqueta, kind, ini, fin, ndias). kind: "rent" o "vol".
catalogo_metricas <- function(fecha_hoy = Sys.Date()) {
  yr  <- lubridate::year(fecha_hoy)
  mes <- lubridate::month(fecha_hoy)
  fin_mes_ant  <- lubridate::floor_date(fecha_hoy, "month") - 1
  fin_anio_ant <- as.Date(paste0(yr - 1, "-12-31"))
  ult_dia_mes <- function(y, m) as.Date(lubridate::ceiling_date(as.Date(sprintf("%d-%02d-01", y, m)), "month") - 1)
  ini_mes     <- function(y, m) as.Date(sprintf("%d-%02d-01", y, m)) - 1

  defs <- list()
  defs[["MTD"]] <- list(etiqueta = "MTD", kind = "rent", ini = fin_mes_ant,   fin = fecha_hoy)
  defs[["YTD"]] <- list(etiqueta = "YTD", kind = "rent", ini = fin_anio_ant,  fin = fecha_hoy)
  defs[["30d"]] <- list(etiqueta = "30d", kind = "rent", ini = fecha_hoy - 30,  fin = fecha_hoy)
  defs[["60d"]] <- list(etiqueta = "60d", kind = "rent", ini = fecha_hoy - 60,  fin = fecha_hoy)
  defs[["90d"]] <- list(etiqueta = "90d", kind = "rent", ini = fecha_hoy - 90,  fin = fecha_hoy)
  defs[["12M"]] <- list(etiqueta = "12M", kind = "rent", ini = fecha_hoy - 365, fin = fecha_hoy)
  for (k in 1:3) {                          # 3 meses calendario anteriores
    mo <- mes - k; y <- yr
    if (mo <= 0) { mo <- mo + 12; y <- y - 1 }
    lab <- if (y == yr) MESES_ES[mo] else paste0(MESES_ES[mo], " ", y)
    defs[[paste0("MES", k)]] <- list(etiqueta = lab, kind = "rent",
                                     ini = ini_mes(y, mo), fin = ult_dia_mes(y, mo))
  }
  # Volatilidad anualizada (sd retornos diarios x sqrt(252)) en 3 ventanas
  defs[["VOL30"]] <- list(etiqueta = "Vol 30d", kind = "vol", ndias = 30)
  defs[["VOL60"]] <- list(etiqueta = "Vol 60d", kind = "vol", ndias = 60)
  defs[["VOL90"]] <- list(etiqueta = "Vol 90d", kind = "vol", ndias = 90)
  defs
}

#' Volatilidad anualizada: sd(retornos diarios de los ultimos `ndias`) * sqrt(252)
calcular_volatilidad <- function(historico, ndias = 90, col_vc = "valor_cuota") {
  if (is.null(historico) || nrow(historico) < 6) return(NA_real_)
  if (!col_vc %in% colnames(historico)) col_vc <- "valor_cuota"
  h <- historico %>% dplyr::arrange(fecha)
  h <- h %>% dplyr::filter(fecha >= max(fecha) - ndias)
  v <- as.numeric(h[[col_vc]]); v <- v[is.finite(v) & v > 0]
  if (length(v) < 6) return(NA_real_)
  ret <- diff(v) / utils::head(v, -1)
  ret <- ret[is.finite(ret)]
  if (length(ret) < 5) return(NA_real_)
  stats::sd(ret) * sqrt(252)
}

#' Valor de UNA metrica (rentabilidad o volatilidad) sobre la columna col_vc
valor_metrica <- function(historico, def, col_vc = "valor_cuota") {
  if (def$kind == "vol") return(calcular_volatilidad(historico, def$ndias %||% 90, col_vc))
  calc_rent(historico, def$ini, def$fin, col_vc)
}

#' Dividendos / repartos acumulados (en $ por cuota) al ultimo dato disponible.
#' Para FI viene de los dividendos del Boletin; para FFMM del factor de reparto.
.dividendos_acum <- function(historico) {
  if (is.null(historico) || nrow(historico) == 0) return(NA_real_)
  if (!"div_acum" %in% colnames(historico)) return(0)
  da <- as.numeric(historico$div_acum); da <- da[is.finite(da)]
  if (length(da) == 0) return(0)
  utils::tail(da, 1)
}

#' Fecha (texto dd/mm/yyyy) del ultimo VC usado (<= fecha_ref). Sirve para
#' corroborar que el dato es el cierre esperado y detectar fondos desfasados.
.fecha_ultimo_vc <- function(historico, fecha_ref = Sys.Date()) {
  if (is.null(historico) || nrow(historico) == 0) return(NA_character_)
  fila <- historico %>% dplyr::filter(fecha <= fecha_ref) %>%
    dplyr::arrange(dplyr::desc(fecha)) %>% dplyr::slice(1)
  if (nrow(fila) == 0) return(NA_character_)
  format(fila$fecha[1], "%d/%m/%Y")
}

#' Fila de la tabla segun las metricas elegidas (multi-seleccion).
#' Una sola columna por metrica; ajustado=TRUE usa valor_cuota_ajustado si existe.
tabla_metricas_fila <- function(historico, nombre_fondo, serie, tipo,
                                metricas_sel, ajustado = TRUE,
                                fecha_hoy = Sys.Date()) {
  defs <- catalogo_metricas(fecha_hoy)
  if (is.null(metricas_sel) || length(metricas_sel) == 0) metricas_sel <- c("MTD", "YTD")
  metricas_sel <- metricas_sel[metricas_sel %in% names(defs)]

  col_vc <- if (ajustado && !is.null(historico) &&
                "valor_cuota_ajustado" %in% colnames(historico))
              "valor_cuota_ajustado" else "valor_cuota"

  out <- tibble::tibble(
    Fondo       = nombre_fondo,
    Serie       = serie,
    Tipo        = tipo,
    `Ultimo VC` = get_vc_a_fecha(historico, fecha_hoy, "valor_cuota")
  )
  for (k in metricas_sel) {
    out[[defs[[k]]$etiqueta]] <- valor_metrica(historico, defs[[k]], col_vc)
  }
  out
}
