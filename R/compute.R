# =============================================================================
# COMPUTE: series VC crudas + metadata -> estructura del dashboard
# MTD / YTD / "Mes anterior" se calculan del valor cuota; el resto
# (2025, 2024, duracion, liquidez, moneda, TAC) es metadata estatica.
#
# NOTA: la fidelidad de dividendos / factor de reparto / factor de ajuste se
# aplica via R/dividendos.R sobre las series antes de llamar aqui (columna
# valor_cuota_ajustado). Si no existe, se usa valor_cuota crudo.
# =============================================================================

suppressMessages({ library(dplyr); library(lubridate) })

MESES_ES <- c("Enero","Febrero","Marzo","Abril","Mayo","Junio",
              "Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")

# VC mas reciente con fecha <= ref (col elegible: ajustado si existe)
.vc_a_fecha <- function(h, ref, col = NULL) {
  if (is.null(h) || nrow(h) == 0 || is.na(ref)) return(NA_real_)
  if (is.null(col)) col <- if ("valor_cuota_ajustado" %in% names(h)) "valor_cuota_ajustado" else "valor_cuota"
  fila <- h %>% filter(fecha <= ref) %>% arrange(desc(fecha)) %>% slice(1)
  if (nrow(fila) == 0) NA_real_ else fila[[col]][1]
}

.rent <- function(h, fi, ff, col = NULL) {
  vi <- .vc_a_fecha(h, fi, col); vf <- .vc_a_fecha(h, ff, col)
  if (is.na(vi) || is.na(vf) || vi == 0) return(NA_real_)
  (vf / vi) - 1
}

#' Construye la estructura de datos del dashboard.
#' @param series named list: nombre_script -> tibble(fecha, valor_cuota[, valor_cuota_ajustado])
#' @param fecha_cierre Date del cierre (ultimo dato comun)
#' @param macro list de list(nombre, wtd, mtd, ytd)  (precalculado)
#' @param rf_usa list(headers=list(cierre,fecha1,fecha2), rows=list(list(nombre,cierre2025,abr20,abr27)))
#' @param manuales named list opcional: nombre_excel -> list(mtd=, ytd=, mes_ant=) (fracciones)
# Fecha de cierre "del grupo": la fecha mas frecuente entre los ultimos datos
# de los fondos (robusta ante un outlier de fin de semana). Fallback al maximo.
.moda_fecha <- function(fechas) {
  if (is.numeric(fechas)) fechas <- as.Date(fechas, origin = "1970-01-01")
  fechas <- fechas[!is.na(fechas)]
  if (!length(fechas)) return(NA)
  tt <- table(as.character(fechas))
  cand <- names(tt)[tt == max(tt)]
  as.Date(max(cand))   # si hay empate, la mas reciente entre las mas frecuentes
}

# Dia habil anterior (lun-vie; no considera feriados) respecto a 'ref'
.dia_habil_anterior <- function(ref = Sys.Date()) {
  d <- as.Date(ref) - 1
  while (lubridate::wday(d) %in% c(1, 7)) d <- d - 1   # 1=domingo, 7=sabado
  d
}

#' @param fecha_cierre Date o NULL. Si NULL se infiere de las series (moda).
#' @param series_manual named list nombre_excel -> tibble(fecha, valor_cuota) para
#'        fondos manuales (VC ingresado a mano). Se calculan igual que los demas.
construir_pulso_data <- function(series, fecha_cierre = NULL, macro = list(), rf_usa = NULL,
                                 series_manual = NULL) {
  # Fecha de cada fondo (ultimo dato) para detectar atrasos
  fechas_dato <- list()
  for (nm in names(series)) {
    h <- series[[nm]]
    if (!is.null(h) && nrow(h) > 0) fechas_dato[[nm]] <- max(h$fecha)
  }
  fechas_vec <- if (length(fechas_dato)) do.call(c, unname(fechas_dato)) else as.Date(character())
  fc <- if (!is.null(fecha_cierre)) as.Date(fecha_cierre) else .moda_fecha(fechas_vec)
  if (is.na(fc)) fc <- Sys.Date() - 1
  yr  <- year(fc)
  fin_mes_ant  <- floor_date(fc, "month") - 1          # ultimo dia mes anterior (base MTD y fin "mes ant")
  ini_mes_ant  <- floor_date(fin_mes_ant, "month") - 1 # dia previo al 1ro del mes anterior (base "mes ant")
  fin_anio_ant <- as.Date(sprintf("%d-12-31", yr - 1))
  mes_ant_label <- MESES_ES[month(fin_mes_ant)]

  cats_out <- list()
  for (cat in CATEGORIAS) {
    fondos_out <- list()
    for (f in cat$fondos) {
      nombre_excel  <- f$nombre_excel
      nombre_script <- f$nombre_script
      meta <- DATOS_FONDO[[nombre_excel]]
      if (is.null(meta)) meta <- list(rent2025="", rent2024="", duracion="", liquidez="", moneda="", tac="")

      mtd <- NA_real_; ytd <- NA_real_; mes_ant <- NA_real_
      fecha_dato <- NA; atrasado <- FALSE

      es_manual <- identical(nombre_script, "__MANUAL__")
      h <- if (es_manual) {
        if (!is.null(series_manual)) series_manual[[nombre_excel]] else NULL
      } else series[[nombre_script]]

      {
        if (!is.null(h) && nrow(h) > 0) {
          fecha_dato <- max(h$fecha)
          atrasado   <- fecha_dato < fc
          # Los fondos MANUALES muestran su propio ultimo VC (el usuario lo ingresa
          # a mano): si su dato es mas nuevo que el cierre del grupo scrapeado, se
          # ancla a SU fecha en vez de recortarse a fc. Los scrapeados usan fc igual
          # que antes (ref_fin == fc -> comportamiento identico).
          ref_fin <- if (es_manual && fecha_dato > fc) fecha_dato else fc
          f_fin_mes_ant  <- floor_date(ref_fin, "month") - 1
          f_ini_mes_ant  <- floor_date(f_fin_mes_ant, "month") - 1
          f_fin_anio_ant <- as.Date(sprintf("%d-12-31", year(ref_fin) - 1))
          mtd     <- .rent(h, f_fin_mes_ant, ref_fin)
          mes_ant <- .rent(h, f_ini_mes_ant, f_fin_mes_ant)
          if (nombre_excel %in% FONDOS_YTD_DESDE_INICIO) {
            col <- if ("valor_cuota_ajustado" %in% names(h)) "valor_cuota_ajustado" else "valor_cuota"
            v0 <- h %>% arrange(fecha) %>% slice(1)
            vf <- .vc_a_fecha(h, ref_fin, col)
            ytd <- if (nrow(v0) && !is.na(vf) && v0[[col]][1] != 0) (vf / v0[[col]][1]) - 1 else NA_real_
          } else {
            ytd <- .rent(h, f_fin_anio_ant, ref_fin)
          }
        }
      }

      fondos_out[[length(fondos_out) + 1]] <- list(
        nombre   = nombre_excel,
        mtd      = mtd, ytd = ytd, mes_ant = mes_ant,
        rent2025 = meta$rent2025 %||% "", rent2024 = meta$rent2024 %||% "",
        duracion = meta$duracion %||% "", liquidez = meta$liquidez %||% "",
        moneda   = meta$moneda   %||% "", tac      = meta$tac      %||% "",
        fecha_dato = if (!is.na(fecha_dato)) format(fecha_dato, "%d/%m") else NA_character_,
        atrasado   = atrasado
      )
    }
    cats_out[[length(cats_out) + 1]] <- list(nombre = cat$titulo, fondos = fondos_out)
  }

  # ---- Alertas de datos desactualizados ----
  atrasados <- list()
  for (nm in names(fechas_dato)) {
    if (fechas_dato[[nm]] < fc)
      atrasados[[length(atrasados) + 1]] <- list(nombre = nm, fecha = format(fechas_dato[[nm]], "%d/%m"))
  }
  habil_ant <- .dia_habil_anterior(Sys.Date())
  alerta_global <- if (fc < habil_ant)
    sprintf("El cierre más reciente es del %s, no del día hábil anterior (%s).",
            format(fc, "%d/%m"), format(habil_ant, "%d/%m")) else NULL

  list(
    fecha_cierre  = sprintf("%02d de %s de %d", day(fc), MESES_ES[month(fc)], year(fc)),
    fecha_cierre_date = fc,
    mes_ant_label = mes_ant_label,
    categorias    = cats_out,
    indices_macro = macro,
    rf_usa        = rf_usa,
    alertas       = list(global = alerta_global, fondos = atrasados),
    generado      = NULL
  )
}

if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a
