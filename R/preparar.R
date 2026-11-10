# =============================================================================
# PREPARAR DATOS DEL PULSO (compartido por la app y por scripts/correo_diario.R)
# A partir de data/series_vc.rds + dividendos + VC manuales + IPSA de Yahoo arma
# `pd`, lo que muestra el dashboard: asi el correo diario lleva exactamente las
# mismas cifras. Requiere ya cargados: fondos_pulso, calculos, dividendos,
# ajustes y compute (ver app.R).
# =============================================================================
if (!exists("DATA_RDS")) DATA_RDS <- "data/series_vc.rds"

default_vc <- function() data.frame(Nombre = character(), Tipo = character(),
                                    Fecha = character(), Valor = numeric(), stringsAsFactors = FALSE)
default_ov <- function() data.frame(Fondo = character(), `Fecha limite` = character(),
                                    Monto = numeric(), check.names = FALSE, stringsAsFactors = FALSE)

# Serie (fecha, valor_cuota) de una entidad manual desde la tabla larga de VC
serie_de_vc <- function(vc_df, nombre) {
  s <- vc_df[vc_df$Nombre == nombre, c("Fecha", "Valor"), drop = FALSE]
  if (!nrow(s)) return(NULL)
  out <- tibble(fecha = as.Date(s$Fecha), valor_cuota = suppressWarnings(as.numeric(s$Valor))) %>%
    filter(!is.na(fecha), !is.na(valor_cuota)) %>% arrange(fecha) %>% distinct(fecha, .keep_all = TRUE)
  if (nrow(out)) out else NULL
}
construir_series_manual <- function(vc_df) {
  res <- list()
  for (nm in NOMBRES_FONDOS_MANUALES) { s <- serie_de_vc(vc_df, nm); if (!is.null(s)) res[[nm]] <- s }
  res
}
# Serie automatica del IPSA: la oficial que guarda el cron (BCCh / datosmacro, en
# macro_series) + el cierre diario de Yahoo (MXIPSAGC.SN, data/ipsa_yahoo.csv, lo
# guarda el workflow ipsa_yahoo.yml tras el cierre). En la misma fecha manda la oficial.
serie_ipsa_auto <- function(datos) {
  of <- datos$macro_series[["IPSA"]]
  of <- if (!is.null(of) && nrow(of)) tibble(fecha = as.Date(of$fecha), valor_cuota = as.numeric(of$valor)) else NULL
  y <- if (file.exists("data/ipsa_yahoo.csv"))
    tryCatch(read.csv("data/ipsa_yahoo.csv", colClasses = "character"), error = function(e) NULL) else NULL
  y <- if (!is.null(y) && nrow(y)) tibble(fecha = as.Date(y$fecha), valor_cuota = suppressWarnings(as.numeric(y$cierre))) else NULL
  s <- bind_rows(of, y)
  if (!nrow(s)) return(NULL)
  s %>% filter(!is.na(fecha), !is.na(valor_cuota)) %>% distinct(fecha, .keep_all = TRUE) %>% arrange(fecha)
}

# Inyecta WTD/MTD/YTD de los indices manuales en la lista macro. Cada indice usa
# su VC manual y, en las fechas sin VC manual, la serie automatica de `auto`
# (ej. IPSA: oficial + Yahoo): si un dia no se ingresa a mano, igual queda al dia.
aplicar_indices_manuales <- function(macro, vc_df, fc, auto = list()) {
  ref_wtd <- fc - 7; ref_mtd <- floor_date(fc, "month") - 1
  ref_ytd <- as.Date(sprintf("%d-12-31", year(fc) - 1))
  ref_mes_ant_fin <- ref_mtd; ref_mes_ant_ini <- floor_date(ref_mes_ant_fin, "month") - 1
  for (nm in NOMBRES_INDICES_MANUALES) {
    s <- bind_rows(serie_de_vc(vc_df, nm), auto[[nm]])   # en la misma fecha manda el manual
    if (!nrow(s)) next
    s <- s %>% distinct(fecha, .keep_all = TRUE) %>% arrange(fecha)
    for (j in seq_along(macro)) if (identical(macro[[j]]$nombre, nm)) {
      macro[[j]]$wtd <- .rent(s, ref_wtd, fc)
      macro[[j]]$mtd <- .rent(s, ref_mtd, fc)
      macro[[j]]$ytd <- .rent(s, ref_ytd, fc)
      macro[[j]]$mes_ant <- .rent(s, ref_mes_ant_ini, ref_mes_ant_fin)
    }
  }
  macro
}

preparar_todo <- function(vc_df, ov_df = NULL) {
  # Lee SOLO del disco: series_vc.rds, fondos_curados.csv, dividendos.xlsx y
  # dividendos_calculados.csv los mantiene al dia sincronizar_datos() (al arrancar
  # y cada vez que cambia el commit de GitHub, via datos_ver en el server). Antes
  # se bajaban en cada render, y la lista de fondos podia quedar vieja hasta
  # reiniciar la app.
  if (!file.exists(DATA_RDS)) return(NULL)
  datos <- readRDS(DATA_RDS)
  fc <- as.Date(datos$cierre)
  # Dividendos (FIRES/FINRE) + factor de reparto (RGFMU) -> valor_cuota_ajustado
  ruta_div <- if (file.exists("data/dividendos.xlsx")) "data/dividendos.xlsx" else NULL
  div <- fusionar_overrides(cargar_dividendos_pulso(ruta_div), ov_df)
  series_aj <- enriquecer_series_ajustadas(series_por_nombre(datos$series), div)   # rds por run-serie -> nombre actual
  macro <- aplicar_indices_manuales(datos$macro, vc_df, fc, auto = list(IPSA = serie_ipsa_auto(datos)))
  series_man <- construir_series_manual(vc_df)
  # "MBI Deuda estructurada" es manual (VC ingresado a mano) pero sus dividendos SI
  # se cargan del boletin (ticker CFI-MBICHA, ver MAPEO_SEBRA); sin este ajuste
  # quedaban registrados en `div` pero nunca se sumaban al VC para la rentabilidad.
  if (!is.null(series_man[["MBI Deuda estructurada"]])) {
    ev <- div[["MBI Deuda estructurada"]]
    ev_t <- if (!is.null(ev)) tibble(fecha_limite = as.Date(ev$fecha_limite),
                                     monto = as.numeric(ev$monto), moneda = NA_character_) else NULL
    series_man[["MBI Deuda estructurada"]] <- aplicar_dividendos_a_historico(
      series_man[["MBI Deuda estructurada"]], ev_t, moneda_vc = "$")
  }
  pd <- construir_pulso_data(series_aj, fecha_cierre = fc, macro = macro,
                             rf_usa = datos$rf_usa, series_manual = series_man)
  list(pd = pd, datos = datos, series_aj = series_aj, div = div)
}
preparar_pd <- function(vc_df, ov_df = NULL) { t <- preparar_todo(vc_df, ov_df); if (is.null(t)) NULL else t$pd }
