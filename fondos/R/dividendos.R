# =============================================================================
# DIVIDENDOS - CARGA DESDE BOLETIN BURSATIL DE LA BOLSA DE SANTIAGO
#
# Funciones:
#   descargar_boletin_bursatil(carpeta)  -> descarga el xlsx del dia (con cache)
#   cargar_eventos_boletin(ruta_xlsx)    -> lee y normaliza hojas de dividendos
#                                            y repartos
#   cruzar_eventos_catalogo(events, cat) -> joinea por (nombre, serie) y agrega
#                                            id (run_serie_tipoentidad)
#   aplicar_dividendos_a_historico(...)  -> agrega columnas div_acum y
#                                            valor_cuota_ajustado al historico
#
# Identificacion del fondo: por (nombre_normalizado, serie_normalizada).
# Esto evita depender de nemotecnicos (que el catalogo CMF no tiene) y captura
# ~96% de los fondos.
#
# Moneda: por defecto solo se consideran dividendos en CLP ($), porque sumar
# US$ a un VC en CLP rompe la unidad. Se puede pasar moneda="US$" para fondos
# en dolares.
# =============================================================================

library(httr)
library(readxl)
library(dplyr)
library(stringr)
library(lubridate)
library(tibble)


# -----------------------------------------------------------------------------
# Helpers de normalizacion
# -----------------------------------------------------------------------------

#' Quita acentos, pasa a mayusculas y colapsa espacios
.normalizar_nombre <- function(s) {
  if (is.null(s)) return("")
  s <- as.character(s)
  s <- stringi::stri_trans_general(s, "Latin-ASCII")
  s <- toupper(trimws(s))
  s <- gsub("\\s+", " ", s)
  s
}

#' Quita acentos, mayusculas, y elimina texto entre parentesis al final:
#' "C (BCI)" -> "C", "L (Clasica)" -> "L"
.normalizar_serie <- function(s) {
  if (is.null(s)) return("")
  s <- as.character(s)
  s <- toupper(trimws(s))
  s <- stringi::stri_trans_general(s, "Latin-ASCII")
  s <- gsub("\\s*\\([^)]*\\)\\s*", "", s)
  trimws(s)
}

#' Parsea fechas en muchos formatos (serial Excel, dd/mm/yyyy, yyyy-mm-dd, etc.)
.parse_fecha <- function(x) {
  x <- trimws(as.character(x))
  n <- length(x)
  out <- as.Date(rep(NA_real_, n), origin = "1970-01-01")
  num <- suppressWarnings(as.numeric(x))
  es_serial <- !is.na(num) & num > 30000 & num < 80000
  out[es_serial] <- as.Date(num[es_serial], origin = "1899-12-30")
  fmts <- c("%Y-%m-%d", "%d/%m/%Y", "%d-%m-%Y", "%Y/%m/%d", "%m/%d/%Y")
  for (fmt in fmts) {
    falta <- is.na(out) & nzchar(x) & x != "NA"
    if (!any(falta)) break
    cand <- suppressWarnings(as.Date(x[falta], format = fmt))
    ok <- !is.na(cand)
    out[which(falta)[ok]] <- cand[ok]
  }
  out
}

#' Parsea montos en formato chileno o usa-style indistintamente.
#' "0,003133", "1.234,56", "0.5" -> 0.003133, 1234.56, 0.5
.parse_monto <- function(x) {
  x <- gsub("\\s", "", as.character(x))
  tiene_coma <- grepl(",", x, fixed = TRUE)
  # Si tiene coma, formato chileno: el punto es separador de miles
  x[tiene_coma] <- gsub("\\.", "", x[tiene_coma])
  x[tiene_coma] <- gsub(",", ".", x[tiene_coma], fixed = TRUE)
  suppressWarnings(as.numeric(x))
}


# -----------------------------------------------------------------------------
# Descarga del Excel del boletin (con cache automatico)
# -----------------------------------------------------------------------------

#' Descarga el Excel del Boletin Bursatil de la Bolsa de Santiago.
#'
#' Busca primero en `carpeta` un archivo ya descargado del dia. Si no existe,
#' intenta descargar de varias URLs candidatas (la Bolsa cambio el path varias
#' veces). Devuelve la ruta al archivo, o NULL si no fue posible.
#'
#' @param carpeta carpeta destino (default: "data/")
#' @param forzar  si TRUE, descarga aunque ya exista uno de hoy
#' @return ruta absoluta al .xlsx, o NULL
descargar_boletin_bursatil <- function(carpeta = "data", forzar = FALSE) {
  if (!dir.exists(carpeta)) dir.create(carpeta, recursive = TRUE)
  fecha_str <- format(Sys.Date(), "%Y%m%d")
  ruta_local <- file.path(carpeta, paste0(fecha_str, "_resumen_dividendos.xlsx"))

  if (!forzar && file.exists(ruta_local)) {
    message("[dividendos] Boletin del dia ya descargado: ", ruta_local)
    return(normalizePath(ruta_local))
  }

  # Si no es de hoy, buscar el mas reciente cacheado
  cacheados <- list.files(carpeta, pattern = "^\\d{8}_resumen_dividendos\\.xlsx$",
                          full.names = TRUE)
  if (!forzar && length(cacheados) > 0) {
    mas_reciente <- cacheados[order(basename(cacheados), decreasing = TRUE)][1]
    fecha_cache  <- as.Date(substr(basename(mas_reciente), 1, 8), format = "%Y%m%d")
    # Si el cache es de menos de 24h, usalo
    if (!is.na(fecha_cache) && Sys.Date() - fecha_cache <= 1) {
      message("[dividendos] Usando boletin cacheado (", basename(mas_reciente), ")")
      return(normalizePath(mas_reciente))
    }
  }

  # Intentar descargar
  message("[dividendos] Descargando boletin del dia (", fecha_str, ")...")
  urls <- c(
    sprintf("https://www.bolsadesantiago.com/content/estadisticas/resumen_dividendos_repartos_emisiones/%s_resumen_dividendos_-_repartos_-_emisiones.xlsx", fecha_str),
    sprintf("https://www.bolsadesantiago.com/content/estadisticas/%s_resumen_dividendos_-_repartos_-_emisiones.xlsx", fecha_str)
  )

  for (url in urls) {
    resp <- tryCatch(
      httr::GET(url,
                httr::add_headers(
                  "User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36",
                  "Accept" = "*/*",
                  "Referer" = "https://www.bolsadesantiago.com/estadisticas_boletinbursatil"
                ),
                httr::write_disk(ruta_local, overwrite = TRUE),
                httr::timeout(60)),
      error = function(e) { message("  Error: ", e$message); NULL }
    )
    if (!is.null(resp) && httr::status_code(resp) == 200 && file.size(ruta_local) > 10000) {
      message("[dividendos] OK -> ", ruta_local, " (", round(file.size(ruta_local)/1024), " KB)")
      return(normalizePath(ruta_local))
    }
    if (file.exists(ruta_local)) file.remove(ruta_local)
  }

  # Si fallo todo, intentar usar un cache aunque sea viejo
  if (length(cacheados) > 0) {
    mas_reciente <- cacheados[order(basename(cacheados), decreasing = TRUE)][1]
    message("[dividendos] Descarga fallo, usando cache antiguo: ", basename(mas_reciente))
    return(normalizePath(mas_reciente))
  }

  message("[dividendos] No se pudo obtener el boletin (sin descarga y sin cache).")
  NULL
}


#' Descarga el ULTIMO boletin disponible probando fechas hacia atras.
#' La Bolsa publica el Excel en una ruta con la fecha; iterando desde hoy hacia
#' atras y quedandose con el primero que exista garantiza "siempre el ultimo".
#'
#' @param carpeta destino
#' @param dias_atras cuantos dias retroceder como maximo (default 12)
#' @return ruta al xlsx mas reciente, o NULL
descargar_ultimo_boletin <- function(carpeta = "data", dias_atras = 12) {
  if (!dir.exists(carpeta)) dir.create(carpeta, recursive = TRUE)
  ua <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36"
  for (k in 0:dias_atras) {
    dia <- Sys.Date() - k
    if (lubridate::wday(dia, week_start = 1) >= 6) next      # saltar fin de semana
    fecha_str <- format(dia, "%Y%m%d")
    ruta_local <- file.path(carpeta, paste0(fecha_str, "_resumen_dividendos.xlsx"))
    if (file.exists(ruta_local) && file.size(ruta_local) > 10000) {
      message("[dividendos] Ultimo boletin (cache): ", basename(ruta_local)); return(normalizePath(ruta_local))
    }
    # Host de descargas de documentos (servicioscms): devuelve el xlsx REAL sin
    # captcha ni cookies (a diferencia de www.*, que esta tras un WAF hCaptcha).
    # attach = "Noticias/avisos generales/<YYYYMMDD> resumen dividendos - repartos - emisiones.xlsx"
    attach <- sprintf("Noticias/avisos generales/%s resumen dividendos - repartos - emisiones.xlsx", fecha_str)
    urls <- c(paste0("https://servicioscms.bolsadesantiago.com/Paginas/Descarga.aspx?attach=",
                     utils::URLencode(attach, reserved = TRUE)))
    for (url in urls) {
      resp <- tryCatch(httr::GET(url, httr::add_headers("User-Agent" = ua, "Accept" = "*/*"),
                       httr::write_disk(ruta_local, overwrite = TRUE), httr::timeout(120)),
                       error = function(e) NULL)
      ok_xlsx <- FALSE
      if (!is.null(resp) && httr::status_code(resp) == 200 && file.size(ruta_local) > 10000) {
        # Validar firma ZIP "PK": la bolsa devuelve el HTML del SPA para rutas
        # inexistentes, asi que un 200 NO garantiza que sea un xlsx real.
        sig <- tryCatch(readBin(ruta_local, "raw", 2), error = function(e) raw(0))
        ok_xlsx <- length(sig) == 2 && sig[1] == as.raw(0x50) && sig[2] == as.raw(0x4B)
      }
      if (ok_xlsx) {
        message("[dividendos] Ultimo boletin descargado: ", basename(ruta_local),
                " (", round(file.size(ruta_local)/1024), " KB, dia ", fecha_str, ")")
        return(normalizePath(ruta_local))
      }
      if (file.exists(ruta_local)) file.remove(ruta_local)
    }
  }
  message("[dividendos] No se encontro boletin en los ultimos ", dias_atras, " dias.")
  NULL
}


# -----------------------------------------------------------------------------
# Carga de eventos (dividendos + repartos) desde el Excel
# -----------------------------------------------------------------------------

#' Lee las hojas "Dividendos CFI nacionales" y "Repartos CFI-CFM" del Excel
#' y devuelve un tibble normalizado con todos los eventos.
#'
#' @param ruta_excel ruta al .xlsx
#' @param fecha_minima eventos con fecha_limite < esta fecha se descartan
#' @return tibble(nombre_norm, serie_norm, nombre_original, serie_original,
#'                tipo_evento, moneda, fecha_limite, fecha_pago, monto)
cargar_eventos_boletin <- function(ruta_excel, fecha_minima = as.Date("2025-01-01")) {
  vacio <- tibble(
    nombre_norm = character(), serie_norm = character(),
    nombre_original = character(), serie_original = character(),
    tipo_evento = character(), moneda = character(),
    fecha_limite = as.Date(character()), fecha_pago = as.Date(character()),
    monto = numeric()
  )
  if (is.null(ruta_excel) || !file.exists(ruta_excel)) {
    message("[dividendos] Archivo Excel no encontrado.")
    return(vacio)
  }

  leer_hoja <- function(sheet, skip, tipo_evento) {
    df <- tryCatch(
      suppressMessages(readxl::read_excel(ruta_excel, sheet = sheet,
                                          skip = skip, col_types = "text")),
      error = function(e) { message("  Error leyendo ", sheet, ": ", e$message); NULL }
    )
    if (is.null(df) || nrow(df) == 0) return(NULL)

    # Localizar columnas por nombre (case-insensitive, robust to small changes)
    cols <- tolower(colnames(df))
    idx_nombre  <- which(grepl("^nombre", cols))[1]
    idx_serie   <- which(grepl("^serie", cols))[1]
    idx_moneda  <- which(grepl("^moneda", cols))[1]
    idx_monto   <- which(grepl("por acci|cuota", cols))[1]
    idx_limite  <- which(grepl("l.mite|limite", cols))[1]
    idx_pago    <- which(grepl("^pago", cols))[1]

    if (any(is.na(c(idx_nombre, idx_serie, idx_monto, idx_limite)))) {
      message("  Hoja ", sheet, ": columnas clave no encontradas.")
      return(NULL)
    }

    tibble(
      nombre_original = as.character(df[[idx_nombre]]),
      serie_original  = as.character(df[[idx_serie]]),
      moneda          = if (!is.na(idx_moneda)) as.character(df[[idx_moneda]]) else NA_character_,
      monto_raw       = as.character(df[[idx_monto]]),
      fecha_lim_raw   = as.character(df[[idx_limite]]),
      fecha_pago_raw  = if (!is.na(idx_pago)) as.character(df[[idx_pago]]) else NA_character_,
      tipo_evento     = tipo_evento
    )
  }

  l1 <- leer_hoja("Dividendos CFI nacionales", skip = 11, tipo_evento = "Dividendo")
  l2 <- leer_hoja("Repartos CFI-CFM",          skip = 10, tipo_evento = "Reparto")
  df <- bind_rows(l1, l2)
  if (nrow(df) == 0) return(vacio)

  df <- df %>%
    filter(!is.na(nombre_original) & nombre_original != "NA" & nzchar(nombre_original),
           !is.na(serie_original)  & serie_original  != "NA" & nzchar(serie_original)) %>%
    mutate(
      nombre_norm   = sapply(nombre_original, .normalizar_nombre),
      serie_norm    = sapply(serie_original,  .normalizar_serie),
      fecha_limite  = .parse_fecha(fecha_lim_raw),
      fecha_pago    = .parse_fecha(fecha_pago_raw),
      monto         = .parse_monto(monto_raw),
      moneda        = trimws(moneda)
    ) %>%
    filter(!is.na(fecha_limite), !is.na(monto), monto > 0,
           fecha_limite >= fecha_minima) %>%
    select(nombre_norm, serie_norm, nombre_original, serie_original,
           tipo_evento, moneda, fecha_limite, fecha_pago, monto) %>%
    distinct(nombre_norm, serie_norm, tipo_evento, fecha_limite, moneda,
             .keep_all = TRUE) %>%
    arrange(nombre_norm, serie_norm, fecha_limite)

  message("[dividendos] Eventos cargados: ", nrow(df),
          " | desde ", min(df$fecha_limite, na.rm = TRUE),
          " hasta ", max(df$fecha_limite, na.rm = TRUE))
  df
}


# -----------------------------------------------------------------------------
# Cruce eventos <-> catalogo del dashboard
# -----------------------------------------------------------------------------

#' Joinea eventos del boletin con el catalogo del dashboard por (nombre, serie).
#'
#' @param eventos tibble de cargar_eventos_boletin()
#' @param catalogo tibble del catalogo (debe tener: nombre, run, serie, tipoentidad)
#' @return tibble eventos enriquecido con (id, run, serie, tipoentidad)
cruzar_eventos_catalogo <- function(eventos, catalogo) {
  cat_norm <- catalogo %>%
    mutate(
      nombre_norm = sapply(nombre, .normalizar_nombre),
      serie_norm  = sapply(serie,  .normalizar_serie),
      id          = paste0(run, "_", serie, "_", tipoentidad)
    ) %>%
    select(id, run, serie, tipoentidad, nombre_norm, serie_norm)

  out <- eventos %>%
    inner_join(cat_norm, by = c("nombre_norm", "serie_norm"))

  message("[dividendos] Cruce con catalogo: ", nrow(out),
          " eventos / ", length(unique(out$id)), " series con eventos")
  out
}


# -----------------------------------------------------------------------------
# Aplicacion de dividendos al historico de un fondo
# -----------------------------------------------------------------------------

#' Agrega columnas `div_acum` y `valor_cuota_ajustado` al historico de un fondo.
#'
#' Para cada fila del historico:
#'   div_acum(t)            = sum(monto donde fecha_limite <= t)
#'   valor_cuota_ajustado(t) = valor_cuota(t) + div_acum(t)
#'
#' Si no hay eventos para el fondo, devuelve el historico con div_acum=0 y
#' valor_cuota_ajustado = valor_cuota.
#'
#' @param historico tibble(fecha, valor_cuota)
#' @param eventos_fondo tibble con fecha_limite y monto (eventos de ESTE fondo)
#' @param moneda_vc moneda del VC ("$" o "US$"). Solo se aplican dividendos en
#'   esa misma moneda.
#' @return historico con columnas extra
aplicar_dividendos_a_historico <- function(historico, eventos_fondo,
                                            moneda_vc = "$") {
  if (is.null(historico) || nrow(historico) == 0) return(historico)

  # Si no hay eventos: VCA = VC
  if (is.null(eventos_fondo) || nrow(eventos_fondo) == 0) {
    historico$div_acum <- 0
    historico$valor_cuota_ajustado <- historico$valor_cuota
    return(historico)
  }

  # Filtrar eventos a la misma moneda que el VC
  ev <- eventos_fondo %>%
    filter(is.na(moneda) | moneda == moneda_vc | (moneda_vc == "$" & moneda %in% c("$","CLP","Pesos","CLP$"))) %>%
    arrange(fecha_limite)

  if (nrow(ev) == 0) {
    historico$div_acum <- 0
    historico$valor_cuota_ajustado <- historico$valor_cuota
    return(historico)
  }

  # Calculo vectorizado: para cada fecha del historico, suma monto donde fecha_limite <= fecha
  historico$div_acum <- sapply(historico$fecha, function(f) {
    sum(ev$monto[ev$fecha_limite <= f], na.rm = TRUE)
  })
  historico$valor_cuota_ajustado <- historico$valor_cuota + historico$div_acum
  historico
}


# -----------------------------------------------------------------------------
# Factor de Reparto (FFMM / RGFMU)
# -----------------------------------------------------------------------------

#' Ajusta el VC de un Fondo Mutuo por el Factor de Reparto de la CMF.
#'
#' A diferencia de los fondos de inversion (que se ajustan con dividendos
#' aditivos del Boletin Bursatil), los FFMM traen en la propia tabla de la CMF
#' una columna "Factor de Reparto" con un factor multiplicativo en cada fecha
#' de reparto (vacio = sin reparto = factor 1). Replica la logica del script
#' original: FA_acumulado(t) = FA1 x FA2 x ... (multiplicacion, no suma).
#'
#'   valor_cuota_ajustado(t) = valor_cuota(t) x prod(factor_reparto[fecha <= t])
#'
#' Asi la rentabilidad ajustada = VCA_fin / VCA_ini - 1 aplica solo los
#' repartos ocurridos dentro del periodo. Si no hay columna factor_reparto o
#' todos son 1, VCA = VC.
#'
#' @param historico tibble(fecha, valor_cuota[, factor_reparto])
#' @return historico con columnas `div_acum` y `valor_cuota_ajustado`
aplicar_factor_reparto <- function(historico) {
  if (is.null(historico) || nrow(historico) == 0) return(historico)

  historico <- historico %>% dplyr::arrange(fecha)

  fr <- if ("factor_reparto" %in% colnames(historico)) as.numeric(historico$factor_reparto) else NA_real_
  fr <- rep_len(fr, nrow(historico))
  fr[is.na(fr) | fr <= 0] <- 1            # dias sin reparto -> factor neutro
  fa_acum <- cumprod(fr)

  historico$valor_cuota_ajustado <- historico$valor_cuota * fa_acum
  # div_acum: equivalente en pesos del ajuste acumulado (para la columna "Div Acum.")
  historico$div_acum <- historico$valor_cuota_ajustado - historico$valor_cuota
  historico
}
