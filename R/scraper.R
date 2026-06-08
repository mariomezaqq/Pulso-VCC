# =============================================================================
# SCRAPER CMF
# Funciones extraidas del script `resumen_semanal_ONEDRIVE.R` y adaptadas
# para uso modular en el dashboard. La logica de POST + parsing es identica.
#
# DIAGNOSTICO:
# Todas las funciones usan message() para logear a la consola R cada paso
# importante (URL, HTTP status, tablas encontradas, series disponibles).
# Cuando un fondo no devuelve datos, mira la consola para ver POR QUE.
# =============================================================================


#' Limpia un token reCAPTCHA contra errores comunes de copia/paste.
#'
#' Cuando el usuario copia del DevTools de Chrome, a veces incluye basura
#' alrededor del valor real (ej. termina con "&enviado=1" porque copio
#' el payload completo y no solo el campo). Esta funcion deja solo los
#' caracteres validos del token (alfanumericos, _ , -).
#'
#' @param tok string crudo
#' @return string saneado
sanitize_token <- function(tok) {
  if (is.null(tok)) return("")
  tok <- as.character(tok)
  # Quitar TODO el whitespace (incluye saltos de linea internos por word-wrap)
  tok <- gsub("\\s+", "", tok)
  # Si vino con parametros extra pegados (ej. "...&enviado=1" o "...=valor"),
  # cortar en el primer & o =. El token valido solo contiene [A-Za-z0-9_-].
  tok <- sub("[&=].*$", "", tok)
  tok
}


#' Obtiene la tabla cruda de valor cuota desde la CMF para un fondo
#'
#' @param fondo Lista con campos: run, serie, row, tipoentidad, nombre
#' @param desde Date de inicio del rango de consulta
#' @param hasta Date de fin del rango de consulta
#' @param token reCAPTCHA token (no requerido para RGFMU)
#' @param cookies String con todas las cookies
#' @return data.frame crudo (NULL si falla)
obtener_datos_cmf <- function(fondo, desde, hasta, token, cookies) {

  es_ffmm <- !is.null(fondo$tipoentidad) && fondo$tipoentidad == "RGFMU"
  etiqueta <- paste0(fondo$nombre %||% paste0("run=", fondo$run),
                     " (serie=", fondo$serie, ", ", fondo$tipoentidad %||% "FIRES", ")")

  # Sanitizar token y cookies por las dudas
  token   <- sanitize_token(token)
  cookies <- if (is.null(cookies)) "" else gsub("[\r\n]", "", as.character(cookies))

  message("[scraper] === ", etiqueta, " ===")
  message("[scraper] rango: ", desde, " a ", hasta)
  message("[scraper] token len: ", nchar(token),
          if (nchar(token) > 0 && nchar(token) < 100) " <- SOSPECHOSO, los tokens recaptcha son ~1500-2500 chars" else "")

  if (!es_ffmm && nchar(token) < 100) {
    message("[scraper] ABORT: token muy corto o vacio. Re-genera el token en DevTools.")
    return(NULL)
  }

  url_cmf <- paste0(
    "https://www.cmfchile.cl/institucional/mercados/entidad.php",
    "?mercado=V&rut=", fondo$run,
    "&grupo=&tipoentidad=", if (!is.null(fondo$tipoentidad)) fondo$tipoentidad else "FIRES",
    if (nchar(fondo$row) > 0) paste0("&row=", fondo$row) else "",
    "&vig=VI&control=svs&pestania=7"
  )
  message("[scraper] URL: ", url_cmf)

  if (es_ffmm) {
    # ---- RGFMU (Fondos Mutuos): no requiere captcha, pero suele necesitar cookies ----
    if (nchar(cookies) == 0) {
      message("[scraper] WARN: RGFMU sin cookies. Si falla, pega tus cookies en credentials.R")
    }

    body_params <- list(
      mercado = "V", rut = fondo$run, grupo = "", tipoentidad = "RGFMU",
      row = fondo$row, vig = "VI", control = "svs", pestania = "7",
      ddi = format(desde, "%d"), mmi = format(desde, "%m"), aai = format(desde, "%Y"),
      ddf = format(hasta, "%d"), mmf = format(hasta, "%m"), aaf = format(hasta, "%Y"),
      se = fondo$serie
    )

    resp <- tryCatch(
      httr::POST(url_cmf, httr::add_headers(
        "User-Agent"   = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36",
        "Content-Type" = "application/x-www-form-urlencoded",
        "Origin"       = "https://www.cmfchile.cl",
        "Referer"      = url_cmf, "Cookie" = cookies
      ), body = body_params, encode = "form", httr::timeout(30)),
      error = function(e) { message("[scraper] httr error: ", e$message); NULL }
    )

    if (is.null(resp)) return(NULL)
    message("[scraper] HTTP RGFMU paso 1: ", httr::status_code(resp))
    if (httr::status_code(resp) != 200) return(NULL)

    html_str <- httr::content(resp, "text", encoding = "UTF-8")
    message("[scraper] body size: ", nchar(html_str), " chars")

    pagina    <- rvest::read_html(html_str)
    url_serie <- pagina %>%
      rvest::html_nodes("[onclick]") %>%
      rvest::html_attr("onclick") %>%
      .[grep("valor_serie.php", .)] %>%
      stringr::str_extract("(?<=ventana\\(')[^']+") %>%
      .[!is.na(.)] %>%
      .[1]

    if (length(url_serie) == 0 || is.na(url_serie)) {
      message("[scraper] No se encontro link valor_serie.php en la respuesta.")
      # Diagnostico extra: ver si la pagina pide cookies o tiene mensaje de error
      if (stringr::str_detect(html_str, "(?i)cookie|sesion expirada|inicie sesion")) {
        message("[scraper] La respuesta menciona cookies/sesion. Pega cookies en credentials.R")
      }
      return(NULL)
    }
    message("[scraper] -> valor_serie: https://www.cmfchile.cl", url_serie)

    resp2 <- tryCatch(
      httr::GET(paste0("https://www.cmfchile.cl", url_serie),
                httr::add_headers("User-Agent" = "Mozilla/5.0",
                                  "Referer" = url_cmf, "Cookie" = cookies),
                httr::timeout(30)),
      error = function(e) { message("[scraper] httr error paso 2: ", e$message); NULL }
    )
    if (is.null(resp2)) return(NULL)
    message("[scraper] HTTP RGFMU paso 2: ", httr::status_code(resp2))
    if (httr::status_code(resp2) != 200) return(NULL)

    pagina2 <- rvest::read_html(httr::content(resp2, "text", encoding = "UTF-8"))
    tablas  <- pagina2 %>% rvest::html_nodes("table")
    message("[scraper] tablas en valor_serie.php: ", length(tablas))
    if (length(tablas) == 0) return(NULL)

    for (i in seq_along(tablas)) {
      df <- tryCatch(rvest::html_table(tablas[[i]], fill = TRUE), error = function(e) NULL)
      if (is.null(df) || nrow(df) < 2) next
      texto <- paste(unlist(df), collapse = " ")
      if (stringr::str_detect(texto, "\\d{2}[/-]\\d{2}[/-]\\d{4}") &&
          stringr::str_detect(texto, "\\d+[,.]\\d{2,}")) {
        message("[scraper] OK: tabla ", i, " (", nrow(df), " filas) parseada")
        return(df)
      }
    }
    message("[scraper] Ninguna tabla parecia tener (fecha + valor cuota).")
    return(NULL)

  } else {
    # ---- FIRES / FINRE: requiere reCAPTCHA token ----
    body_params <- list(
      dia1 = format(desde, "%d"), mes1 = format(desde, "%m"), anio1 = format(desde, "%Y"),
      dia2 = format(hasta, "%d"), mes2 = format(hasta, "%m"), anio2 = format(hasta, "%Y"),
      sub_consulta_fi = "Consultar",
      `g-recaptcha-response` = token,
      enviado = "1"
    )

    resp <- tryCatch(
      httr::POST(url_cmf, httr::add_headers(
        "User-Agent"      = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/145.0.0.0 Safari/537.36",
        "Accept"          = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
        "Accept-Language" = "es-419,es;q=0.9",
        "Content-Type"    = "application/x-www-form-urlencoded",
        "Origin"          = "https://www.cmfchile.cl",
        "Referer"         = url_cmf,
        "Cookie"          = cookies,
        "Upgrade-Insecure-Requests" = "1"
      ), body = body_params, encode = "form", httr::timeout(30)),
      error = function(e) { message("[scraper] httr error: ", e$message); NULL }
    )

    if (is.null(resp)) return(NULL)
    message("[scraper] HTTP: ", httr::status_code(resp))
    if (httr::status_code(resp) != 200) return(NULL)

    html_str <- httr::content(resp, "text", encoding = "UTF-8")
    message("[scraper] body size: ", nchar(html_str), " chars")

    # Heuristica: detectar rechazo de captcha
    if (stringr::str_detect(html_str, "(?i)captcha|recaptcha") &&
        !stringr::str_detect(html_str, "\\d{2}[/-]\\d{2}[/-]\\d{4}")) {
      message("[scraper] La respuesta menciona captcha y no trae fechas. ",
              "Probablemente el token esta rechazado o expirado (los tokens duran ~2 minutos).")
    }

    pagina <- rvest::read_html(html_str)
    tablas <- pagina %>% rvest::html_nodes("table")
    message("[scraper] tablas en respuesta: ", length(tablas))
    if (length(tablas) == 0) return(NULL)

    for (i in seq_along(tablas)) {
      df <- tryCatch(rvest::html_table(tablas[[i]], fill = TRUE), error = function(e) NULL)
      if (is.null(df) || nrow(df) < 2) next
      texto <- paste(unlist(df), collapse = " ")
      if (stringr::str_detect(texto, "\\d{2}[/-]\\d{2}[/-]\\d{4}") &&
          stringr::str_detect(texto, "\\d+[,.]\\d{2,}")) {
        message("[scraper] OK: tabla ", i, " (", nrow(df), " filas) parseada")
        if ("Serie" %in% colnames(df)) {
          message("[scraper] Series disponibles en respuesta: ",
                  paste(sort(unique(df$Serie)), collapse = " | "))
        }
        return(df)
      }
    }
    message("[scraper] Ninguna tabla parecia tener (fecha + valor cuota). ",
            "Si esto pasa para todos los fondos FIRES/FINRE, casi seguro el token esta vencido.")
    return(NULL)
  }
}


#' Limpia la tabla cruda CMF y devuelve un data.frame de serie temporal
#'
#' @param df_raw data.frame devuelto por obtener_datos_cmf()
#' @param serie codigo de serie (A, B, F, etc.)
#' @param es_ffmm TRUE si tipoentidad == "RGFMU"
#' @return tibble con columnas: fecha (Date), valor_cuota (numeric), ordenado ascendente
limpiar_cmf <- function(df_raw, serie, es_ffmm = FALSE) {

  if (is.null(df_raw)) return(NULL)

  # En RGFMU los encabezados estan en la fila 2
  if (es_ffmm) {
    colnames(df_raw) <- stringr::str_trim(as.character(df_raw[2, ]))
    df_raw <- df_raw[-(1:2), ]
    df_raw <- df_raw %>% dplyr::filter(!is.na(.[[1]]), .[[1]] != "")
  }

  # Filtrar por serie si la columna existe
  if ("Serie" %in% colnames(df_raw)) {
    series_disp <- sort(unique(df_raw$Serie))
    if (!serie %in% series_disp) {
      message("[scraper] Serie '", serie, "' NO esta en la respuesta. ",
              "Disponibles: ", paste(series_disp, collapse = " | "))
      return(NULL)
    }
    df_raw <- df_raw %>% dplyr::filter(Serie == serie)
    if (nrow(df_raw) == 0) return(NULL)
  }

  # Identificar columna de fecha y de valor
  col_fecha <- NA_integer_
  col_valor <- NA_integer_

  if (es_ffmm) {
    nombres <- tolower(stringr::str_trim(stringr::str_replace_all(colnames(df_raw),
                                                                  "[\n\t\r]+|\\s{2,}", " ")))
    idx_f <- which(nombres == "fecha")
    idx_v <- which(stringr::str_detect(nombres, "valor\\s*cuota"))
    if (length(idx_f) > 0) col_fecha <- idx_f[1]
    if (length(idx_v) > 0) col_valor <- idx_v[1]
  }

  if (is.na(col_fecha) || is.na(col_valor)) {
    for (j in seq_along(df_raw)) {
      vals <- as.character(df_raw[[j]])
      if (is.na(col_fecha) &&
          sum(stringr::str_detect(vals, "\\d{2}[/-]\\d{2}[/-]\\d{4}"), na.rm = TRUE) > 1) {
        col_fecha <- j
      } else if (is.na(col_valor)) {
        nums <- suppressWarnings(as.numeric(
          stringr::str_replace_all(stringr::str_replace_all(vals, "\\.", ""), ",", ".")
        ))
        if (sum(!is.na(nums), na.rm = TRUE) > 1) col_valor <- j
      }
    }
  }

  if (is.na(col_fecha) || is.na(col_valor)) {
    message("[scraper] No se identifico columna fecha o valor cuota en la tabla.")
    return(NULL)
  }

  # Factor de Reparto (solo FFMM): columna que la CMF usa para el ajuste.
  # Dias sin reparto vienen vacios -> se tratan como factor 1 mas adelante.
  # OJO: este factor usa "." como decimal (no formato chileno), ej "1.001637".
  col_freparto <- NA_integer_
  if (es_ffmm) {
    nombres_fr <- tolower(stringr::str_replace_all(colnames(df_raw), "[\n\t\r]+|\\s{2,}", " "))
    idx_fr <- which(stringr::str_detect(nombres_fr, "factor.*reparto"))
    if (length(idx_fr) > 0) col_freparto <- idx_fr[1]
  }

  historico <- df_raw %>%
    dplyr::transmute(
      fecha = lubridate::dmy(stringr::str_replace_all(as.character(.[[col_fecha]]), "-", "/")),
      valor_cuota = suppressWarnings(as.numeric(
        stringr::str_replace_all(stringr::str_replace_all(as.character(.[[col_valor]]),
                                                          "\\.", ""), ",", ".")
      )),
      factor_reparto = if (!is.na(col_freparto))
          suppressWarnings(as.numeric(gsub(",", ".", trimws(as.character(.[[col_freparto]])))))
        else NA_real_
    ) %>%
    dplyr::filter(!is.na(fecha), !is.na(valor_cuota)) %>%
    dplyr::arrange(fecha) %>%
    dplyr::distinct(fecha, .keep_all = TRUE)

  if (nrow(historico) == 0) {
    message("[scraper] Despues de limpiar la tabla quedaron 0 filas.")
    return(NULL)
  }
  message("[scraper] historico final: ", nrow(historico), " filas (",
          min(historico$fecha), " a ", max(historico$fecha), ")")
  historico
}


#' Wrapper: obtiene + limpia + cachea un fondo
#'
#' @param fondo lista con run, serie, row, tipoentidad, nombre
#' @param desde Date inicio
#' @param hasta Date fin
#' @param token reCAPTCHA token
#' @param cookies cookies CMF
#' @return tibble(fecha, valor_cuota) o NULL si falla
scrapear_fondo <- function(fondo, desde, hasta, token, cookies) {
  df_raw <- obtener_datos_cmf(fondo, desde, hasta, token, cookies)
  if (is.null(df_raw)) return(NULL)
  es_ffmm <- !is.null(fondo$tipoentidad) && fondo$tipoentidad == "RGFMU"
  limpiar_cmf(df_raw, fondo$serie, es_ffmm = es_ffmm)
}


# Helper coalesce (por si no esta definido al cargar este archivo solo)
if (!exists("%||%")) {
  `%||%` <- function(a, b) if (is.null(a)) b else a
}
