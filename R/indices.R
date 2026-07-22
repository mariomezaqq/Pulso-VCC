# =============================================================================
# INDICES MACRO + RENTA FIJA USA  (extraido de resumen_semanal_ONEDRIVE.R)
# IPSA y USD via BCCh (web scraping); resto via Yahoo (quantmod + fallback JSON);
# Treasury 10y (^TNX) para Renta Fija USA.
# =============================================================================
suppressMessages({
  library(httr); library(rvest); library(dplyr); library(tidyr)
  library(lubridate); library(stringr); library(tibble); library(jsonlite)
  library(quantmod)
})

# ---- IPSA: serie diaria oficial del BCCh (Canasta) ----
# Devuelve tibble(fecha, valor) con el historico reciente (~desde ene del anio).
# Es la fuente OFICIAL y al dia del IPSA; se prefiere sobre datosmacro/Yahoo
# (que se corrompio / quedo congelado). Sin credenciales.
obtener_ipsa_bcch_serie <- function() {
  message("  Descargando: IPSA (BCCh Canasta - serie oficial)")
  url <- "https://si3.bcentral.cl/siete/ES/Siete/Canasta?idCanasta=JQTEU1162911"
  historico <- tryCatch({
    resp <- httr::GET(url, httr::timeout(25),
                      httr::add_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36"))
    if (httr::status_code(resp) != 200) { message("    HTTP Error: ", httr::status_code(resp)); return(NULL) }
    page <- rvest::read_html(resp)
    headers <- page %>% html_nodes("table thead th") %>% html_text(trim = TRUE)
    if (length(headers) < 4) { message("    Sin encabezados de fecha"); return(NULL) }
    headers_fechas <- headers[4:length(headers)]
    filas <- page %>% html_nodes("table tbody tr")
    if (length(filas) == 0) { message("    Sin filas"); return(NULL) }
    celdas <- filas[1] %>% html_nodes("td") %>% html_text(trim = TRUE)
    if (length(celdas) < 4) { message("    Primera fila incompleta"); return(NULL) }
    valores_text <- celdas[4:length(celdas)]
    valores_num  <- suppressWarnings(as.numeric(gsub("\\.", "", valores_text) %>% gsub(",", ".", .)))
    ok <- !is.na(valores_num) & nzchar(valores_text)
    valores_num_clean <- valores_num[ok]; headers_fechas_clean <- headers_fechas[ok]
    if (!length(valores_num_clean)) { message("    Sin valores validos"); return(NULL) }
    meses_es <- c("ene"="01","feb"="02","mar"="03","abr"="04","may"="05","jun"="06",
                  "jul"="07","ago"="08","sep"="09","oct"="10","nov"="11","dic"="12")
    fechas_conv <- sapply(headers_fechas_clean, function(fs) {
      p <- strsplit(tolower(fs), "\\.")[[1]]
      if (length(p) == 3 && !is.na(meses_es[p[2]])) as.Date(paste(p[3], meses_es[p[2]], p[1], sep = "-")) else NA_Date_
    })
    tibble::as_tibble(list(fecha = as.Date(fechas_conv), valor = valores_num_clean)) %>%
      filter(!is.na(fecha), !is.na(valor)) %>% distinct(fecha, .keep_all = TRUE) %>% arrange(fecha)
  }, error = function(e) { message("    Error BCCh: ", e$message); NULL })
  if (is.null(historico) || !nrow(historico)) return(NULL)
  message("    ✓ IPSA BCCh: ", nrow(historico), " dias (",
          format(min(historico$fecha)), " -> ", format(max(historico$fecha)), ")")
  historico
}

# ---- IPSA (BCCh web) ----
obtener_ipsa_web <- function(fechas_objetivo) {
  message("  Descargando: IPSA (BCCh - Web Scraping)")

  url <- "https://si3.bcentral.cl/siete/ES/Siete/Canasta?idCanasta=JQTEU1162911"

  historico <- tryCatch({
    resp <- httr::GET(url, 
                      httr::timeout(20),
                      httr::add_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36"))
    
    if (httr::status_code(resp) != 200) {
      message("    HTTP Error: ", httr::status_code(resp))
      return(NULL)
    }
    
    page <- rvest::read_html(resp)
    
    headers <- page %>% 
      html_nodes("table thead th") %>% 
      html_text(trim = TRUE)
    
    if (length(headers) < 4) {
      message("    No se encontraron encabezados de fecha")
      return(NULL)
    }
    
    headers_fechas <- headers[4:length(headers)]
    
    filas <- page %>% 
      html_nodes("table tbody tr")
    
    if (length(filas) == 0) {
      message("    No se encontraron filas de datos")
      return(NULL)
    }
    
    fila_1 <- filas[1]
    celdas <- fila_1 %>% 
      html_nodes("td") %>% 
      html_text(trim = TRUE)
    
    if (length(celdas) < 4) {
      message("    Primera fila incompleta")
      return(NULL)
    }
    
    valores_text <- celdas[4:length(celdas)]
    
    valores_num <- suppressWarnings(as.numeric(
      gsub("\\.", "", valores_text) %>%
        gsub(",", ".", .)
    ))
    
    valores_num_clean <- valores_num[!is.na(valores_num) & nzchar(valores_text)]
    headers_fechas_clean <- headers_fechas[!is.na(valores_num)]
    
    if (length(valores_num_clean) == 0) {
      message("    No hay valores numéricos válidos")
      return(NULL)
    }
    
    meses_es <- c(
      "ene" = "01", "feb" = "02", "mar" = "03", "abr" = "04",
      "may" = "05", "jun" = "06", "jul" = "07", "ago" = "08",
      "sep" = "09", "oct" = "10", "nov" = "11", "dic" = "12"
    )
    
    fechas_convertidas <- sapply(headers_fechas_clean, function(fecha_str) {
      fecha_lower <- tolower(fecha_str)
      partes <- strsplit(fecha_lower, "\\.")[[1]]
      
      if (length(partes) == 3) {
        dia <- partes[1]
        mes_es <- partes[2]
        year <- partes[3]
        mes <- meses_es[mes_es]
        
        if (!is.na(mes)) {
          fecha <- as.Date(paste(year, mes, dia, sep = "-"))
          return(fecha)
        }
      }
      NA_Date_
    })
    
    tibble::as_tibble(list(
      fecha = as.Date(fechas_convertidas),
      valor = valores_num_clean
    )) %>%
      filter(!is.na(fecha), !is.na(valor)) %>%
      arrange(fecha)
    
  }, error = function(e) {
    message("    Error: ", e$message)
    NULL
  })
  
  if (is.null(historico) || nrow(historico) == 0) {
    message("    Sin datos para IPSA")
    return(tibble(fecha = fechas_objetivo, valor = NA_real_))
  }
  
  message("    ✓ IPSA OK: ", nrow(historico), " registros (",
          format(min(historico$fecha)), " -> ", format(max(historico$fecha)), ")")
  
  resultados <- lapply(fechas_objetivo, function(f) {
    exacto <- historico %>% filter(fecha == f)
    if (nrow(exacto) > 0) return(tibble(fecha = f, valor = exacto$valor[1]))
    previo <- historico %>% filter(fecha <= f) %>% arrange(desc(fecha)) %>% head(1)
    if (nrow(previo) > 0) return(tibble(fecha = f, valor = previo$valor[1]))
    tibble(fecha = f, valor = NA_real_)
  })
  
  bind_rows(resultados)
}

# ---- IPSA (datosmacro.expansion.com - sin credenciales; dato MAS FRESCO que el BCCh) ----
# La pagina trae una tabla DIARIA del IPSA. El parametro ?dr=YYYY-MM devuelve la
# ventana (~21 dias habiles) terminada en ese mes; iterando los meses desde 'desde'
# se arma el historico continuo (las ventanas se solapan). Cada fila expone fecha
# ISO + valor (punto decimal) en data-value. WebFetch la bloquea; httr + UA da 200.
.ipsa_dm_parse <- function(html) {
  patron <- paste0('<td class="fecha" data-value="([0-9]{4}-[0-9]{2}-[0-9]{2})">',
                   '[^<]*</td><td class="numero" data-value="([0-9.]+)">')
  hits <- regmatches(html, gregexpr(patron, html, perl = TRUE))[[1]]
  if (!length(hits)) return(NULL)
  tibble(fecha = as.Date(sub(paste0(".*", patron, ".*"), "\\1", hits, perl = TRUE)),
         valor = suppressWarnings(as.numeric(sub(paste0(".*", patron, ".*"), "\\2", hits, perl = TRUE))))
}
obtener_ipsa_datosmacro <- function(desde = NULL, hasta = Sys.Date(),
                                    base = "https://datosmacro.expansion.com/bolsa/chile") {
  message("  Descargando: IPSA (datosmacro.expansion.com)")
  ua <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0 Safari/537.36"
  get1 <- function(u) {                      # con reintento ante hipos de la pagina
    for (intento in 1:3) {
      r <- tryCatch(httr::GET(u, httr::user_agent(ua), httr::timeout(30)), error = function(e) NULL)
      if (!is.null(r) && httr::status_code(r) == 200) {
        d <- .ipsa_dm_parse(httr::content(r, "text", encoding = "UTF-8"))
        if (!is.null(d) && nrow(d)) return(d)
      }
      Sys.sleep(1.5 * intento)
    }
    NULL
  }
  urls <- base                               # base = dato del ultimo dia disponible
  if (!is.null(desde)) {
    meses <- seq(lubridate::floor_date(as.Date(desde), "month"),
                 lubridate::floor_date(as.Date(hasta), "month"), by = "month")
    urls <- c(base, paste0(base, "?dr=", format(meses, "%Y-%m")))
  }
  df <- dplyr::bind_rows(lapply(urls, get1))
  if (is.null(df) || !nrow(df)) { message("    Sin datos IPSA de datosmacro"); return(NULL) }
  df <- df %>% dplyr::filter(!is.na(fecha), !is.na(valor), !(lubridate::wday(fecha) %in% c(1, 7))) %>%
    dplyr::distinct(fecha, .keep_all = TRUE) %>% dplyr::arrange(fecha)
  message("    ✓ IPSA datosmacro: ", nrow(df), " dias (",
          format(min(df$fecha)), " -> ", format(max(df$fecha)), ")")
  df
}

# ---- Guard anti-basura del IPSA ----
# La fuente (datosmacro) a veces sirve valores corruptos (~la mitad del indice,
# p.ej. 5480 cuando el IPSA real es ~10800). Un indice NO se mueve >15% en un dia,
# asi que descartamos cualquier punto cuyo salto dia-a-dia respecto al ULTIMO
# valor RETENIDO supere el umbral. Se escanea hacia adelante desde un ancla buena
# (el historico/cierre 2025), de modo que un outlier no arrastre a los siguientes
# ni congele la serie de forma permanente si la fuente se recupera.
filtrar_saltos_ipsa <- function(s, max_salto = 0.15) {
  if (is.null(s) || nrow(s) < 2) return(s)
  s <- s[order(s$fecha), , drop = FALSE]
  keep <- rep(TRUE, nrow(s)); ref <- NA_real_
  for (i in seq_len(nrow(s))) {
    v <- s$valor[i]
    if (is.na(v) || v <= 0) { keep[i] <- FALSE; next }
    if (is.na(ref)) { ref <- v; next }                 # primer valor valido = ancla
    if (abs(v / ref - 1) > max_salto) keep[i] <- FALSE # salto imposible -> descartar, ref se mantiene
    else ref <- v
  }
  n_out <- sum(!keep)
  if (n_out > 0) message("    IPSA: descartados ", n_out, " punto(s) por salto > ",
                         round(max_salto * 100), "% (fuente corrupta): ",
                         paste(format(s$fecha[!keep]), round(s$valor[!keep], 1), collapse = ", "))
  s[keep, , drop = FALSE]
}

# Ancla del ultimo IPSA via Yahoo (^IPSA): 1 punto (valor del dia), sin historia.
# Sirve como valor fresco cuando datosmacro entrega basura en el ultimo dia.
obtener_ipsa_yahoo_actual <- function() {
  s <- tryCatch(obtener_historico_yahoo_json("^IPSA", Sys.Date() - 10, Sys.Date()),
                error = function(e) NULL)
  if (is.null(s) || !nrow(s)) return(NULL)
  s <- s %>% filter(!is.na(valor), valor > 0) %>% arrange(fecha)
  if (!nrow(s)) return(NULL)
  message("    ✓ IPSA Yahoo (ancla): ", format(max(s$fecha)), " = ", round(tail(s$valor, 1), 1))
  s
}

# ---- USD observado (BCCh web) ----
obtener_usdclp_web <- function(fechas_objetivo) {
  message("  Descargando: USD observado (BCCh - Web Scraping)")
  
  url <- "https://si3.bcentral.cl/siete/ES/Siete/Cuadro/CAP_TIPO_CAMBIO/MN_TIPO_CAMBIO4/DOLAR_OBS_ADO"
  
  historico <- tryCatch({
    resp <- httr::GET(url, 
                      httr::timeout(20),
                      httr::add_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36"))
    
    if (httr::status_code(resp) != 200) {
      message("    HTTP Error: ", httr::status_code(resp))
      return(NULL)
    }
    
    page <- rvest::read_html(resp)
    headers <- page %>% html_nodes("table thead th") %>% html_text(trim = TRUE)
    if (length(headers) < 4) { message("    No se encontraron encabezados de fecha"); return(NULL) }
    headers_fechas <- headers[4:length(headers)]
    filas <- page %>% html_nodes("table tbody tr")
    if (length(filas) == 0) { message("    No se encontraron filas de datos"); return(NULL) }
    fila_1 <- filas[1]
    celdas <- fila_1 %>% html_nodes("td") %>% html_text(trim = TRUE)
    if (length(celdas) < 4) { message("    Primera fila incompleta"); return(NULL) }
    valores_text <- celdas[4:length(celdas)]
    valores_num <- suppressWarnings(as.numeric(gsub("\\.", "", valores_text) %>% gsub(",", ".", .)))
    valores_num_clean <- valores_num[!is.na(valores_num) & nzchar(valores_text)]
    headers_fechas_clean <- headers_fechas[!is.na(valores_num)]
    if (length(valores_num_clean) == 0) { message("    No hay valores numéricos válidos"); return(NULL) }
    
    meses_es <- c("ene"="01","feb"="02","mar"="03","abr"="04","may"="05","jun"="06",
                  "jul"="07","ago"="08","sep"="09","oct"="10","nov"="11","dic"="12")
    
    fechas_convertidas <- sapply(headers_fechas_clean, function(fecha_str) {
      fecha_lower <- tolower(fecha_str)
      partes <- strsplit(fecha_lower, "\\.")[[1]]
      if (length(partes) == 3) {
        mes <- meses_es[partes[2]]
        if (!is.na(mes)) return(as.Date(paste(partes[3], mes, partes[1], sep = "-")))
      }
      NA_Date_
    })
    
    tibble::as_tibble(list(fecha = as.Date(fechas_convertidas), usdclp = valores_num_clean)) %>%
      filter(!is.na(fecha), !is.na(usdclp)) %>% arrange(fecha)
    
  }, error = function(e) { message("    Error: ", e$message); NULL })
  
  if (is.null(historico) || nrow(historico) == 0) {
    message("    Sin datos para USD observado")
    return(tibble(fecha = as.Date(character()), usdclp = numeric()))
  }
  message("    ✓ USD observado OK: ", nrow(historico), " registros (",
          format(min(historico$fecha)), " -> ", format(max(historico$fecha)), ")")
  historico
}

# ---- Yahoo Finance + wrappers de indices ----
obtener_historico_yahoo_json <- function(ticker, desde, hasta) {
  dias      <- as.numeric(hasta - desde) + 30
  range_str <- if (dias <= 90) "3mo" else if (dias <= 180) "6mo" else if (dias <= 365) "1y" else "2y"
  url  <- paste0("https://query1.finance.yahoo.com/v8/finance/chart/", URLencode(ticker, reserved = TRUE))
  resp <- tryCatch(
    GET(url, query = list(interval = "1d", range = range_str),
        add_headers("User-Agent" = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36"), timeout(30)),
    error = function(e) { message("    Error: ", e$message); NULL }
  )
  if (is.null(resp) || status_code(resp) != 200) return(NULL)
  datos <- tryCatch(content(resp, "text", encoding = "UTF-8") %>% fromJSON(), error = function(e) NULL)
  if (is.null(datos$chart$result)) return(NULL)
  timestamps <- datos$chart$result$timestamp[[1]]
  cierres    <- datos$chart$result$indicators$quote[[1]]$close[[1]]
  if (is.null(timestamps) || is.null(cierres)) return(NULL)
  tibble(fecha = as.Date(as.POSIXct(timestamps, origin = "1970-01-01", tz = "UTC")),
         valor = cierres) %>% filter(!is.na(valor)) %>% arrange(fecha)
}

obtener_ipsa_bcch <- function(fechas_objetivo) { obtener_ipsa_web(fechas_objetivo) }
obtener_fx_bcch   <- function(fechas_objetivo) { obtener_usdclp_web(fechas_objetivo) }

fx_proximo_dia_habil <- function(fecha, fx_df) {
  if (is.null(fx_df) || nrow(fx_df) == 0) return(NA_real_)
  posteriores <- fx_df %>% filter(fecha > !!fecha) %>% arrange(fecha)
  if (nrow(posteriores) == 0) return(NA_real_)
  posteriores$usdclp[1]
}

obtener_indice_yahoo <- function(indice, fechas_objetivo) {
  if (!is.null(indice$fuente) && indice$fuente == "bcch") return(obtener_ipsa_bcch(fechas_objetivo))
  if (is.null(indice$ticker) || nchar(indice$ticker) == 0) {
    message("  ", indice$nombre, ": sin ticker, columna vacia")
    return(tibble(fecha = fechas_objetivo, valor = NA_real_))
  }
  message("  Descargando: ", indice$nombre, " (", indice$ticker, ")")
  desde <- min(fechas_objetivo) - 180
  hasta <- max(fechas_objetivo)

  bajar_quantmod <- function() {
    env_local <- new.env()
    out <- tryCatch({
      suppressWarnings(getSymbols(indice$ticker, src = "yahoo", from = format(desde, "%Y-%m-%d"), auto.assign = TRUE, env = env_local))
      vars <- ls(envir = env_local)
      if (length(vars) == 0) NULL else get(vars[1], envir = env_local)
    }, error = function(e) { message("    quantmod error: ", e$message); NULL })
    if (is.null(out) || nrow(out) == 0) return(NULL)
    cierres <- tryCatch(as.numeric(Cl(out)), error = function(e) as.numeric(out[, ncol(out)]))
    df <- tibble(fecha = as.Date(index(out)), valor = cierres) %>% filter(!is.na(valor)) %>% arrange(fecha)
    if (nrow(df) == 0) NULL else df
  }

  bajar_json <- function() {
    df <- tryCatch(obtener_historico_yahoo_json(indice$ticker, desde, hasta), error = function(e) { message("    JSON error: ", e$message); NULL })
    if (is.null(df) || nrow(df) == 0) NULL else df
  }

  historico <- bajar_quantmod()
  if (!is.null(historico)) {
    message("    quantmod OK: ", nrow(historico), " filas (", format(min(historico$fecha)), " -> ", format(max(historico$fecha)), ")")
  } else {
    message("    quantmod sin datos, probando JSON...")
    historico <- bajar_json()
    if (!is.null(historico)) message("    JSON OK: ", nrow(historico), " filas (", format(min(historico$fecha)), " -> ", format(max(historico$fecha)), ")")
  }

  if (is.null(historico) || nrow(historico) == 0) { message("    Sin datos"); return(tibble(fecha = fechas_objetivo, valor = NA_real_)) }
  message("    Registros: ", nrow(historico))
  resultados <- lapply(fechas_objetivo, function(f) {
    exacto <- historico %>% filter(fecha == f)
    if (nrow(exacto) > 0) return(tibble(fecha = f, valor = exacto$valor[1]))
    previo <- historico %>% filter(fecha <= f) %>% arrange(desc(fecha)) %>% head(1)
    if (nrow(previo) > 0) return(tibble(fecha = f, valor = previo$valor[1]))
    tibble(fecha = f, valor = NA_real_)
  })
  bind_rows(resultados)
}

# ---- Treasury 10y (^TNX) ----
obtener_treasury_10y <- function(fecha_referencia = Sys.Date()) {
  desde <- as.Date(paste0(year(fecha_referencia) - 1, "-12-01"))
  hasta <- fecha_referencia + 1
  message("  Descargando Treasury 10y (^TNX) desde Yahoo...")
  datos_xts <- tryCatch(
    getSymbols("^TNX", src = "yahoo", from = format(desde, "%Y-%m-%d"), to = format(hasta, "%Y-%m-%d"), auto.assign = FALSE),
    error = function(e) { message("    quantmod error: ", e$message); NULL }
  )
  historico <- NULL
  if (!is.null(datos_xts) && nrow(datos_xts) > 0)
    historico <- tibble(fecha = as.Date(index(datos_xts)), valor = as.numeric(Cl(datos_xts))) %>% filter(!is.na(valor)) %>% arrange(fecha)
  if (is.null(historico) || nrow(historico) == 0)
    historico <- tryCatch(obtener_historico_yahoo_json("^TNX", desde, fecha_referencia), error = function(e) NULL)
  if (is.null(historico) || nrow(historico) == 0) { message("    Sin datos para ^TNX"); return(NULL) }
  message("    Registros ^TNX: ", nrow(historico))
  historico
}
