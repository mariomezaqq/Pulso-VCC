# =============================================================================
# FOLLETOS INFORMATIVOS (pestania = 68 de la CMF)
# Descarga el Folleto Informativo (PDF) de cada fondo/serie y extrae campos
# estructurados: TAC, duracion, moneda, administradora, patrimonio, fecha cierre.
#
# Flujo descubierto en la CMF (no requiere reCAPTCHA, a diferencia del valor cuota):
#   1) GET  entidad.php?...&pestania=68         -> lista folletos; de ahi sale el
#                                                  rutAdmin (RUT de la AGF) por serie.
#   2) POST inc/ver_folleto_fm.php {runFondo, serie, rutAdmin}
#                                               -> devuelve la URL del PDF (o 'ERROR').
#   3) GET  <url PDF>                           -> el PDF del folleto.
#
# Todas las funciones logean con message() igual que scraper.R.
# =============================================================================
suppressMessages({ library(httr); library(rvest); library(stringr); library(pdftools) })

if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

.CMF_BASE <- "https://www.cmfchile.cl"
.UA <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/145.0.0.0 Safari/537.36"

# En Windows la CMF a veces cierra la conexion sin close_notify y schannel lo
# reporta como error al REUSAR la conexion. Forzamos conexion fresca por request
# (fresh_connect/forbid_reuse) y reintentamos ante errores transitorios de red.
.cmf_cfg <- httr::config(fresh_connect = 1, forbid_reuse = 1)

.cmf_request <- function(verb, url, ..., intentos = 3) {
  for (k in seq_len(intentos)) {
    resp <- tryCatch(
      if (identical(verb, "POST")) httr::POST(url, .cmf_cfg, ...) else httr::GET(url, .cmf_cfg, ...),
      error = function(e) { message("[folletos] ", verb, " intento ", k, "/", intentos, " err: ", e$message); NULL })
    if (!is.null(resp)) return(resp)
    Sys.sleep(0.5 * k)
  }
  NULL
}

#' URL de la pestania 68 (folletos) de un fondo.
folleto_url_pestania68 <- function(run, tipoentidad, row) {
  # OJO: en pulso los `row` ya vienen URL-safe (con "+" o "%20" para espacios,
  # igual que los usa R/scraper.R), asi que NO se re-encodean (URLencode los
  # romperia: "+" -> "%2B"). Se usan tal cual, como el scraper de valor cuota.
  paste0(.CMF_BASE, "/institucional/mercados/entidad.php",
         "?mercado=V&rut=", run, "&grupo=&tipoentidad=", tipoentidad,
         if (nchar(row %||% "") > 0) paste0("&row=", row) else "",
         "&vig=VI&control=svs&pestania=68")
}

#' Lee la pestania 68 y devuelve, por serie, el rutAdmin y (opcional) el numero
#' de documento del folleto vigente. Es una sola peticion por fondo (run).
#'
#' @return data.frame(serie, rutAdmin) o NULL si no hay folletos.
folleto_series_disponibles <- function(run, tipoentidad, row, cookies = "") {
  url <- folleto_url_pestania68(run, tipoentidad, row)
  message("[folletos] pestania68: ", url)
  resp <- .cmf_request("GET", url, add_headers("User-Agent" = .UA, "Cookie" = cookies), timeout(40))
  if (is.null(resp) || status_code(resp) != 200) {
    message("[folletos] HTTP pestania68: ", if (!is.null(resp)) status_code(resp) else "NULL"); return(NULL)
  }
  html <- content(resp, "text", encoding = "UTF-8")
  # verFolleto('run','serie','rutAdmin')  -> el vigente de cada serie
  m <- str_match_all(html, "verFolleto\\('([^']+)','([^']+)','([^']+)'\\)")[[1]]
  if (nrow(m) == 0) { message("[folletos] sin verFolleto() en la pagina (fondo sin folleto?)"); return(NULL) }
  df <- unique(data.frame(serie = m[, 3], rutAdmin = m[, 4], stringsAsFactors = FALSE))
  message("[folletos] series con folleto: ", paste(df$serie, collapse = " | "))
  df
}

#' POST a ver_folleto_fm.php -> URL del PDF del folleto (vigente si numeroDocumento=NULL).
folleto_url_pdf <- function(run, serie, rutAdmin, numeroDocumento = NULL, referer = NULL, cookies = "") {
  body <- list(runFondo = as.character(run), serie = as.character(serie), rutAdmin = as.character(rutAdmin))
  if (!is.null(numeroDocumento)) body$numeroDocumento <- as.character(numeroDocumento)
  resp <- .cmf_request("POST", paste0(.CMF_BASE, "/institucional/inc/ver_folleto_fm.php"),
                       add_headers("User-Agent" = .UA, "X-Requested-With" = "XMLHttpRequest",
                                   "Origin" = .CMF_BASE, "Referer" = referer %||% .CMF_BASE,
                                   "Cookie" = cookies),
                       body = body, encode = "form", timeout(40))
  if (is.null(resp) || status_code(resp) != 200) return(NULL)
  data <- str_trim(content(resp, "text", encoding = "UTF-8"))
  if (nchar(data) == 0 || toupper(data) == "ERROR") {
    message("[folletos] ver_folleto_fm devolvio ERROR (sin folleto para serie ", serie, ")"); return(NULL)
  }
  if (!grepl("^http", data)) data <- paste0(.CMF_BASE, data)
  data
}

#' Descarga el PDF del folleto vigente de un fondo/serie a 'dir'.
#' @return ruta local al PDF, o NULL.
descargar_folleto <- function(run, serie, tipoentidad, row, dir = "data/folletos",
                              rutAdmin = NULL, cookies = "") {
  dir.create(dir, showWarnings = FALSE, recursive = TRUE)
  ref <- folleto_url_pestania68(run, tipoentidad, row)

  if (is.null(rutAdmin)) {
    disp <- folleto_series_disponibles(run, tipoentidad, row, cookies)
    if (is.null(disp)) return(NULL)
    fila <- disp[disp$serie == serie, , drop = FALSE]
    if (nrow(fila) == 0) {
      message("[folletos] serie '", serie, "' no tiene folleto; disponibles: ",
              paste(disp$serie, collapse = " | "))
      # Algunos fondos publican una sola serie: usar la primera como fallback
      fila <- disp[1, , drop = FALSE]
    }
    rutAdmin <- fila$rutAdmin[1]
  }

  pdf_url <- folleto_url_pdf(run, serie, rutAdmin, referer = ref, cookies = cookies)
  if (is.null(pdf_url)) return(NULL)
  message("[folletos] PDF URL: ", pdf_url)

  dest <- file.path(dir, paste0(run, "_", gsub("[^A-Za-z0-9]", "", serie), ".pdf"))
  rp <- .cmf_request("GET", pdf_url, add_headers("User-Agent" = .UA, "Referer" = ref, "Cookie" = cookies),
                     write_disk(dest, overwrite = TRUE), timeout(60))
  if (is.null(rp) || status_code(rp) != 200) return(NULL)
  ct <- headers(rp)[["content-type"]] %||% ""
  if (!grepl("pdf", ct, ignore.case = TRUE)) {
    message("[folletos] respuesta no es PDF (content-type: ", ct, ")"); return(NULL)
  }
  message("[folletos] guardado: ", dest, " (", file.info(dest)$size, " bytes)")
  dest
}

# ---- EXTRACCION DE CAMPOS DESDE EL PDF -------------------------------------

#' Extrae el TAC (%) del texto del folleto. Cubre dos formatos:
#'   "TAC Serie (IVA incluido) 4,38%"   y   "TAC Serie 1 2,21%"
#' Devuelve el numero (ej. 4.38) o NA.
extraer_tac <- function(texto) {
  m <- str_match(texto, "(?i)TAC\\s*Serie[^\\n]*?(\\d+(?:[.,]\\d+)?)\\s*%")
  if (is.na(m[1, 2])) return(NA_real_)
  as.numeric(gsub(",", ".", m[1, 2]))
}

#' Extrae campos estructurados de un PDF de folleto informativo.
#' @return list(nombre, serie, fecha_cierre, administradora, moneda, duracion, patrimonio, tac, n_paginas)
extraer_campos_folleto <- function(pdf_path) {
  if (!file.exists(pdf_path)) return(NULL)
  txt <- tryCatch(pdf_text(pdf_path), error = function(e) { message("[folletos] pdf_text err: ", e$message); NULL })
  if (is.null(txt)) return(NULL)
  full <- paste(txt, collapse = "\n")
  norm <- function(x) str_trim(gsub("[ \\t]{2,}", " ", x %||% NA_character_))

  cab <- str_match(full, "(?s)^\\s*(.+?)\\s*[|\\-]\\s*[Ss]erie\\s*([^\\n]+)")
  fecha <- str_match(full, "(?i)Folleto Informativo al cierre de\\s*([^\\n]+)")[, 2]
  admin <- str_match(full, "(?i)(?:Administradora[^\\n]*\\n\\s*)([A-ZÁÉÍÓÚÑ][^\\n]{4,80})")[, 2]
  dur   <- str_match(full, "(?i)Duraci[oó]n[:\\s]*([A-Za-z0-9][^\\n]{0,40})")[, 2]
  mon   <- str_match(full, "(?i)Moneda Nacional|Pesos|D[oó]lares|UF")[, 1]

  list(
    nombre        = norm(cab[, 2]),
    serie         = norm(cab[, 3]),
    fecha_cierre  = norm(fecha),
    administradora= norm(admin),
    moneda        = norm(mon),
    duracion      = norm(dur),
    tac           = extraer_tac(full),
    n_paginas     = length(txt)
  )
}

# ---- INDICE DE TAC (para el dashboard) -------------------------------------

#' Carga el indice de TAC generado por scripts/batch_tac.R.
#' @return data.frame con run, serie, tipoentidad, tac (numerico %), ... o NULL.
cargar_indice_tac <- function(ruta = "data/folletos_index.csv") {
  if (!file.exists(ruta)) return(NULL)
  idx <- tryCatch(read.csv(ruta, stringsAsFactors = FALSE,
                           colClasses = c(run = "character", serie = "character")),
                  error = function(e) NULL)
  if (is.null(idx) || !"tac" %in% names(idx)) return(NULL)
  idx$tac <- suppressWarnings(as.numeric(idx$tac))
  idx
}

#' TAC (% numerico) de un fondo/serie desde el indice cargado. NA si no esta.
tac_de_fondo_idx <- function(indice, run, serie) {
  if (is.null(indice)) return(NA_real_)
  fila <- indice[indice$run == as.character(run) & indice$serie == as.character(serie), , drop = FALSE]
  if (nrow(fila) == 0) return(NA_real_)
  fila$tac[1]
}

#' Estado del folleto de un fondo/serie en el indice ("ok","sin_tac","sin_folleto",...).
#' NA si el fondo no esta en el indice. Sirve para decidir si ofrecer la descarga.
folleto_estado_idx <- function(indice, run, serie) {
  if (is.null(indice) || !"estado" %in% names(indice)) return(NA_character_)
  fila <- indice[indice$run == as.character(run) & indice$serie == as.character(serie), , drop = FALSE]
  if (nrow(fila) == 0) return(NA_character_)
  fila$estado[1]
}

#' Alto nivel: baja el folleto de un fondo y extrae el TAC (+ campos).
#' Cachea el PDF en 'dir'; si ya existe y reusar=TRUE, no lo vuelve a bajar.
#' @param fondo lista/fila con run, serie, tipoentidad, row
#' @return list de campos (incluye $tac) o NULL.
obtener_folleto_fondo <- function(fondo, dir = "data/folletos", cookies = "", reusar = TRUE) {
  dest <- file.path(dir, paste0(fondo$run, "_", gsub("[^A-Za-z0-9]", "", fondo$serie), ".pdf"))
  if (!(reusar && file.exists(dest) && file.info(dest)$size > 1000)) {
    dest <- descargar_folleto(fondo$run, fondo$serie, fondo$tipoentidad, fondo$row %||% "",
                              dir = dir, cookies = cookies)
    if (is.null(dest)) return(NULL)
  }
  extraer_campos_folleto(dest)
}
