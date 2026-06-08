# =============================================================================
# ALMACEN DE DATOS (local + GitHub)
# En la nube no hay disco permanente: los inputs editables (dividendos,
# manuales, correcciones) se commitean al repo de GitHub via REST Contents API.
#
# Config por variables de entorno:
#   GITHUB_TOKEN  -> PAT con permiso 'contents:write' al repo
#   GH_REPO       -> "usuario/repositorio"
#   GH_BRANCH     -> rama (default "main")
#   PULSO_DATA_URL-> (opcional) base raw para LEER, ej:
#                    https://raw.githubusercontent.com/usuario/repo/main
#
# Si no hay token/repo, todo funciona en local: lee y escribe en data/.
# =============================================================================
suppressMessages({ library(httr); library(jsonlite) })

.gh <- function() list(
  token  = Sys.getenv("GITHUB_TOKEN", ""),
  repo   = Sys.getenv("GH_REPO", ""),
  branch = Sys.getenv("GH_BRANCH", "main"),
  raw    = Sys.getenv("PULSO_DATA_URL", "")
)
gh_habilitado <- function() { g <- .gh(); nzchar(g$token) && nzchar(g$repo) }

# ---- LECTURA ----
# Lee un archivo de data/ . En la nube intenta primero la URL raw de GitHub.
store_path <- function(rel) file.path("data", rel)

store_existe <- function(rel) {
  if (file.exists(store_path(rel))) return(TRUE)
  g <- .gh(); if (!nzchar(g$raw)) return(FALSE)
  resp <- tryCatch(HEAD(paste0(g$raw, "/data/", rel), timeout(15)), error = function(e) NULL)
  !is.null(resp) && status_code(resp) == 200
}

#' Descarga un archivo del repo (raw) a data/ local si no existe localmente.
store_sync <- function(rel) {
  local <- store_path(rel)
  g <- .gh()
  if (nzchar(g$raw)) {
    resp <- tryCatch(GET(paste0(g$raw, "/data/", rel), timeout(30)), error = function(e) NULL)
    if (!is.null(resp) && status_code(resp) == 200) {
      dir.create(dirname(local), showWarnings = FALSE, recursive = TRUE)
      writeBin(content(resp, "raw"), local)
    }
  }
  if (file.exists(local)) local else NULL
}

# ---- ESCRITURA ----
#' Guarda 'raw' (vector raw) en data/<rel> local y, si GitHub esta configurado,
#' lo commitea al repo. Devuelve un mensaje de estado.
store_write_bin <- function(rel, raw, mensaje = NULL) {
  local <- store_path(rel)
  dir.create(dirname(local), showWarnings = FALSE, recursive = TRUE)
  writeBin(raw, local)
  if (!gh_habilitado()) return(paste0("Guardado local: ", local, " (GitHub no configurado)"))

  g <- .gh()
  api <- paste0("https://api.github.com/repos/", g$repo, "/contents/data/", rel)
  hdr <- add_headers(Authorization = paste("token", g$token),
                     Accept = "application/vnd.github+json",
                     "User-Agent" = "pulso-vcc-cloud")
  # sha actual (si existe) para actualizar en vez de crear
  sha <- NULL
  r0 <- tryCatch(GET(api, hdr, query = list(ref = g$branch), timeout(20)), error = function(e) NULL)
  if (!is.null(r0) && status_code(r0) == 200)
    sha <- content(r0, "parsed")$sha
  body <- list(message = mensaje %||% paste("Actualizar data/", rel),
               content = jsonlite::base64_enc(raw), branch = g$branch)
  if (!is.null(sha)) body$sha <- sha
  r1 <- tryCatch(PUT(api, hdr, body = toJSON(body, auto_unbox = TRUE), timeout(30)),
                 error = function(e) NULL)
  if (!is.null(r1) && status_code(r1) %in% c(200, 201))
    paste0("Commit OK a GitHub: data/", rel)
  else
    paste0("Guardado local OK, pero FALLO el commit a GitHub (HTTP ",
           if (!is.null(r1)) status_code(r1) else "NULL", ").")
}

store_write_text <- function(rel, texto, mensaje = NULL)
  store_write_bin(rel, charToRaw(enc2utf8(paste(texto, collapse = "\n"))), mensaje)

store_write_file <- function(rel, ruta_local_origen, mensaje = NULL)
  store_write_bin(rel, readBin(ruta_local_origen, "raw", file.info(ruta_local_origen)$size), mensaje)

if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a
