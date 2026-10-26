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

# Repo y URL de datos NO son secretos (el repo es público) -> van como default.
# El token (para escribir) es secreto: solo via env var / R/secret_token.R.
.gh <- function() list(
  token  = Sys.getenv("GITHUB_TOKEN", ""),
  repo   = Sys.getenv("GH_REPO", "mariomezaqq/Pulso-VCC"),
  branch = Sys.getenv("GH_BRANCH", "main"),
  raw    = Sys.getenv("PULSO_DATA_URL", "https://raw.githubusercontent.com/mariomezaqq/Pulso-VCC/main")
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

#' Lee un archivo SIEMPRE fresco desde la API de contenidos de GitHub (a
#' diferencia de store_sync/raw.githubusercontent.com, que es un CDN cacheado
#' ~5 min: releer por ahi justo despues de guardar puede traer la version
#' VIEJA y, al fusionar y volver a guardar, borrar lo recien commiteado).
#' Solo funciona si hay token (si no, NULL -> el llamador cae a store_sync).
#' Pide el contenido CRUDO (Accept: vnd.github.raw): sirve hasta 100 MB. Con el
#' JSON + base64 de antes, GitHub devuelve content vacio sobre 1 MB y se
#' escribia un archivo vacio (le pasaba a dividendos.xlsx).
#' @param ref commit/rama a leer (default: la rama configurada).
#' @return vector raw (bytes) del archivo, o NULL si no se pudo leer.
store_read_fresh_raw <- function(rel, ref = NULL) {
  g <- .gh(); if (!nzchar(g$token) || !nzchar(g$repo)) return(NULL)
  api <- paste0("https://api.github.com/repos/", g$repo, "/contents/data/", rel)
  r <- tryCatch(GET(api, .gh_hdr(g, "application/vnd.github.raw"),
                    query = list(ref = ref %||% g$branch), timeout(60)),
                error = function(e) NULL)
  if (is.null(r) || status_code(r) != 200) return(NULL)
  content(r, "raw")
}

.gh_hdr <- function(g, accept = "application/vnd.github+json")
  add_headers(Authorization = paste("token", g$token), Accept = accept,
              "User-Agent" = "pulso-vcc-cloud")

#' Igual que store_read_fresh_raw() pero devuelve texto (character). Usar solo
#' para archivos de texto (CSV); para binarios (.rds, .xlsx) usar la version raw.
store_read_fresh_text <- function(rel) {
  raw <- store_read_fresh_raw(rel)
  if (is.null(raw)) return(NULL)
  tryCatch(rawToChar(raw), error = function(e) NULL)
}

#' Como store_sync(), pero via la API de contenidos de GitHub (sin el cache
#' de CDN de raw.githubusercontent.com). Usar para archivos que se editan y
#' se vuelven a renderizar en la misma sesion (ej. fondos_curados.csv desde
#' el panel admin, o series_vc.rds justo despues de que el cron termino): con
#' store_sync, el primer render inmediatamente despues de guardar puede pisar
#' el archivo recien escrito con la version vieja que el CDN todavia tiene
#' cacheada, revirtiendo silenciosamente la edicion. Binary-safe (usa bytes
#' crudos, nunca pasa por texto) para que sirva igual con .rds/.xlsx que con .csv.
store_sync_fresh <- function(rel) {
  local <- store_path(rel)
  raw <- store_read_fresh_raw(rel)
  if (is.null(raw)) return(store_sync(rel))  # sin token/error -> cae a CDN/local
  dir.create(dirname(local), showWarnings = FALSE, recursive = TRUE)
  writeBin(raw, local)
  if (file.exists(local)) local else NULL
}

# ---- SINCRONIZACION POR VERSION (datos al dia sin reiniciar la app) ----
# La app mira cada ~30-60 s el ultimo commit de la rama (1 llamada barata). Solo
# si cambio, baja los archivos de data/ cuyo blob cambio, leidos EN ESE commit
# (ref = sha): no mezcla versiones ni depende del cache de CDN. El estado es del
# proceso R, asi que lo comparten todas las sesiones abiertas.
ARCHIVOS_SYNC <- c("series_vc.rds", "fondos_curados.csv", "dividendos.xlsx",
                   "dividendos_calculados.csv")
.sync <- new.env()
.sync$sha <- NA_character_          # version ya bajada a data/
.sync$sha_remoto <- NA_character_   # ultima version vista en GitHub
.sync$t <- 0                        # cuando se consulto GitHub por ultima vez
.sync$blobs <- list()               # sha del blob de cada archivo bajado

#' Version actual de los datos en GitHub (sha del ultimo commit de la rama),
#' consultada como mucho una vez cada `cada` segundos por proceso.
#' Sin token (uso local): cambia cada 5 min y se re-sincroniza por CDN.
version_datos <- function(cada = 30) {
  g <- .gh()
  if (!nzchar(g$token)) return(as.character(as.numeric(Sys.time()) %/% 300))
  if (!is.na(.sync$sha_remoto) && as.numeric(Sys.time()) - .sync$t < cada) return(.sync$sha_remoto)
  r <- tryCatch(GET(sprintf("https://api.github.com/repos/%s/commits/%s", g$repo, g$branch),
                    .gh_hdr(g, "application/vnd.github.sha"), timeout(10)),
                error = function(e) NULL)
  .sync$t <- as.numeric(Sys.time())
  if (!is.null(r) && status_code(r) == 200)
    .sync$sha_remoto <- trimws(content(r, "text", encoding = "UTF-8"))
  .sync$sha_remoto
}

#' Baja a data/ los archivos de ARCHIVOS_SYNC que cambiaron desde la ultima
#' sincronizacion. Devuelve sus nombres (character(0) si no cambio nada). Si una
#' descarga falla, no marca la version: se reintenta en la proxima vuelta.
sincronizar_datos <- function() {
  g <- .gh(); v <- version_datos()
  if (is.na(v) || identical(v, .sync$sha)) return(character(0))
  if (!nzchar(g$token)) {
    for (f in ARCHIVOS_SYNC) store_sync(f)
    .sync$sha <- v
    return(ARCHIVOS_SYNC)
  }
  r <- tryCatch(GET(sprintf("https://api.github.com/repos/%s/contents/data", g$repo),
                    .gh_hdr(g), query = list(ref = v), timeout(15)),
                error = function(e) NULL)
  if (is.null(r) || status_code(r) != 200) return(character(0))
  lista <- content(r, "parsed")
  blobs <- setNames(lapply(lista, `[[`, "sha"), vapply(lista, `[[`, "", "name"))
  cambiados <- character(0)
  for (f in intersect(ARCHIVOS_SYNC, names(blobs))) {
    if (identical(.sync$blobs[[f]], blobs[[f]]) && file.exists(store_path(f))) next
    raw <- store_read_fresh_raw(f, ref = v)
    if (is.null(raw)) return(cambiados)
    dir.create(dirname(store_path(f)), showWarnings = FALSE, recursive = TRUE)
    writeBin(raw, store_path(f))
    .sync$blobs[[f]] <- blobs[[f]]
    cambiados <- c(cambiados, f)
  }
  .sync$sha <- v
  cambiados
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
    return(paste0("Commit OK a GitHub: data/", rel))
  # La API de contenidos falla con archivos de algunos MB (HTTP 500, le pasaba
  # al boletin) o si otro commit cambio el archivo entretanto (409). Respaldo:
  # commit por la Git Data API (blob + tree + commit), que acepta hasta 100 MB.
  r2 <- .commit_git_data(g, paste0("data/", rel), raw, body$message)
  if (isTRUE(r2$ok)) return(paste0("Commit OK a GitHub: data/", rel))
  paste0("Guardado local OK, pero FALLO el commit a GitHub (HTTP ",
         if (!is.null(r1)) status_code(r1) else "NULL", "; respaldo: ", r2$msg, ").")
}

#' Commit de un archivo via Git Data API: blob -> tree -> commit -> mover la rama.
#' Si otro commit entra entremedio (la rama no avanza en fast-forward), reintenta
#' sobre el commit nuevo, hasta `intentos` veces. Devuelve list(ok, msg).
.commit_git_data <- function(g, path, raw, mensaje, intentos = 3) {
  base <- sprintf("https://api.github.com/repos/%s/git", g$repo)
  hdr <- .gh_hdr(g)
  pedir <- function(verbo, url, body = NULL) {
    r <- tryCatch(VERB(verbo, url, hdr, timeout(120),
                       body = if (!is.null(body)) toJSON(body, auto_unbox = TRUE)),
                  error = function(e) NULL)
    if (is.null(r) || status_code(r) >= 300)
      return(list(ok = FALSE, msg = paste(verbo, sub(".*/git/", "", url), "HTTP",
                                          if (is.null(r)) "sin respuesta" else status_code(r))))
    list(ok = TRUE, d = content(r, "parsed"))
  }
  blob <- pedir("POST", paste0(base, "/blobs"),
                list(content = jsonlite::base64_enc(raw), encoding = "base64"))
  if (!blob$ok) return(blob)
  for (i in seq_len(intentos)) {
    ref <- pedir("GET", paste0(base, "/ref/heads/", g$branch)); if (!ref$ok) return(ref)
    padre <- ref$d$object$sha
    com <- pedir("GET", paste0(base, "/commits/", padre)); if (!com$ok) return(com)
    tree <- pedir("POST", paste0(base, "/trees"), list(base_tree = com$d$tree$sha,
      tree = list(list(path = path, mode = "100644", type = "blob", sha = blob$d$sha))))
    if (!tree$ok) return(tree)
    nuevo <- pedir("POST", paste0(base, "/commits"),
                   list(message = mensaje, tree = tree$d$sha, parents = list(padre)))
    if (!nuevo$ok) return(nuevo)
    mov <- pedir("PATCH", paste0(base, "/refs/heads/", g$branch), list(sha = nuevo$d$sha, force = FALSE))
    if (mov$ok) return(list(ok = TRUE, msg = "ok"))
    Sys.sleep(1)   # la rama avanzo (ej. commit del cron): reintentar sobre el nuevo
  }
  mov
}

store_write_text <- function(rel, texto, mensaje = NULL)
  store_write_bin(rel, charToRaw(enc2utf8(paste(texto, collapse = "\n"))), mensaje)

store_write_file <- function(rel, ruta_local_origen, mensaje = NULL)
  store_write_bin(rel, readBin(ruta_local_origen, "raw", file.info(ruta_local_origen)$size), mensaje)

# Dispara el workflow del cron (re-scrapea la CMF ahora). Devuelve list(ok, msg).
gh_dispatch <- function(workflow = "refresh.yml") {
  g <- .gh()
  if (!nzchar(g$token)) return(list(ok = FALSE, msg = "No hay token de GitHub configurado."))
  r <- tryCatch(POST(
    sprintf("https://api.github.com/repos/%s/actions/workflows/%s/dispatches", g$repo, workflow),
    add_headers(Authorization = paste("token", g$token), Accept = "application/vnd.github+json",
                "User-Agent" = "pulso-vcc"),
    body = sprintf('{"ref":"%s"}', g$branch)), error = function(e) NULL)
  if (!is.null(r) && status_code(r) == 204) list(ok = TRUE, msg = "Actualización iniciada.")
  else list(ok = FALSE, msg = paste0("No se pudo iniciar (HTTP ", if (!is.null(r)) status_code(r) else "sin respuesta", ")."))
}

if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a
