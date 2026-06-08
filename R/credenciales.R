# =============================================================================
# CARGA DE CREDENCIALES
# Orden de busqueda:
#   1. Variable de entorno CMF_RECAPTCHA_TOKEN  (nube: shinyapps.io / GitHub Actions)
#   2. Archivo R/credentials.R                  (uso local en tu PC)
#   3. Placeholder                              (falla con mensaje claro)
# =============================================================================

cargar_credenciales <- function() {
  token <- Sys.getenv("CMF_RECAPTCHA_TOKEN", "")
  cookies <- Sys.getenv("CMF_COOKIES", "")

  if (!nzchar(token) && file.exists("R/credentials.R")) {
    e <- new.env()
    sys.source("R/credentials.R", envir = e)
    if (exists("RECAPTCHA_TOKEN", envir = e)) token   <- get("RECAPTCHA_TOKEN", envir = e)
    if (exists("COOKIES",         envir = e)) cookies <- get("COOKIES",         envir = e)
  }

  if (!nzchar(token) || token == "PEGA_AQUI_TU_TOKEN_RECAPTCHA") {
    stop("No hay token reCAPTCHA. Defini CMF_RECAPTCHA_TOKEN o crea R/credentials.R ",
         "(copia R/credentials.example.R).")
  }
  list(token = token, cookies = cookies)
}
