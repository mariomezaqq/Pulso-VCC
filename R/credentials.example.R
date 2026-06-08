# =============================================================================
# CREDENCIALES CMF - PLANTILLA PUBLICA
# Copia este archivo como `R/credentials.R` y pega tu token real.
# `R/credentials.R` esta en .gitignore (nunca se sube a GitHub).
#
# En la nube (shinyapps.io / GitHub Actions) NO se usa este archivo:
# el token se inyecta como variable de entorno CMF_RECAPTCHA_TOKEN.
# =============================================================================

# Token reCAPTCHA fijo (DevTools de Chrome -> POST entidad.php -> g-recaptcha-response)
RECAPTCHA_TOKEN <- "PEGA_AQUI_TU_TOKEN_RECAPTCHA"

# Cookies: la CMF NO las necesita para este flujo. Dejar "".
COOKIES <- ""
