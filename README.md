# Pulso VCC — Dashboard de Fondos (Shiny)

Dashboard del Comité de Inversiones de Vizcaya Capital: ~48 fondos aprobados +
índices macro + Renta Fija USA, con el mismo diseño del reporte semanal y una
columna extra de rentabilidad del mes anterior. Corre solo en la nube y se
actualiza todos los días.

## Arquitectura

```
┌─ GitHub Actions (cron diario 11:00 Chile) ─┐   ┌─ Repo GitHub ──────┐   ┌─ shinyapps.io ───────────┐
│ scripts/actualizar_datos.R                 │──▶│ data/series_vc.rds │──▶│ app.R lee el rds y dibuja │
│ scrapea CMF (token fijo) + macro + RF USA  │   │ + inputs editables │   │ el dashboard. Tu link.    │
└────────────────────────────────────────────┘   └────────────────────┘   │ Admin escribe de vuelta ──┘
```

- **El cron** (`.github/workflows/refresh.yml`) scrapea y deja `data/series_vc.rds` actualizado.
- **La app** lee ese archivo (y los inputs editables) desde el repo y dibuja el dashboard.
- **El panel de administración** (subir dividendos, VC manuales, correcciones) guarda los cambios commiteándolos al repo vía la API de GitHub.

## Archivos

```
app.R                      # UI + server (dashboard + administración)
R/  fondos_pulso.R         # catálogo curado (fondos, categorías, metadata, mapeo SEBRA)
    scraper.R              # scraping CMF (FIRES/FINRE/RGFMU)
    indices.R              # IPSA/USD (BCCh) + Yahoo + Treasury
    calculos.R compute.R   # cálculo de rentabilidades (MTD/YTD/mes anterior) + alertas
    dividendos.R ajustes.R # dividendos (Boletín) + factor de reparto -> VC ajustado
    ui_dashboard.R         # render HTML del dashboard (idéntico al original)
    excel_export.R         # genera file_show.xlsx (copia fiel, multi-hoja)
    github_store.R         # lee/escribe inputs en GitHub
    credenciales.R         # carga del token (env var o credentials.R local)
scripts/actualizar_datos.R # job de scraping (lo corre el cron)
scripts/importar_manuales_excel.R  # seed de manuales desde un file_show.xlsx viejo
data/                      # series_vc.rds, manuales_vc.csv, dividendos.xlsx, overrides
www/logo.jpg
```

## Correr en local

```r
# Dependencias (una vez)
install.packages(c("shiny","bslib","DT","httr","rvest","dplyr","tidyr","tibble",
                   "lubridate","stringr","quantmod","jsonlite","readxl","openxlsx"))

# Token CMF local
file.copy("R/credentials.example.R", "R/credentials.R")  # luego pega tu token

# Generar datos
Rscript -e "setwd('.'); source('scripts/actualizar_datos.R')"

# Correr la app
Rscript -e "shiny::runApp('.', host='127.0.0.1', port=7766, launch.browser=TRUE)"
```

## Deploy a la nube

### 1) Subir a GitHub
```bash
git remote add origin https://github.com/TU_USUARIO/TU_REPO.git
git push -u origin main
```
> `R/credentials.R` NO se sube (está en `.gitignore`).

### 2) Secreto del repo (para el cron)
En el repo: **Settings → Secrets and variables → Actions → New repository secret**
- `CMF_RECAPTCHA_TOKEN` = el token reCAPTCHA fijo.

El workflow ya está en `.github/workflows/refresh.yml` (corre diario y se puede disparar a mano en la pestaña **Actions**).

### 3) Token de GitHub (para que el admin guarde)
Crea un **Personal Access Token** (Settings → Developer settings → Tokens) con permiso
`contents: write` sobre el repo. Lo usarás como variable `GITHUB_TOKEN` en shinyapps.io.

### 4) Publicar en shinyapps.io
```r
install.packages("rsconnect")
rsconnect::setAccountInfo(name="TU_CUENTA", token="...", secret="...")  # de shinyapps.io → Account → Tokens
rsconnect::deployApp(
  appDir  = ".",
  appName = "pulso-vcc",
  envVars = c("GITHUB_TOKEN","GH_REPO","PULSO_DATA_URL")  # toma los valores de tu entorno local
)
```
Variables de entorno necesarias en la app:
- `GITHUB_TOKEN`   = el PAT del paso 3
- `GH_REPO`        = `TU_USUARIO/TU_REPO`
- `PULSO_DATA_URL` = `https://raw.githubusercontent.com/TU_USUARIO/TU_REPO/main`
- (opcional) `GH_BRANCH` = `main`

En ~1 minuto tendrás el link público `https://TU_CUENTA.shinyapps.io/pulso-vcc/`.

## Notas
- **CMF desde GitHub Actions:** la primera corrida del cron confirma si la CMF
  responde desde los servidores de GitHub (EE.UU.). Si los bloqueara, el plan B
  es correr `scripts/actualizar_datos.R` en un PC (Chile) con el Programador de
  tareas de Windows y hacer `git push` del `series_vc.rds`.
- **Token reCAPTCHA:** es fijo y no caduca (validado por meses). No requiere cookies.
