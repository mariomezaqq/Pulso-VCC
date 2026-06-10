# =============================================================================
# PROCESAR DIVIDENDOS DEL BOLETIN BURSATIL  (con caso "monto total")
#
# 1) Descarga el ULTIMO boletin disponible (itera fechas hacia atras).
# 2) Lee "Dividendos CFI nacionales" y "Repartos CFI-CFM" capturando tanto el
#    dividendo "Por Accion / cuota" como el "Monto total a distribuir".
# 3) Cruza con el catalogo para obtener run / tipoentidad / row.
# 4) CASO ESPECIAL: filas con "Por cuota" VACIO y "Monto total" presente ->
#    obtiene las cuotas del fondo a la FECHA LIMITE (cuotas = Pat.Neto / Valor
#    cuota, desde la CMF) y calcula  div_por_cuota = monto_total / cuotas.
# 5) Escribe data/dividendos_calculados.csv con AMBOS resultados (trazable).
#
# Uso:  Rscript scripts/procesar_dividendos.R
# =============================================================================
# Ruta local solo si existe (en GitHub Actions el working dir ya es la raiz del repo).
if (dir.exists("C:/Users/Mario/Desktop/vcc-fondos-dashboard-bundle"))
  setwd("C:/Users/Mario/Desktop/vcc-fondos-dashboard-bundle")
suppressMessages({ library(httr); library(rvest); library(readxl); library(dplyr)
                   library(stringr); library(lubridate); library(tibble) })
# Credenciales: local desde R/credentials.R; en CI desde variables de entorno
# (secrets: CMF_RECAPTCHA_TOKEN, opcional CMF_COOKIES).
if (file.exists("R/credentials.R")) source("R/credentials.R")
source("R/catalogo.R")             # CATALOGO_FONDOS
source("R/scraper.R")              # obtener_datos_cmf
source("R/dividendos.R")           # normalizers + descargar_ultimo_boletin
source("R/cuotas_cmf.R")           # cuotas_fondo_a_fecha
TOKEN   <- if (exists("RECAPTCHA_TOKEN") && nchar(RECAPTCHA_TOKEN) > 50) RECAPTCHA_TOKEN else Sys.getenv("CMF_RECAPTCHA_TOKEN", "")
COOKIES <- if (exists("COOKIES") && nzchar(COOKIES)) COOKIES else Sys.getenv("CMF_COOKIES", "")
if (nchar(TOKEN) < 50) message("[proc] AVISO: token reCAPTCHA vacio/corto; los fondos FIRES/FINRE no scrapearan cuotas.")

# ---- 1) Boletin a procesar ----
# Orden de obtencion del boletin:
#   1) AUTO-DOWNLOAD del ultimo desde servicioscms.bolsadesantiago.com (sin captcha).
#   2) env BOLETIN_XLSX -> fallback: el ultimo boletin subido por la app (memoria GitHub).
#   3) el xlsx mas reciente que haya en data/.
ruta <- tryCatch(descargar_ultimo_boletin("data", dias_atras = 12), error = function(e) NULL)
if (is.null(ruta) || !nzchar(ruta) || !file.exists(ruta)) {
  rb <- Sys.getenv("BOLETIN_XLSX", "")
  if (nzchar(rb) && file.exists(rb)) ruta <- rb
}
if (is.null(ruta) || !nzchar(ruta) || !file.exists(ruta)) {
  cand <- list.files("data", pattern = "(resumen_dividendos|dividendos_ultimo)\\.xlsx$", full.names = TRUE)
  if (length(cand) > 0) ruta <- cand[order(file.info(cand)$mtime, decreasing = TRUE)][1]
}
stopifnot(!is.null(ruta), nzchar(ruta), file.exists(ruta))
cat("[proc] Boletin a procesar:", ruta, "\n")

# ---- 2) Leer hojas capturando por_cuota Y monto_total ----
leer_hoja2 <- function(sheet, skip, tipo_evento) {
  df <- tryCatch(suppressMessages(read_excel(ruta, sheet = sheet, skip = skip, col_types = "text")),
                 error = function(e) NULL)
  if (is.null(df) || nrow(df) == 0) return(NULL)
  cols <- tolower(colnames(df))
  pick <- function(p) { w <- which(grepl(p, cols)); if (length(w)) w[1] else NA_integer_ }
  i_nom <- pick("^nombre"); i_ser <- pick("^serie"); i_mon <- pick("^moneda")
  i_pc  <- pick("por acci|por cuota|acci.*cuota"); i_tot <- pick("monto total|total a distribuir")
  i_lim <- pick("l.mite|limite"); i_pag <- pick("^pago")
  if (any(is.na(c(i_nom, i_ser, i_lim)))) return(NULL)
  tibble(
    nombre_original = as.character(df[[i_nom]]),
    serie_original  = as.character(df[[i_ser]]),
    moneda          = if (!is.na(i_mon)) trimws(as.character(df[[i_mon]])) else NA_character_,
    por_cuota_raw   = if (!is.na(i_pc))  as.character(df[[i_pc]])  else NA_character_,
    monto_total_raw = if (!is.na(i_tot)) as.character(df[[i_tot]]) else NA_character_,
    fecha_lim_raw   = as.character(df[[i_lim]]),
    fecha_pago_raw  = if (!is.na(i_pag)) as.character(df[[i_pag]]) else NA_character_,
    tipo_evento     = tipo_evento
  )
}

ev <- bind_rows(
  leer_hoja2("Dividendos CFI nacionales", 11, "Dividendo"),
  leer_hoja2("Repartos CFI-CFM",          10, "Reparto")
)

ev <- ev %>%
  filter(!is.na(nombre_original), nombre_original != "NA", nzchar(nombre_original),
         !is.na(serie_original),  serie_original  != "NA", nzchar(serie_original)) %>%
  mutate(
    nombre_norm  = sapply(nombre_original, .normalizar_nombre),
    serie_norm   = sapply(serie_original,  .normalizar_serie),
    fecha_limite = .parse_fecha(fecha_lim_raw),
    fecha_pago   = .parse_fecha(fecha_pago_raw),
    por_cuota    = .parse_monto(por_cuota_raw),
    monto_total  = .parse_monto(monto_total_raw),
    es_total     = (is.na(por_cuota) | por_cuota <= 0) & !is.na(monto_total) & monto_total > 0
  ) %>%
  filter(!is.na(fecha_limite), fecha_limite >= as.Date("2025-01-01"),
         (!is.na(por_cuota) & por_cuota > 0) | es_total) %>%
  distinct(nombre_norm, serie_norm, tipo_evento, fecha_limite, .keep_all = TRUE)

cat(sprintf("[proc] eventos: %d | con monto total a calcular: %d\n",
            nrow(ev), sum(ev$es_total)))

# ---- 3) Cruce con catalogo (run/tipoentidad/row) ----
cat_norm <- CATALOGO_FONDOS %>%
  mutate(nombre_norm = sapply(nombre, .normalizar_nombre),
         serie_norm  = sapply(serie,  .normalizar_serie)) %>%
  select(run, serie_cat = serie, tipoentidad, row, nombre_norm, serie_norm)

ev <- ev %>% left_join(cat_norm, by = c("nombre_norm", "serie_norm"))

# Fallback por NOMBRE: si la serie no esta en el catalogo (ej. series nuevas
# IE/DE/EE) igual obtenemos run/tipoentidad/row del fondo -> las cuotas son del
# FONDO, no de la serie, asi que sirve para el caso "monto total".
cat_nombre <- cat_norm %>% distinct(nombre_norm, .keep_all = TRUE) %>%
  select(nombre_norm, run_n = run, tipo_n = tipoentidad, row_n = row)
ev <- ev %>% left_join(cat_nombre, by = "nombre_norm") %>%
  mutate(run         = ifelse(is.na(run), run_n, run),
         tipoentidad = ifelse(is.na(tipoentidad), tipo_n, tipoentidad),
         row         = ifelse(is.na(row), row_n, row)) %>%
  select(-run_n, -tipo_n, -row_n)

# ---- 4) Resolver el caso "monto total" (cuotas a la fecha limite) ----
# Cache por (run, fecha_limite): el total y las cuotas son del FONDO, no de la serie.
cache_cuotas <- list()
get_cuotas <- function(run, tipoentidad, row, fl) {
  key <- paste0(run, "_", format(fl, "%Y%m%d"))
  if (!is.null(cache_cuotas[[key]])) return(cache_cuotas[[key]])
  res <- tryCatch(cuotas_fondo_a_fecha(run, tipoentidad, row, fl, TOKEN, COOKIES),
                  error = function(e) NULL)
  cache_cuotas[[key]] <<- res
  res
}

ev$cuotas_fecha_limite <- NA_real_
ev$fecha_cuotas_usada  <- as.Date(NA)
ev$div_calculado       <- NA_real_
ev$motivo              <- NA_character_

idx_tot <- which(ev$es_total & !is.na(ev$run))
for (i in idx_tot) {
  res <- get_cuotas(ev$run[i], ev$tipoentidad[i], ev$row[i], ev$fecha_limite[i])
  if (is.null(res)) { ev$motivo[i] <- "no se pudo consultar la CMF"; next }
  if (isFALSE(res$disponible) || is.na(res$total_cuotas) || res$total_cuotas <= 0) {
    ev$motivo[i] <- res$motivo %||% "sin cuotas en la fecha limite"; next
  }
  ev$cuotas_fecha_limite[i] <- res$total_cuotas
  ev$fecha_cuotas_usada[i]  <- res$fecha_usada
  ev$div_calculado[i]       <- ev$monto_total[i] / res$total_cuotas
}

# ---- 5) Monto final + origen + archivo ----
final <- ev %>% transmute(
  nombre = nombre_original, serie = serie_original,
  run, tipoentidad, fecha_limite, fecha_pago, moneda, tipo_evento,
  monto_por_cuota_boletin = round(por_cuota, 6),
  monto_total,
  cuotas_fecha_limite = round(cuotas_fecha_limite),
  fecha_cuotas_usada,
  div_por_cuota_calculado = round(div_calculado, 6),
  monto_final = ifelse(es_total, div_calculado, por_cuota),
  origen = case_when(
    !es_total                        ~ "boletin",
    es_total & !is.na(div_calculado) ~ "calculado (total/cuotas dia limite)",
    TRUE                             ~ "pendiente (VC del dia limite no disponible)"),
  motivo
) %>% arrange(origen, nombre, fecha_limite)

out <- "data/dividendos_calculados.csv"
write.csv(final, out, row.names = FALSE, fileEncoding = "UTF-8")
cat(sprintf("\n[proc] LISTO -> %s (%d filas, %d calculadas por total/cuotas)\n",
            out, nrow(final), sum(final$origen != "boletin", na.rm = TRUE)))
cat("[proc] Ejemplos calculados:\n")
print(as.data.frame(final %>% filter(origen != "boletin") %>%
        select(nombre, serie, fecha_limite, monto_total, cuotas_fecha_limite,
               fecha_cuotas_usada, div_por_cuota_calculado) %>% head(12)))
