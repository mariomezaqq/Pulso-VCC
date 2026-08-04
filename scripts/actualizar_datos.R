# =============================================================================
# ACTUALIZAR DATOS  (lo corre el cron diario 11:00 Chile / 15:00 UTC, y manual)
# Scrapea las series de valor cuota de los 48 fondos + indices macro + RF USA,
# y guarda data/series_vc.rds (series CRUDAS). Los dividendos / manuales se
# aplican al renderizar en la app, no aqui.
# =============================================================================
suppressMessages({ library(httr); library(rvest); library(dplyr); library(tidyr)
                   library(stringr); library(lubridate); library(tibble); library(jsonlite)
                   library(quantmod) })

base <- tryCatch(dirname(dirname(normalizePath(sys.frame(1)$ofile))), error = function(e) getwd())
if (!dir.exists(file.path(base, "R"))) base <- getwd()
setwd(base)
source("R/fondos_pulso.R"); source("R/credenciales.R"); source("R/scraper.R")
source("R/indices.R");      source("R/compute.R")

cred  <- cargar_credenciales()
hasta <- Sys.Date() - 1
desde <- as.Date(sprintf("%d-12-24", year(hasta) - 1))
message("Rango de consulta: ", desde, " a ", hasta)

# ---- 1) Series de fondos ----
# Reintenta cada fondo: la CMF falla de forma intermitente (sobre todo desde la
# IP del runner US del cron), asi que un solo intento no basta para correr solo.
scrapear_con_reintento <- function(fd, n = 3) {
  for (intento in seq_len(n)) {
    h <- tryCatch(scrapear_fondo(fd, desde, hasta, cred$token, cred$cookies),
                  error = function(e) { message("  [intento ", intento, "/", n, "] ERROR ",
                                                fd$nombre, ": ", e$message); NULL })
    if (!is.null(h) && nrow(h) > 0) return(h)
    if (intento < n) Sys.sleep(2 * intento)   # backoff: 2s, 4s
  }
  NULL
}
series <- list(); fallidos <- character()
for (fd in FONDOS) {
  h <- scrapear_con_reintento(fd, n = 3)
  if (!is.null(h)) series[[fd$nombre]] <- h else fallidos <- c(fallidos, fd$nombre)
  Sys.sleep(0.6)
}
message("Fondos OK: ", length(series), " / ", length(FONDOS),
        if (length(fallidos)) paste0(" | FALLIDOS: ", paste(fallidos, collapse = ", ")) else "")

# ---- GUARD anti-overwrite: no pisar datos buenos con un scrape vacio ----
# Si la CMF no responde y NO se scrapeo ningun fondo, abortamos con error: el
# step del cron queda marcado como fallido (alerta visible) y NO se commitea,
# asi conservamos el series_vc.rds anterior en vez de borrarlo en silencio.
ruta_rds <- file.path(base, "data", "series_vc.rds")
prev <- if (file.exists(ruta_rds)) tryCatch(readRDS(ruta_rds), error = function(e) NULL) else NULL
if (length(series) == 0) {
  n_prev <- if (!is.null(prev) && !is.null(prev$series)) length(prev$series) else 0L
  message("SCRAPE VACIO: 0/", length(FONDOS), " fondos OK. NO se sobrescribe series_vc.rds.",
          if (n_prev > 0) paste0(" Se conservan los ", n_prev, " fondos previos.") else "")
  quit(save = "no", status = 1)   # el run del cron queda como fallido; datos intactos
}

# --- Quest renta global A: VC en CLP -> USD (dolar observado) + valor fijo 31-12-2025 ---
# Replica del script base: se divide por el USD observado del dia habil siguiente
# (con fallback al ultimo disponible) y el cierre 2025 se fija en 2.2079 (en USD).
FX <- tryCatch(obtener_usdclp_web(hasta), error = function(e) NULL)
qn <- "Quest renta global A"
if (!is.null(series[[qn]]) && !is.null(FX) && nrow(FX) > 0) {
  h <- series[[qn]]
  h$valor_cuota <- vapply(seq_len(nrow(h)), function(i) {
    fx <- fx_proximo_dia_habil(h$fecha[i], FX)
    if (is.na(fx)) { prev <- FX[FX$fecha <= h$fecha[i], ]; if (nrow(prev)) fx <- prev$usdclp[which.max(prev$fecha)] }
    if (!is.na(fx) && fx > 0) h$valor_cuota[i] / fx else h$valor_cuota[i]
  }, numeric(1))
  d31 <- as.Date("2025-12-31")
  if (any(h$fecha == d31)) h$valor_cuota[h$fecha == d31] <- 2.2079
  series[[qn]] <- h
  message("  ", qn, " convertido a USD (cierre 2025 fijo=2.2079, ult VC=", round(tail(h$valor_cuota, 1), 4), ")")
} else if (!is.null(series[[qn]])) {
  message("  AVISO: sin USD observado; ", qn, " queda en CLP")
}

# ---- Resiliencia ante fallos PARCIALES ----
# Para los fondos que fallaron HOY pero existian en el archivo previo, conservamos
# su ultima serie buena (el dashboard ya marca con ⚠ los datos atrasados). Asi un
# hipo puntual de la CMF no hace desaparecer fondos del cuadro.
if (length(fallidos) && !is.null(prev) && !is.null(prev$series)) {
  rescatados <- intersect(fallidos, names(prev$series))
  for (nm in rescatados) series[[nm]] <- prev$series[[nm]]
  if (length(rescatados))
    message("Conservados del archivo previo (fallaron hoy): ", length(rescatados),
            " -> ", paste(rescatados, collapse = ", "))
}

# Cierre del grupo (moda de ultimos datos)
fechas_dato <- vapply(series, function(h) as.character(max(h$fecha)), character(1))
fc <- .moda_fecha(as.Date(fechas_dato)); if (is.na(fc)) fc <- hasta
message("Cierre del grupo: ", fc)

# ---- 2) Indices macro (WTD / MTD / YTD) ----
ref_wtd <- fc - 7
ref_mtd <- floor_date(fc, "month") - 1
ref_ytd <- as.Date(sprintf("%d-12-31", year(fc) - 1))

rent_serie <- function(h) {                # h: tibble(fecha, valor_cuota)
  list(wtd = .rent(h, ref_wtd, fc), mtd = .rent(h, ref_mtd, fc), ytd = .rent(h, ref_ytd, fc))
}
IPSA_31DIC2025 <- 10481.40                 # valor fijo (la serie web del BCCh no llega tan atras)
macro <- list(); macro_series <- list()
for (idx in INDICES) {
  r <- list(wtd = NA_real_, mtd = NA_real_, ytd = NA_real_); s <- NULL
  if (!is.null(idx$fuente) && idx$fuente == "bcch") {
    # IPSA: fuente OFICIAL del BCCh (Canasta), al dia y confiable. Fallback a
    # datosmacro si el BCCh falla. Se acumula con el historico previo guardado en
    # macro_series por si una descarga falla. (Se descarto Yahoo ^IPSA: quedaba
    # congelado dias, y datosmacro solo: se habia corrompido a ~la mitad.)
    s <- tryCatch(obtener_ipsa_bcch_serie(), error = function(e) NULL)
    if (is.null(s) || !nrow(s))
      s <- tryCatch(obtener_ipsa_datosmacro(desde = ref_ytd, hasta = fc), error = function(e) NULL)
    prev_ipsa <- if (!is.null(prev) && !is.null(prev$macro_series)) prev$macro_series[[idx$nombre]] else NULL
    s <- bind_rows(s, prev_ipsa)                 # fuente nueva primero -> pisa al previo en duplicados
    if (!is.null(s) && nrow(s) > 0) {
      s <- s %>% filter(!is.na(valor)) %>% distinct(fecha, .keep_all = TRUE) %>% arrange(fecha)
      if (!any(s$fecha <= ref_ytd))              # fallback: cierre 2025 fijo si la fuente no lo trae
        s <- bind_rows(tibble(fecha = ref_ytd, valor = IPSA_31DIC2025), s) %>% arrange(fecha)
      # Guard: descarta saltos imposibles (por si cae al fallback datosmacro corrupto).
      s <- filtrar_saltos_ipsa(s)
    } else s <- NULL
  } else if (!is.null(idx$ticker) && nchar(idx$ticker) > 0) {
    s <- tryCatch(obtener_historico_yahoo_json(idx$ticker, ref_ytd - 5, fc), error = function(e) NULL)
    Sys.sleep(1)
  }
  if (!is.null(s) && nrow(s) > 0) {
    r <- rent_serie(s %>% rename(valor_cuota = valor))
    macro_series[[idx$nombre]] <- s
  }
  macro[[length(macro) + 1]] <- list(nombre = idx$nombre, wtd = r$wtd, mtd = r$mtd, ytd = r$ytd)
  message("  Macro ", idx$nombre, ": WTD ", round(r$wtd*100,2), " MTD ", round(r$mtd*100,2), " YTD ", round(r$ytd*100,2))
}

# ---- 3) Renta Fija USA (Treasury 10y ^TNX) ----
rf_usa <- NULL
tnx <- tryCatch(obtener_treasury_10y(fc), error = function(e) NULL)
if (!is.null(tnx) && nrow(tnx) > 0) {
  val_a <- function(ref) { f <- tnx %>% filter(fecha <= ref) %>% arrange(desc(fecha)) %>% slice(1)
                           if (nrow(f)) f$valor[1] else NA_real_ }
  rf_usa <- list(
    headers = list(cierre = "Cierre 2025", fecha1 = format(ref_wtd, "%d/%m"), fecha2 = format(fc, "%d/%m")),
    rows = list(list(nombre = "Treasury 10y",
                     cierre2025 = val_a(ref_ytd), abr20 = val_a(ref_wtd), abr27 = val_a(fc)))
  )
}

# ---- 4) Guardar ----
datos <- list(generado = format(Sys.time(), "%Y-%m-%d %H:%M", tz = "America/Santiago"), cierre = as.character(fc),
              series = series, macro = macro, macro_series = macro_series,
              tnx_series = if (!is.null(tnx) && nrow(tnx) > 0) tnx else NULL,
              rf_usa = rf_usa, fallidos = fallidos)
saveRDS(datos, file.path(base, "data", "series_vc.rds"))
message("Guardado data/series_vc.rds | series: ", length(series),
        " | macro: ", length(macro), " (", length(macro_series), " con serie diaria)",
        " | RF USA: ", if (is.null(rf_usa)) 0 else length(rf_usa$rows))
