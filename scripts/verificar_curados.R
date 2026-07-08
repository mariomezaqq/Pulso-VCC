# Verifica que reconstruir las listas desde data/fondos_curados.csv == hardcode.
`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a
options(pulso.hardcode_only = TRUE)
source("R/fondos_pulso.R", local = TRUE)   # hardcode: FONDOS, CATEGORIAS, DATOS_FONDO, MAPEO_SEBRA

# ---- reconstruir desde CSV (misma logica que ira en fondos_pulso.R) ----
df <- read.csv("data/fondos_curados.csv", stringsAsFactors = FALSE, fileEncoding = "UTF-8",
               colClasses = "character")
df$orden <- as.integer(df$orden)
df <- df[order(df$orden), ]

# FONDOS: unico por nombre_script (no manuales)
r_FONDOS <- list()
vis <- character()
for (i in which(df$es_manual %in% c("FALSE","false","0") & nzchar(df$nombre_script))) {
  ns <- df$nombre_script[i]
  if (ns %in% vis) next
  vis <- c(vis, ns)
  ent <- list(nombre = ns, run = df$run[i], serie = df$serie[i], row = df$row[i])
  if (nzchar(df$tipoentidad[i])) ent$tipoentidad <- df$tipoentidad[i]
  r_FONDOS[[length(r_FONDOS)+1L]] <- ent
}

# CATEGORIAS: agrupar por hoja preservando orden
r_CATEGORIAS <- list()
for (hj in unique(df$hoja)) {
  sub <- df[df$hoja == hj, ]
  fondos <- lapply(seq_len(nrow(sub)), function(k)
    list(nombre_script = sub$nombre_script[k], nombre_excel = sub$nombre_excel[k]))
  r_CATEGORIAS[[length(r_CATEGORIAS)+1L]] <- list(hoja = hj, titulo = sub$titulo[1], fondos = fondos)
}

# DATOS_FONDO: por nombre_excel unico
r_DATOS <- list()
for (ne in unique(df$nombre_excel)) {
  i <- which(df$nombre_excel == ne)[1]
  r_DATOS[[ne]] <- list(rent2024 = df$rent2024[i], rent2025 = df$rent2025[i],
                        duracion = df$duracion[i], liquidez = df$liquidez[i],
                        moneda = df$moneda[i], tac = df$tac[i])
}

# MAPEO_SEBRA: por nombre_excel unico
r_SEBRA <- list()
seen <- character()
for (i in seq_len(nrow(df))) {
  ne <- df$nombre_excel[i]; if (ne %in% seen || !nzchar(ne)) next
  seen <- c(seen, ne)
  tk <- df$ticker_sebra[i]; tk <- if (!nzchar(tk)) NA else tk
  r_SEBRA[[length(r_SEBRA)+1L]] <- list(nombre_excel = ne, ticker_sebra = tk)
}

# ---- comparaciones ----
cmp <- function(lbl, a, b) cat(sprintf("%-16s hardcode=%d  csv=%d  identico=%s\n",
      lbl, length(a), length(b), identical(a, b)))

# normalizar FONDOS hardcode: agregar tipoentidad FIRES explicito para comparar
norm_f <- lapply(FONDOS, function(x){ if (is.null(x$tipoentidad)) x$tipoentidad <- NULL; x })

cat("== conteos ==\n")
cmp("FONDOS", FONDOS, r_FONDOS)
cmp("CATEGORIAS", CATEGORIAS, r_CATEGORIAS)
cmp("DATOS_FONDO", DATOS_FONDO, r_DATOS)
cmp("MAPEO_SEBRA", MAPEO_SEBRA, r_SEBRA)

# Diferencias finas (nombres/keys)
cat("\n== chequeos de contenido ==\n")
cat("FONDOS runs iguales:", identical(sort(sapply(FONDOS, `[[`, "run")),
                                      sort(sapply(r_FONDOS, `[[`, "run"))), "\n")
cat("DATOS keys iguales:", identical(sort(names(DATOS_FONDO)), sort(names(r_DATOS))), "\n")
cat("CATEGORIAS titulos:", identical(sapply(CATEGORIAS, `[[`, "titulo"),
                                     sapply(r_CATEGORIAS, `[[`, "titulo")), "\n")
# comparar DATOS_FONDO valor a valor
difs <- 0
for (k in names(DATOS_FONDO)) {
  a <- DATOS_FONDO[[k]]; b <- r_DATOS[[k]]
  for (campo in c("rent2024","rent2025","duracion","liquidez","moneda","tac")) {
    va <- as.character(a[[campo]] %||% ""); vb <- as.character(b[[campo]] %||% "")
    if (!identical(va, vb)) { difs <- difs + 1; cat("  DIF", k, campo, ":[", va, "] vs [", vb, "]\n") }
  }
}
cat("DATOS_FONDO difs de valor:", difs, "\n")
