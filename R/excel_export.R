# =============================================================================
# GENERADOR DE file_show.xlsx  (copia fiel del Excel del Comite original)
# Cuadro resumen identico (estilos Arial, secciones Indices macro / Renta Fija
# USA / categorias) + una hoja por categoria (serie VC diaria) + Datos macro +
# Treasury + Dividendos + Fondos Manuales + Indices Manuales.
# Todas las series parten desde el 31-dic-2025.
# =============================================================================
suppressMessages({ library(openxlsx); library(dplyr); library(lubridate); library(tibble) })

FECHA_BASE_XLSX <- as.Date("2025-12-31")
.MESES_XLSX <- c("Enero","Febrero","Marzo","Abril","Mayo","Junio",
                 "Julio","Agosto","Septiembre","Octubre","Noviembre","Diciembre")
.fmt_es <- function(d) if (is.na(d)) "-" else paste(format(d, "%d"), "de", .MESES_XLSX[as.integer(format(d, "%m"))])

.estilos <- function() list(
  tit = createStyle(fontSize=13, fontName="Arial", textDecoration="bold", fontColour="#FFFFFF", fgFill="#1F4E79", halign="left"),
  cat = createStyle(fontSize=11, fontName="Arial", textDecoration="bold", fontColour="#FFFFFF", fgFill="#2E75B6", border="TopBottomLeftRight"),
  hdr = createStyle(fontSize=10, fontName="Arial", textDecoration="bold", border="TopBottomLeftRight", halign="center", fgFill="#D9E1F2"),
  nom = createStyle(fontSize=10, fontName="Arial", border="TopBottomLeftRight", halign="left"),
  txt = createStyle(fontSize=10, fontName="Arial", border="TopBottomLeftRight", halign="center"),
  ptx = createStyle(fontSize=10, fontName="Arial", border="TopBottomLeftRight", halign="right"),
  pos = createStyle(fontSize=10, fontName="Arial", numFmt="0.00%", border="TopBottomLeftRight", halign="right", fontColour="#1D6A2E", fgFill="#C6EFCE"),
  neg = createStyle(fontSize=10, fontName="Arial", numFmt="0.00%", border="TopBottomLeftRight", halign="right", fontColour="#9C0006", fgFill="#FFC7CE"),
  na  = createStyle(fontSize=10, fontName="Arial", border="TopBottomLeftRight", halign="center", fontColour="#888888"),
  tnx = createStyle(fontSize=10, fontName="Arial", numFmt="0.00", border="TopBottomLeftRight", halign="right"),
  fch = createStyle(fontSize=10, fontName="Arial", numFmt="DD/MM/YYYY", border="TopBottomLeftRight"),
  num = createStyle(fontSize=10, fontName="Arial", numFmt="#,##0.0000", border="TopBottomLeftRight", halign="right")
)

.serie_fondo <- function(f, series_aj, vc_df) {
  if (identical(f$nombre_script, "__MANUAL__")) {
    s <- vc_df[vc_df$Nombre == f$nombre_excel, c("Fecha","Valor")]; if (!nrow(s)) return(NULL)
    tibble(fecha=as.Date(s$Fecha), valor_cuota=as.numeric(s$Valor)) %>%
      filter(!is.na(fecha), !is.na(valor_cuota)) %>% arrange(fecha) %>% distinct(fecha, .keep_all=TRUE)
  } else series_aj[[f$nombre_script]]
}

generar_file_show <- function(pd, datos, series_aj, div_por_fondo, vc_df, path) {
  S <- .estilos(); wb <- createWorkbook(); NC <- 9
  fc <- if (!is.null(pd$fecha_cierre_date)) pd$fecha_cierre_date else as.Date(datos$cierre)

  # ============ Cuadro resumen ============
  sh <- "Cuadro resumen"; addWorksheet(wb, sh)
  mergeCells(wb, sh, cols=1:NC, rows=1)
  writeData(wb, sh, paste0("Pulso VCC | Cierre ", pd$fecha_cierre), startRow=1)
  addStyle(wb, sh, S$tit, rows=1, cols=1:NC, gridExpand=TRUE); setRowHeights(wb, sh, rows=1, heights=28)
  r <- 2
  seccion <- function(txt, headers) {
    mergeCells(wb, sh, cols=1:NC, rows=r); writeData(wb, sh, txt, startCol=1, startRow=r)
    addStyle(wb, sh, S$cat, rows=r, cols=1:NC, gridExpand=TRUE); r <<- r + 1
    for (j in seq_along(headers)) writeData(wb, sh, headers[j], startCol=j, startRow=r)
    for (j in (length(headers)+1):NC) writeData(wb, sh, "", startCol=j, startRow=r)
    addStyle(wb, sh, S$hdr, rows=r, cols=1:NC, gridExpand=TRUE); r <<- r + 1
  }
  wr_pct <- function(col, v) {
    if (is.null(v) || is.na(v)) { writeData(wb, sh, "", startCol=col, startRow=r); addStyle(wb, sh, S$na, rows=r, cols=col) }
    else { writeData(wb, sh, v, startCol=col, startRow=r); addStyle(wb, sh, if (v>=0) S$pos else S$neg, rows=r, cols=col) }
  }
  blanco <- function(cols) for (c in cols) { writeData(wb, sh, "", startCol=c, startRow=r); addStyle(wb, sh, S$txt, rows=r, cols=c) }

  # -- Indices macro --
  seccion("Indices macro", c("Indice","WTD","MTD","YTD"))
  for (idx in pd$indices_macro) {
    writeData(wb, sh, idx$nombre, startCol=1, startRow=r); addStyle(wb, sh, S$nom, rows=r, cols=1)
    wr_pct(2, idx$wtd); wr_pct(3, idx$mtd); wr_pct(4, idx$ytd); blanco(5:NC); r <- r + 1
  }
  r <- r + 1
  # -- Renta Fija USA --
  if (!is.null(pd$rf_usa) && length(pd$rf_usa$rows)) {
    seccion("Renta Fija USA", c("Indice", paste0("Cierre ", year(fc)-1), .fmt_es(fc-7), .fmt_es(fc)))
    for (x in pd$rf_usa$rows) {
      writeData(wb, sh, x$nombre, startCol=1, startRow=r); addStyle(wb, sh, S$nom, rows=r, cols=1)
      vals <- list(x$cierre2025, x$abr20, x$abr27)
      for (cc in 2:4) { v <- vals[[cc-1]]
        if (is.null(v)||is.na(v)) { writeData(wb, sh, "", startCol=cc, startRow=r); addStyle(wb, sh, S$na, rows=r, cols=cc) }
        else { writeData(wb, sh, round(v,2), startCol=cc, startRow=r); addStyle(wb, sh, S$tnx, rows=r, cols=cc) } }
      blanco(5:NC); r <- r + 1
    }
    r <- r + 1
  }
  # -- Categorias --
  for (cat in pd$categorias) {
    metas <- lapply(cat$fondos, function(f) f$duracion)
    tiene_dur <- any(vapply(cat$fondos, function(f) !is.null(f$duracion) && nzchar(f$duracion) && f$duracion!="—", logical(1)))
    headers <- c("Fondo","Rentabilidad MTD","Rentabilidad YTD","Rentabilidad 2025","Rentabilidad 2024",
                 if (tiene_dur) "Duracion" else "", "Liquidez","Moneda","TAC")
    seccion(cat$nombre, headers)
    for (f in cat$fondos) {
      writeData(wb, sh, f$nombre, startCol=1, startRow=r); addStyle(wb, sh, S$nom, rows=r, cols=1)
      wr_pct(2, f$mtd); wr_pct(3, f$ytd)
      txt_o_blanco <- function(col, val, sty) {
        v <- if (is.null(val) || is.na(val) || val %in% c("","—")) "" else val
        writeData(wb, sh, v, startCol=col, startRow=r); addStyle(wb, sh, sty, rows=r, cols=col)
      }
      txt_o_blanco(4, f$rent2025, S$ptx); txt_o_blanco(5, f$rent2024, S$ptx)
      txt_o_blanco(6, f$duracion, S$txt); txt_o_blanco(7, f$liquidez, S$txt)
      txt_o_blanco(8, f$moneda, S$txt);  txt_o_blanco(9, f$tac, S$txt); r <- r + 1
    }
    r <- r + 1
  }
  setColWidths(wb, sh, cols=1, widths=38); setColWidths(wb, sh, cols=2:5, widths=16)
  setColWidths(wb, sh, cols=6, widths=10); setColWidths(wb, sh, cols=7, widths=18)
  setColWidths(wb, sh, cols=8, widths=10); setColWidths(wb, sh, cols=9, widths=14)

  # ============ Hojas por categoria (serie VC diaria desde 31-dic) ============
  esc_wide <- function(sh, wide) {
    writeData(wb, sh, wide, startRow=1, headerStyle=S$hdr)
    n <- nrow(wide)
    if (n) { addStyle(wb, sh, S$fch, rows=2:(n+1), cols=1, gridExpand=TRUE)
      if (ncol(wide)>1) addStyle(wb, sh, S$num, rows=2:(n+1), cols=2:ncol(wide), gridExpand=TRUE) }
    setColWidths(wb, sh, cols=1, widths=12); setColWidths(wb, sh, cols=2:max(2,ncol(wide)), widths=16)
  }
  for (cat in CATEGORIAS) {
    sh <- substr(cat$titulo, 1, 31); addWorksheet(wb, sh); tabs <- list()
    for (f in cat$fondos) {
      s <- .serie_fondo(f, series_aj, vc_df); if (is.null(s) || !nrow(s)) next
      s <- s[s$fecha >= FECHA_BASE_XLSX, , drop=FALSE]; if (!nrow(s)) next
      d <- s[, c("fecha","valor_cuota")]; names(d) <- c("Fecha", f$nombre_excel); tabs[[length(tabs)+1]] <- d
      if ("valor_cuota_ajustado" %in% names(s) && any(abs(s$valor_cuota_ajustado-s$valor_cuota)>1e-9, na.rm=TRUE)) {
        da <- s[, c("fecha","valor_cuota_ajustado")]; names(da) <- c("Fecha", paste0(f$nombre_excel," Aj.")); tabs[[length(tabs)+1]] <- da
      }
    }
    if (length(tabs)) { wide <- Reduce(function(a,b) full_join(a,b,by="Fecha"), tabs); esc_wide(sh, wide[order(wide$Fecha),]) }
  }

  # ============ Datos macro ============
  if (!is.null(datos$macro_series) && length(datos$macro_series)) {
    tabs <- lapply(names(datos$macro_series), function(nm) {
      d <- datos$macro_series[[nm]][, c("fecha","valor")]; d <- d[d$fecha>=FECHA_BASE_XLSX,]; names(d) <- c("Fecha", nm); d })
    tabs <- Filter(function(d) nrow(d)>0, tabs)
    if (length(tabs)) { addWorksheet(wb, "Datos macro")
      esc_wide("Datos macro", (Reduce(function(a,b) full_join(a,b,by="Fecha"), tabs)) %>% arrange(Fecha)) }
  }

  # ============ Treasury ============
  if (!is.null(datos$tnx_series) && nrow(datos$tnx_series)) {
    addWorksheet(wb, "Treasury")
    d <- datos$tnx_series[, c("fecha","valor")]; d <- d[d$fecha>=FECHA_BASE_XLSX,]; names(d) <- c("Fecha","Treasury 10y (^TNX)")
    esc_wide("Treasury", d[order(d$Fecha),])
  }

  # ============ Dividendos ============
  addWorksheet(wb, "Dividendos")
  dd <- do.call(rbind, lapply(names(div_por_fondo), function(nm) { e <- div_por_fondo[[nm]]
    if (is.null(e)||!nrow(e)) return(NULL)
    data.frame(Fondo=nm, `Fecha limite`=as.Date(e$fecha_limite), Monto=e$monto, check.names=FALSE) }))
  if (is.null(dd)) dd <- data.frame(Fondo=character(),`Fecha limite`=as.Date(character()),Monto=numeric(),check.names=FALSE)
  writeData(wb, "Dividendos", dd, startRow=1, headerStyle=S$hdr)
  if (nrow(dd)) { addStyle(wb, "Dividendos", S$fch, rows=2:(nrow(dd)+1), cols=2, gridExpand=TRUE)
    addStyle(wb, "Dividendos", S$num, rows=2:(nrow(dd)+1), cols=3, gridExpand=TRUE) }
  setColWidths(wb, "Dividendos", cols=1:3, widths=c(34,14,14))

  # ============ Fondos / Indices Manuales (orden original) ============
  manual_wide <- function(tipo, orden) {
    sub <- vc_df[vc_df$Tipo==tipo, , drop=FALSE]; if (!nrow(sub)) return(NULL)
    pres <- unique(sub$Nombre); nombres <- c(orden[orden %in% pres], setdiff(pres, orden))
    tabs <- lapply(nombres, function(nm) { d <- sub[sub$Nombre==nm, c("Fecha","Valor")]
      names(d) <- c("Fecha", nm); d$Fecha <- as.Date(d$Fecha); d <- d[d$Fecha>=FECHA_BASE_XLSX,]; d[order(d$Fecha),] })
    Reduce(function(a,b) full_join(a,b,by="Fecha"), tabs) %>% arrange(Fecha)
  }
  for (tt in list(list("Fondo","Fondos Manuales", NOMBRES_FONDOS_MANUALES),
                  list("Indice","Indices Manuales", NOMBRES_INDICES_MANUALES))) {
    w <- manual_wide(tt[[1]], tt[[3]]); addWorksheet(wb, tt[[2]])
    if (!is.null(w) && nrow(w)) esc_wide(tt[[2]], w)
  }

  saveWorkbook(wb, path, overwrite=TRUE)
}
