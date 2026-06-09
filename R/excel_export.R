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
  # Replica fiel del Comite: por cada fondo, junto a su VC van Div. Acum. +
  # Cuota+Div. (si tiene dividendos) o Factor Reparto + FA Acum + VC Ajustado
  # (si usa factor); abajo, MTD/YTD/2025/2024 como FORMULAS vivas de Excel.
  .int2col <- function(n) { rr <- ""; while (n > 0) { m <- (n-1) %% 26; rr <- paste0(LETTERS[m+1], rr); n <- (n-1) %/% 26 }; rr }
  st_tit <- createStyle(fontSize=11, fontName="Arial", textDecoration="bold")
  st_lbl <- createStyle(fontSize=10, fontName="Arial", textDecoration="bold", halign="CENTER", fgFill="#F2F2F2")
  st_pct <- createStyle(fontSize=10, fontName="Arial", numFmt="0.00%", border="TopBottomLeftRight", halign="RIGHT")

  escribir_hoja_cat <- function(cat) {
    fondos <- cat$fondos; nf <- length(fondos)
    sfun <- lapply(fondos, function(f) {
      s <- .serie_fondo(f, series_aj, vc_df)
      if (is.null(s) || !nrow(s)) return(NULL)
      s <- s[s$fecha >= FECHA_BASE_XLSX, , drop=FALSE]; if (!nrow(s)) return(NULL)
      s[order(s$fecha), , drop=FALSE]
    })
    fechas <- sort(unique(do.call(c, lapply(sfun, function(s) if (is.null(s)) as.Date(character()) else s$fecha))))
    addWorksheet(wb, cat$hoja); sh <- cat$hoja
    if (!length(fechas)) return(invisible())
    nF <- length(fechas)

    usa_fa <- vapply(fondos, function(f) isTRUE(f$nombre_excel %in% FONDOS_CON_FACTOR_AJUSTE), logical(1))
    divs <- lapply(fondos, function(f) {
      e <- div_por_fondo[[f$nombre_excel]]
      if (is.null(e) || !nrow(e)) return(NULL)
      e <- e[order(e$fecha_limite), , drop=FALSE]; e <- e[e$fecha_limite <= max(fechas), , drop=FALSE]
      if (nrow(e)) e else NULL
    })
    tiene_div <- vapply(seq_len(nf), function(j) !usa_fa[j] && !is.null(divs[[j]]), logical(1))

    cvc<-integer(nf); cfa<-rep(NA_integer_,nf); cfac<-rep(NA_integer_,nf); cvca<-rep(NA_integer_,nf)
    cdiv<-rep(NA_integer_,nf); ccmd<-rep(NA_integer_,nf); cc<-2
    for (j in seq_len(nf)) { cvc[j]<-cc; cc<-cc+1
      if (usa_fa[j]) { cfa[j]<-cc;cc<-cc+1; cfac[j]<-cc;cc<-cc+1; cvca[j]<-cc;cc<-cc+1 }
      else if (tiene_div[j]) { cdiv[j]<-cc;cc<-cc+1; ccmd[j]<-cc;cc<-cc+1 } }
    ult<-cc-1; col_lbl<-ult+2; col_calc<-ult+3

    writeData(wb, sh, cat$titulo, startCol=1, startRow=1); addStyle(wb, sh, st_tit, rows=1, cols=1)
    writeData(wb, sh, "Fecha", startCol=1, startRow=2); addStyle(wb, sh, S$hdr, rows=2, cols=1)
    for (j in seq_len(nf)) {
      f <- fondos[[j]]
      writeData(wb, sh, f$nombre_excel, startCol=cvc[j], startRow=2); addStyle(wb, sh, S$hdr, rows=2, cols=cvc[j])
      if (usa_fa[j]) {
        writeData(wb, sh, "Factor Reparto", startCol=cfa[j], startRow=2)
        writeData(wb, sh, "FA Acumulado", startCol=cfac[j], startRow=2)
        writeData(wb, sh, "VC Ajustado", startCol=cvca[j], startRow=2)
        for (cx in c(cfa[j],cfac[j],cvca[j])) addStyle(wb, sh, S$hdr, rows=2, cols=cx)
      } else if (tiene_div[j]) {
        writeData(wb, sh, "Div. Acum.", startCol=cdiv[j], startRow=2)
        writeData(wb, sh, "Cuota + Div.", startCol=ccmd[j], startRow=2)
        for (cx in c(cdiv[j],ccmd[j])) addStyle(wb, sh, S$hdr, rows=2, cols=cx)
      }
      nc <- if (usa_fa[j]) paste0(f$nombre_excel," (ajustado)") else if (tiene_div[j]) paste0(f$nombre_excel," + dividendo") else f$nombre_excel
      writeData(wb, sh, nc, startCol=col_calc+j, startRow=2); addStyle(wb, sh, S$hdr, rows=2, cols=col_calc+j)
    }

    # Calcular vectores por fondo y volcar en BLOQUE (una matriz + estilos por rango)
    refM <- vector("list", nf)
    Md <- matrix(NA_real_, nrow = nF, ncol = ult - 1)   # cols 2..ult del sheet
    for (j in seq_len(nf)) {
      s <- sfun[[j]]; vc <- rep(NA_real_, nF); ref <- rep(NA_real_, nF)
      if (!is.null(s)) {
        idx <- match(s$fecha, fechas); vc[idx] <- s$valor_cuota
        Md[, cvc[j]-1] <- vc; ref <- vc
        if (usa_fa[j]) {
          faacum_s <- if ("valor_cuota_ajustado" %in% names(s))
                        ifelse(!is.na(s$valor_cuota) & s$valor_cuota!=0, s$valor_cuota_ajustado/s$valor_cuota, NA_real_)
                      else if ("factor_reparto" %in% names(s)) cumprod(ifelse(is.na(s$factor_reparto),1,s$factor_reparto))
                      else rep(1, nrow(s))
          vca_s <- if ("valor_cuota_ajustado" %in% names(s)) s$valor_cuota_ajustado else s$valor_cuota * faacum_s
          fr_s  <- if ("factor_reparto" %in% names(s)) s$factor_reparto else rep(NA_real_, nrow(s))
          fr<-rep(NA_real_,nF); fac<-rep(NA_real_,nF); vca<-rep(NA_real_,nF)
          fr[idx]<-fr_s; fac[idx]<-faacum_s; vca[idx]<-vca_s
          Md[, cfa[j]-1]<-fr; Md[, cfac[j]-1]<-fac; Md[, cvca[j]-1]<-vca; ref<-vca
        } else if (tiene_div[j]) {
          e <- divs[[j]]
          dacum <- vapply(seq_len(nF), function(i) sum(e$monto[e$fecha_limite <= fechas[i]], na.rm=TRUE), numeric(1))
          cmd <- vc + dacum
          Md[, cdiv[j]-1]<-dacum; Md[, ccmd[j]-1]<-cmd; ref<-cmd
        }
      }
      refM[[j]] <- ref
    }
    writeData(wb, sh, data.frame(F = fechas), startCol = 1, startRow = 3, colNames = FALSE)
    writeData(wb, sh, Md, startCol = 2, startRow = 3, colNames = FALSE)
    addStyle(wb, sh, S$fch, rows = 3:(nF+2), cols = 1, gridExpand = TRUE)
    addStyle(wb, sh, S$num, rows = 3:(nF+2), cols = 2:ult, gridExpand = TRUE)

    max_f <- fechas[nF]; yr <- as.integer(format(max_f,"%Y"))
    target <- list(MTD = as.Date(format(max_f,"%Y-%m-01")) - 1,
                   YTD = as.Date(sprintf("%d-12-31", yr-1)),
                   `2025` = as.Date("2025-12-31"), `2024` = as.Date("2024-12-31"))
    mets <- c("MTD","YTD","2025","2024")
    for (k in seq_along(mets)) {
      met <- mets[k]; filam <- nF + 3 + (k-1)
      writeData(wb, sh, met, startCol=col_lbl, startRow=filam); addStyle(wb, sh, st_lbl, rows=filam, cols=col_lbl)
      for (j in seq_len(nf)) {
        ref <- refM[[j]]; pres <- which(!is.na(ref)); if (!length(pres)) next
        col_ref <- if (usa_fa[j]) cvca[j] else if (tiene_div[j]) ccmd[j] else cvc[j]
        cl <- .int2col(col_ref); num_row <- max(pres) + 2
        es_inicio <- met=="YTD" && (fondos[[j]]$nombre_excel %in% FONDOS_YTD_DESDE_INICIO)
        if (es_inicio) den_row <- min(pres) + 2
        else { cand <- pres[fechas[pres] <= target[[met]]]; if (!length(cand)) next; den_row <- max(cand) + 2 }
        writeFormula(wb, sh, paste0("=(",cl,num_row,"/$",cl,"$",den_row,")-1"), startCol=col_calc+j, startRow=filam)
        addStyle(wb, sh, st_pct, rows=filam, cols=col_calc+j)
      }
    }

    setColWidths(wb, sh, cols=1, widths=14)
    for (j in seq_len(nf)) {
      setColWidths(wb, sh, cols=cvc[j], widths=14)
      if (usa_fa[j]) setColWidths(wb, sh, cols=c(cfa[j],cfac[j],cvca[j]), widths=c(14,14,16))
      else if (tiene_div[j]) setColWidths(wb, sh, cols=c(cdiv[j],ccmd[j]), widths=c(14,16))
    }
    setColWidths(wb, sh, cols=ult+1, widths=4); setColWidths(wb, sh, cols=col_lbl, widths=8)
    setColWidths(wb, sh, cols=(col_calc+1):(col_calc+nf), widths=rep(18,nf))
  }
  for (cat in CATEGORIAS) escribir_hoja_cat(cat)

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
