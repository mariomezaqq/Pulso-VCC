# =============================================================================
# CORREO DIARIO DEL PULSO  (texto a partir de `pd`, lo mismo que muestra el dashboard)
# Plantilla fija: solo cambian las cifras, y los verbos segun el signo. No usa
# ningun modelo de lenguaje: cada numero sale directo de `pd`.
#   - Indices (S&P 500, Nasdaq, ACWI, IPSA) y macro (oro, cobre, Brent): WTD, mes, YTD
#   - Treasury 10 años: nivel hace una semana vs cierre, y contra el cierre del año anterior
#   - Instrumentos de la casa: mes y YTD
# Si el cierre es el ultimo dia habil del mes dice "cerró septiembre con..."; si
# no, "registra en noviembre...". Lo usa scripts/correo_diario.R.
# =============================================================================
URL_PULSO <- "https://mariomezaqq.shinyapps.io/pulso-vcc/"
.MESES_MIN <- c("enero","febrero","marzo","abril","mayo","junio",
                "julio","agosto","septiembre","octubre","noviembre","diciembre")

.hay <- function(x) !is.null(x) && length(x) == 1 && !is.na(x)
.pct <- function(x) gsub(".", ",", sprintf("%+.2f%%", x * 100), fixed = TRUE)
.num3 <- function(x) gsub(".", ",", sprintf("%.3f", x), fixed = TRUE)
.fecha_larga <- function(d) sprintf("%d de %s", as.integer(format(d, "%d")), .MESES_MIN[as.integer(format(d, "%m"))])
.es_fin_de_mes <- function(fc) {              # ¿el siguiente dia habil (lun-vie) cae en otro mes?
  sig <- fc + 1; while (format(sig, "%u") %in% c("6", "7")) sig <- sig + 1
  format(sig, "%m") != format(fc, "%m")
}
.unir <- function(partes, final = ", ") {     # "a", "a y b", "a y b, c"
  partes <- partes[nzchar(partes)]
  if (!length(partes)) return("")
  if (length(partes) == 1) return(partes)
  paste0(partes[1], " y ", partes[2], if (length(partes) > 2) paste0(final, paste(partes[-(1:2)], collapse = final)) else "")
}

#' Frase de un indice: "el S&P 500 retrocedió -0,71% WTD y cerró septiembre con un
#' retorno de -0,45%, acumulando un avance de +11,77% YTD". `v` (0,1,2...) alterna
#' los verbos para que los parrafos no queden repetitivos. "" si no hay ningun dato.
.frase_indice <- function(sujeto, x, mes, fin_mes, v = 0) {
  sem <- c("WTD", "semanal", "durante la semana")[v %% 3 + 1]
  p_wtd <- if (.hay(x$wtd)) paste(
    if (x$wtd >= 0) c("avanzó", "subió")[v %% 2 + 1] else c("retrocedió", "cayó")[v %% 2 + 1],
    .pct(x$wtd), sem) else ""
  sust <- function(val, neutro) if (neutro) "retorno" else if (val >= 0) "avance" else "retroceso"
  p_mes <- if (.hay(x$mtd)) {
    if (fin_mes) paste0("cerró ", mes, " con un ", sust(x$mtd, v %% 2 == 0), " de ", .pct(x$mtd))
    else paste0(c("registra", "lleva")[v %% 2 + 1], " en ", mes, " un ", sust(x$mtd, v %% 2 == 0), " de ", .pct(x$mtd))
  } else ""
  p_ytd <- if (.hay(x$ytd)) paste0(
    c("acumulando un ", "alcanzando un ", "acumulando ")[v %% 3 + 1],
    c(paste0(sust(x$ytd, FALSE), " de "), "retorno de ", "")[v %% 3 + 1], .pct(x$ytd), " YTD") else ""
  cuerpo <- .unir(c(p_wtd, p_mes))
  if (!nzchar(cuerpo) && !nzchar(p_ytd)) return("")
  if (!nzchar(cuerpo)) return(paste0(sujeto, " acumula ", .pct(x$ytd), " YTD"))
  paste0(sujeto, " ", cuerpo, if (nzchar(p_ytd)) paste0(", ", p_ytd) else "")
}
.may <- function(s) paste0(toupper(substr(s, 1, 1)), substr(s, 2, nchar(s)))
.parrafo <- function(frases) {                # une frases no vacias en un parrafo
  frases <- frases[nzchar(frases)]
  if (!length(frases)) return("")
  paste0(paste(vapply(frases, .may, ""), collapse = ". "), ".")
}

#' Texto del correo diario.
#' @param pd salida de preparar_pd() (ver R/preparar.R)
#' @return list(asunto, parrafos = character(), fecha_cierre = Date)
texto_correo <- function(pd) {
  fc <- pd$fecha_cierre_date
  mes <- .MESES_MIN[as.integer(format(fc, "%m"))]; fin_mes <- .es_fin_de_mes(fc)
  idx <- function(nombre) { for (x in pd$indices_macro) if (identical(x$nombre, nombre)) return(x); list() }

  # 1) Renta variable: EE.UU., global y local
  p1 <- .parrafo(c(
    { f <- .frase_indice("el S&P 500", idx("S&P500"), mes, fin_mes, 0); if (nzchar(f)) paste("En Estados Unidos,", f) else "" },
    { f <- .frase_indice("el Nasdaq", idx("Nasdaq"), mes, fin_mes, 1); if (nzchar(f)) paste("Por su parte,", f) else "" },
    { f <- .frase_indice("el ACWI", idx("ACWI"), mes, fin_mes, 2); if (nzchar(f)) paste("A nivel global,", f) else "" },
    { f <- .frase_indice("el IPSA", idx("IPSA"), mes, fin_mes, 5); if (nzchar(f)) paste("En el mercado local,", f) else "" }))

  # 2) Commodities
  p2 <- .parrafo(c(
    { f <- .frase_indice("el oro", idx("Oro"), mes, fin_mes, 0); if (nzchar(f)) paste("En el ámbito macro,", f) else "" },
    .frase_indice("el cobre", idx("Cobre"), mes, fin_mes, 1),
    { f <- .frase_indice("el petróleo Brent", idx("Petroleo Brent"), mes, fin_mes, 3); if (nzchar(f)) paste("Por su parte,", f) else "" }))

  # 3) Treasury 10 años (nivel hace 7 dias -> cierre; y contra el cierre del año anterior)
  p3 <- ""
  r <- if (!is.null(pd$rf_usa) && length(pd$rf_usa$rows)) pd$rf_usa$rows[[1]] else NULL
  if (!is.null(r) && .hay(r$abr20) && .hay(r$abr27)) {
    var <- r$abr27 - r$abr20
    verbo <- if (abs(var) < 0.0005) "se mantuvo en" else if (var > 0) "aumentó desde" else "bajó desde"
    p3 <- if (abs(var) < 0.0005)
      sprintf("En renta fija internacional, la tasa del Treasury a 10 años se mantuvo en %s entre el %s y el %s.",
              .num3(r$abr27), .fecha_larga(fc - 7), .fecha_larga(fc))
    else sprintf(paste0("En renta fija internacional, la tasa del Treasury a 10 años %s %s el %s hasta %s al %s, ",
                        "registrando una variación semanal de %s puntos porcentuales."),
                 verbo, .num3(r$abr20), .fecha_larga(fc - 7), .num3(r$abr27), .fecha_larga(fc),
                 gsub(".", ",", sprintf("%+.3f", var), fixed = TRUE))
    if (.hay(r$cierre2025)) {
      d <- r$abr27 - r$cierre2025; anio <- as.integer(format(fc, "%Y")) - 1
      pos <- if (abs(d) < 0.05) "en línea con" else paste0(if (abs(d) >= 0.5) "significativamente " else "",
                                                           if (d > 0) "por sobre" else "por debajo de")
      p3 <- paste0(p3, sprintf(" De esta forma, la tasa se mantiene %s el cierre de %d, ubicado en %s.",
                               pos, anio, .num3(r$cierre2025)))
    }
  }

  # 4) Instrumentos de la casa (categoria "De la Casa"): mes y YTD
  p4 <- ""
  casa <- Filter(function(c) grepl("casa", c$nombre, ignore.case = TRUE), pd$categorias)
  if (length(casa)) {
    fr <- character(0); k <- 0
    for (f in casa[[1]]$fondos) {
      if (!.hay(f$mtd) && !.hay(f$ytd)) next
      nom <- sub("^Amc ", "AMC ", f$nombre)
      cuando <- if (k == 0) "el mes" else mes
      sust <- if (k %% 2 == 0) "retorno" else if (f$mtd >= 0) "avance" else "retroceso"
      a <- if (!.hay(f$mtd)) "" else if (fin_mes) paste0("cerró ", cuando, " con un ", sust, " de ", .pct(f$mtd))
           else paste0("acumula en ", cuando, " un ", sust, " de ", .pct(f$mtd))
      b <- if (!.hay(f$ytd)) "" else if (nzchar(a) && !fin_mes) paste0("un ", .pct(f$ytd), " YTD")
           else paste0("acumula un ", .pct(f$ytd), " YTD")
      fr <- c(fr, paste0(if (k == 1) "Por su parte, " else if (k > 1) "A su vez, " else "", nom, " ", .unir(c(a, b))))
      k <- k + 1
    }
    if (length(fr)) { fr[1] <- paste("En los instrumentos de la casa,", fr[1]); p4 <- paste0(paste(fr, collapse = ". "), ".") }
  }

  cierre_txt <- sprintf("%s de %d", .fecha_larga(fc), as.integer(format(fc, "%Y")))
  list(
    asunto = paste("Pulso VCC | Cierre", cierre_txt),
    fecha_cierre = fc,
    parrafos = Filter(nzchar, c(p1, p2, p3, p4)),
    bloques = Filter(function(b) nzchar(b$texto), list(
      list(titulo = "Renta variable", texto = p1), list(titulo = "Commodities", texto = p2),
      list(titulo = "Renta fija internacional", texto = p3), list(titulo = "Instrumentos de la casa", texto = p4))),
    cierre = c(
      "Recuerden que pueden acceder pinchando el siguiente enlace, {LINK}, el cual se actualiza automáticamente todos los días a las 11:00 AM. Les recomendamos agregarlo a sus favoritos o crear un acceso directo en sus computadores para un acceso más rápido.",
      "Ante cualquier duda o consulta, no duden en escribirme.",
      sprintf("Adjuntamos reporte de rentabilidad de instrumentos aprobados por comité de inversión al %s.", format(fc, "%d/%m/%Y")))
  )
}

.esc <- function(x) { x <- gsub("&", "&amp;", x, fixed = TRUE); x <- gsub("<", "&lt;", x, fixed = TRUE); gsub(">", "&gt;", x, fixed = TRUE) }

#' Cuerpo del correo en el formato de correo de la skill vizcaya-afi-diseno: 640 px,
#' tablas con estilos en linea (Outlook y Gmail), encabezado navy con logo, kicker
#' arena, un bloque con filete arena por tema, Georgia + Arial, paleta cerrada.
#' `logo_src`: "cid:logo-pulso" para enviar (Gmail bloquea data:) o "data:image/png;base64,..."
#' para la vista previa. Devuelve list(html, texto).
.GEORGIA <- "Georgia,'Times New Roman',serif"; .ARIAL <- "Arial,Helvetica,sans-serif"
.p_cuerpo <- function(txt, extra = "")
  paste0("<p align=\"justify\" style=\"margin:0;font-family:", .GEORGIA, ";font-size:15px;line-height:24px;color:#2b3340;",
         "text-align:justify;mso-line-height-rule:exactly;", extra, "\">", txt, "</p>")

cuerpo_correo <- function(t, firma = "", logo_src = "cid:logo-pulso") {
  link <- sprintf("<a href=\"%s\" style=\"color:#355470;font-weight:bold;\">Pulso VCC</a>", URL_PULSO)
  html_txt <- function(p) sub("{LINK}", link, .esc(p), fixed = TRUE)
  bloque <- function(b) paste0(
    "<tr><td style=\"padding:0 40px 28px 40px;\"><table role=\"presentation\" width=\"100%\" cellpadding=\"0\" cellspacing=\"0\" border=\"0\"><tr>",
    "<td style=\"border-left:3px solid #a89880;padding:0 0 0 18px;\">",
    "<p style=\"margin:0 0 8px 0;font-family:", .ARIAL, ";font-size:13px;line-height:18px;font-weight:bold;letter-spacing:1px;color:#182b46;text-transform:uppercase;\"><font color=\"#182b46\">",
    .esc(b$titulo), "</font></p>", .p_cuerpo(html_txt(b$texto)), "</td></tr></table></td></tr>")
  fila <- function(p, ultimo = FALSE) paste0("<tr><td style=\"padding:0 40px ", if (ultimo) "30" else "14", "px 40px;\">", .p_cuerpo(html_txt(p)), "</td></tr>")
  cierre <- vapply(seq_along(t$cierre), function(i) fila(t$cierre[i]), "")
  fecha_larga <- sprintf("%s de %d", .fecha_larga(t$fecha_cierre), as.integer(format(t$fecha_cierre, "%Y")))

  html <- paste0(
    "<!doctype html><html lang=\"es\"><head><meta charset=\"utf-8\"><meta name=\"viewport\" content=\"width=device-width,initial-scale=1\">",
    "<meta http-equiv=\"X-UA-Compatible\" content=\"IE=edge\"><title>", .esc(t$asunto), "</title>",
    "<style>p{margin:0;} table{border-collapse:collapse;mso-table-lspace:0pt;mso-table-rspace:0pt;} img{border:0;-ms-interpolation-mode:bicubic;}</style></head>",
    "<body bgcolor=\"#f1f1f1\" style=\"margin:0;padding:0;background-color:#f1f1f1;\">",
    "<table role=\"presentation\" width=\"100%\" cellpadding=\"0\" cellspacing=\"0\" border=\"0\" bgcolor=\"#f1f1f1\"><tr><td align=\"center\" style=\"padding:24px 12px;\">",
    "<table role=\"presentation\" width=\"640\" cellpadding=\"0\" cellspacing=\"0\" border=\"0\" bgcolor=\"#ffffff\" style=\"width:640px;background-color:#ffffff;\">",
    # 1. encabezado navy
    "<tr><td bgcolor=\"#182b46\" style=\"background-color:#182b46;padding:0;\"><table role=\"presentation\" width=\"100%\" cellpadding=\"0\" cellspacing=\"0\" border=\"0\" bgcolor=\"#182b46\"><tr>",
    "<td bgcolor=\"#182b46\" valign=\"middle\" style=\"background-color:#182b46;padding:28px 0 28px 40px;font-family:", .ARIAL, ";font-size:12px;letter-spacing:2px;color:#ffffff;text-transform:uppercase;\"><font color=\"#ffffff\">Pulso VCC &middot; Fondos aprobados</font></td>",
    "<td bgcolor=\"#182b46\" align=\"right\" valign=\"middle\" width=\"230\" style=\"background-color:#182b46;padding:24px 40px 24px 0;\"><img src=\"", logo_src,
    "\" alt=\"Vizcaya Capital\" width=\"190\" height=\"50\" style=\"display:block;border:0;width:190px;height:50px;\"></td></tr></table></td></tr>",
    # 2. kicker arena
    "<tr><td style=\"padding:40px 40px 22px 40px;\"><p style=\"margin:0;font-family:", .ARIAL, ";font-size:12px;line-height:18px;letter-spacing:2px;color:#a89880;text-transform:uppercase;\"><font color=\"#a89880\">Reporte diario &middot; Cierre ",
    .esc(fecha_larga), "</font></p></td></tr>",
    # 3. un bloque por tema
    paste(vapply(t$bloques, bloque, ""), collapse = ""),
    # 4. cierre (enlace, consultas, adjunto)
    paste(cierre, collapse = ""),
    if (nzchar(firma)) fila(firma, ultimo = TRUE) else "<tr><td height=\"16\" style=\"font-size:1px;line-height:16px;\">&nbsp;</td></tr>",
    # 5. pie
    "<tr><td bgcolor=\"#e2ebf2\" style=\"background-color:#e2ebf2;padding:20px 40px;\"><p align=\"justify\" style=\"margin:0;font-family:", .ARIAL,
    ";font-size:11px;line-height:16px;color:#485a66;text-align:justify;\">Confidencial. Este correo est&aacute; dirigido exclusivamente al equipo de Vizcaya Capital. Rentabilidades pasadas no garantizan resultados futuros.</p></td></tr>",
    "</table></td></tr></table></body></html>")
  list(html = html,
       texto = paste(c(t$asunto, sub("{LINK}", paste0("Pulso VCC (", URL_PULSO, ")"), c(t$parrafos, t$cierre, if (nzchar(firma)) firma), fixed = TRUE)), collapse = "\n\n"))
}
