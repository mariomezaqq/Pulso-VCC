# =============================================================================
# RENDER DEL DASHBOARD (portado 1:1 desde generar_dashboard_pulso_vcc.py)
# Mismo HTML/CSS/pestanias. Agrega la columna "Mes anterior" (mes calendario).
# render_dashboard_html(pd, logo_b64) -> string HTML completo.
# =============================================================================

# ---- Formato ----
.fmt_pct_frac <- function(v) {
  if (is.null(v) || length(v) == 0 || is.na(v)) return("&mdash;")
  v <- suppressWarnings(as.numeric(v)); if (is.na(v)) return("&mdash;")
  sprintf("%+.2f%%", v * 100)
}
.fmt_pct_str <- function(v) {
  s <- trimws(as.character(v))
  if (length(s) == 0 || s %in% c("nan","","None","—","NA")) return("&mdash;")
  s2 <- gsub(",", ".", gsub("%", "", s))
  num <- suppressWarnings(as.numeric(s2))
  if (is.na(num)) return(s)
  sprintf("%+.2f%%", num)
}
.cls_frac <- function(v) {
  v <- suppressWarnings(as.numeric(v))
  if (is.na(v)) "neutral" else if (v >= 0) "pos" else "neg"
}
.cls_str <- function(v) {
  s <- gsub(",", ".", gsub("%", "", trimws(as.character(v))))
  num <- suppressWarnings(as.numeric(s))
  if (is.na(num)) "neutral" else if (num >= 0) "pos" else "neg"
}
.raw <- function(v) {
  s <- trimws(as.character(v))
  if (length(s) == 0 || s %in% c("nan","","None","0","NA")) "&mdash;" else s
}
.avg_frac <- function(fondos, key) {
  vals <- vapply(fondos, function(f) suppressWarnings(as.numeric(f[[key]])), numeric(1))
  vals <- vals[!is.na(vals)]
  if (length(vals)) mean(vals) else NA_real_
}
.avg_str_pct <- function(fondos, key) {
  vals <- vapply(fondos, function(f) {
    s <- gsub(",", ".", gsub("%", "", trimws(as.character(f[[key]]))))
    suppressWarnings(as.numeric(s))
  }, numeric(1))
  vals <- vals[!is.na(vals)]
  if (length(vals)) mean(vals) else NA_real_
}

# ---- Builders de tabla (10 columnas: + "Mes ant.") ----
.thead <- function(mes_lbl) {
  paste0("<thead><tr>",
    "<th class='th-left'>Fondo</th>",
    "<th class='th-center'>MTD</th>",
    "<th class='th-center'>", mes_lbl, "</th>",
    "<th class='th-center'>YTD</th>",
    "<th class='th-center'>2025</th><th class='th-center'>2024</th>",
    "<th class='th-center'>Duraci&oacute;n</th><th class='th-center'>Liquidez</th>",
    "<th class='th-center'>Moneda</th><th class='th-center'>TAC</th>",
    "</tr></thead>")
}
.row <- function(f) {
  stale <- if (isTRUE(f$atrasado))
    paste0("<span class='stale-mark' title='Sin el cierre del grupo'>&#9888; ", f$fecha_dato, "</span>") else ""
  paste0("<tr>",
    "<td class='nombre'>", f$nombre, stale, "</td>",
    "<td class='center ", .cls_frac(f$mtd),     "'>", .fmt_pct_frac(f$mtd),     "</td>",
    "<td class='center ", .cls_frac(f$mes_ant), "'>", .fmt_pct_frac(f$mes_ant), "</td>",
    "<td class='center ", .cls_frac(f$ytd),     "'>", .fmt_pct_frac(f$ytd),     "</td>",
    "<td class='center ", .cls_str(f$rent2025), "'>", .fmt_pct_str(f$rent2025), "</td>",
    "<td class='center ", .cls_str(f$rent2024), "'>", .fmt_pct_str(f$rent2024), "</td>",
    "<td class='center secondary'>", .raw(f$duracion), "</td>",
    "<td class='secondary'>", .raw(f$liquidez), "</td>",
    "<td class='center badge'>", .raw(f$moneda), "</td>",
    "<td class='center secondary'>", .raw(f$tac), "</td>",
    "</tr>")
}
.avg_row <- function(fondos) {
  fp <- function(v, frac = TRUE) {
    if (is.na(v)) return("&mdash;")
    if (frac) sprintf("%+.2f%%", v * 100) else sprintf("%+.2f%%", v)
  }
  paste0("<tr class='avg-row'>",
    "<td class='avg-label'>Promedio</td>",
    "<td class='center avg-val'>", fp(.avg_frac(fondos,"mtd")),     "</td>",
    "<td class='center avg-val'>", fp(.avg_frac(fondos,"mes_ant")), "</td>",
    "<td class='center avg-val'>", fp(.avg_frac(fondos,"ytd")),     "</td>",
    "<td class='center avg-val'>", fp(.avg_str_pct(fondos,"rent2025"), FALSE), "</td>",
    "<td class='center avg-val'>", fp(.avg_str_pct(fondos,"rent2024"), FALSE), "</td>",
    "<td></td><td></td><td></td><td></td>",
    "</tr>")
}
.group_header <- function(nombre, mes_lbl) {
  sub <- paste0("<tr class='sub-header'>",
    "<th class='th-left'>Fondo</th>",
    "<th class='th-center'>MTD</th><th class='th-center'>", mes_lbl, "</th>",
    "<th class='th-center'>YTD</th>",
    "<th class='th-center'>2025</th><th class='th-center'>2024</th>",
    "<th class='th-center'>Duraci&oacute;n</th><th class='th-center'>Liquidez</th>",
    "<th class='th-center'>Moneda</th><th class='th-center'>TAC</th>",
    "</tr>")
  paste0("<tr class='group-header'><td class='group-label' colspan='10'>", nombre, "</td></tr>", sub)
}

.panel_macro <- function(pd) {
  h <- "<div class='tab-panel' id='panel-macro' style='display:none'>"
  h <- paste0(h, "<h2 class='cat-title'>Índices Macro</h2><div class='table-wrap'><table>",
    "<thead><tr><th class='th-left'>Índice</th>",
    "<th class='th-center'>WTD</th><th class='th-center'>MTD</th><th class='th-center'>YTD</th>",
    "</tr></thead><tbody>")
  for (idx in pd$indices_macro) {
    h <- paste0(h, "<tr><td class='nombre'>", idx$nombre, "</td>",
      "<td class='center ", .cls_frac(idx$wtd), "'>", .fmt_pct_frac(idx$wtd), "</td>",
      "<td class='center ", .cls_frac(idx$mtd), "'>", .fmt_pct_frac(idx$mtd), "</td>",
      "<td class='center ", .cls_frac(idx$ytd), "'>", .fmt_pct_frac(idx$ytd), "</td></tr>")
  }
  h <- paste0(h, "</tbody></table></div>")
  rf <- pd$rf_usa
  if (!is.null(rf) && length(rf$rows) > 0) {
    h <- paste0(h, "<h2 class='cat-title' style='margin-top:32px'>Renta Fija USA</h2><div class='table-wrap'><table>",
      "<thead><tr><th class='th-left'>Índice</th>",
      "<th class='th-center'>", rf$headers$cierre, "</th>",
      "<th class='th-center'>", rf$headers$fecha1, "</th>",
      "<th class='th-center'>", rf$headers$fecha2, "</th>",
      "<th class='th-center'>Var. semana</th></tr></thead><tbody>")
    for (r in rf$rows) {
      var <- if (!is.null(r$abr27) && !is.null(r$abr20) && !is.na(r$abr27) && !is.na(r$abr20)) r$abr27 - r$abr20 else NA_real_
      var_str <- if (!is.na(var)) sprintf("%+.3f", var) else "&mdash;"
      var_cls <- if (is.na(var)) "neutral" else if (var >= 0) "pos" else "neg"
      cn <- function(x) if (is.null(x) || is.na(x)) "&mdash;" else sprintf("%.3f", x)
      h <- paste0(h, "<tr><td class='nombre'>", r$nombre, "</td>",
        "<td class='center secondary'>", cn(r$cierre2025), "</td>",
        "<td class='center secondary'>", cn(r$abr20), "</td>",
        "<td class='center secondary'>", cn(r$abr27), "</td>",
        "<td class='center ", var_cls, "'>", var_str, "</td></tr>")
    }
    h <- paste0(h, "</tbody></table></div>")
  }
  paste0(h, "</div>")
}

# ---- CSS (identico al original) ----
.CSS <- paste0(
  "@import url('https://fonts.googleapis.com/css2?family=Lato:wght@300;400;700&family=Playfair+Display:wght@600&display=swap');",
  "*,*::before,*::after{box-sizing:border-box;margin:0;padding:0}",
  ":root{--bg:#f0f2f5;--surface:#ffffff;--surface2:#f7f8fa;--border:#dde1e8;",
  "--text:#1a2233;--muted:#7a869a;--accent:#1b2d5b;--accent-light:#e8edf5;",
  "--pos:#1a7f4b;--pos-bg:#eaf6f0;--neg:#c0392b;--neg-bg:#fdf0ef;--radius:8px}",
  "body{background:var(--bg);color:var(--text);font-family:'Lato',sans-serif;font-size:14px;line-height:1.5;min-height:100vh}",
  "header{background:var(--surface);border-bottom:1px solid var(--border);",
  "padding:20px 40px;display:flex;align-items:center;gap:24px;box-shadow:0 1px 4px rgba(0,0,0,.06)}",
  ".logo-img{height:48px;width:auto;flex-shrink:0}",
  ".header-divider{width:1px;height:40px;background:var(--border);flex-shrink:0}",
  ".header-text{flex:1}",
  ".brand{font-family:'Playfair Display',serif;font-size:18px;color:var(--accent);letter-spacing:.3px;font-weight:600}",
  ".subtitle{font-size:11px;color:var(--muted);margin-top:2px;text-transform:uppercase;letter-spacing:1.5px}",
  ".header-meta{text-align:right;font-size:12px;color:var(--muted)}",
  ".fecha{font-size:14px;font-weight:700;color:var(--accent);margin-bottom:2px}",
  ".toolbar{background:var(--surface);border-bottom:2px solid var(--border);padding:0 40px;box-shadow:0 1px 3px rgba(0,0,0,.04)}",
  ".tab-bar{display:flex;flex-wrap:wrap;gap:0}",
  ".tab-btn{background:none;border:none;border-bottom:3px solid transparent;",
  "padding:14px 18px;font-family:'Lato',sans-serif;font-size:12px;font-weight:700;",
  "color:var(--muted);cursor:pointer;white-space:nowrap;text-transform:uppercase;",
  "letter-spacing:.8px;transition:color .15s,border-color .15s,background .15s;margin-bottom:-2px}",
  ".tab-btn:hover{color:var(--accent);background:var(--accent-light)}",
  ".tab-btn.active{color:var(--accent);border-color:var(--accent);background:var(--accent-light)}",
  "main{padding:32px 40px;max-width:1500px;margin:0 auto}",
  ".cat-title{font-family:'Playfair Display',serif;font-size:18px;color:var(--accent);",
  "margin-bottom:14px;padding-bottom:10px;border-bottom:2px solid var(--accent-light)}",
  ".table-wrap{overflow-x:auto;border-radius:var(--radius);border:1px solid var(--border);box-shadow:0 1px 4px rgba(0,0,0,.05)}",
  "table{width:100%;border-collapse:collapse;font-size:13px}",
  "thead th{background:var(--accent);color:#ffffff;font-weight:700;",
  "padding:11px 16px;font-size:11px;text-transform:uppercase;letter-spacing:.8px;white-space:nowrap}",
  "thead th.th-left{text-align:left;min-width:210px}",
  "thead th.th-center{text-align:center}",
  "tbody tr{border-bottom:1px solid var(--border);background:var(--surface)}",
  "tbody tr:nth-child(even){background:var(--surface2)}",
  "tbody tr:last-child{border-bottom:none}",
  "tbody tr:hover{background:var(--accent-light)}",
  "tbody td{padding:10px 16px;color:var(--text);white-space:nowrap;text-align:left}",
  "tbody td.center{text-align:center}",
  "tbody td.nombre{text-align:left;font-weight:600;color:var(--accent)}",
  "tbody td.secondary{color:var(--muted);font-size:12px}",
  "tbody td.pos{color:var(--pos);font-weight:700;background:var(--pos-bg)}",
  "tbody td.neg{color:var(--neg);font-weight:700;background:var(--neg-bg)}",
  "tbody td.neutral{color:var(--muted)}",
  "tbody td.badge{font-size:11px;font-weight:700;color:var(--accent);text-transform:uppercase;",
  "letter-spacing:.5px;background:var(--accent-light);border-radius:4px}",
  "tr.group-header{background:var(--accent)!important}",
  "tr.sub-header th{background:var(--accent);color:rgba(255,255,255,0.65);font-weight:700;",
  "padding:7px 16px;font-size:10px;text-transform:uppercase;letter-spacing:.8px;white-space:nowrap;",
  "border-top:1px solid rgba(255,255,255,0.2)}",
  "td.group-label{text-align:left;padding:7px 16px;font-size:11px;font-weight:700;",
  "color:#ffffff;text-transform:uppercase;letter-spacing:1.2px}",
  "tr.avg-row{background:#e9ecef!important;border-top:2px solid #ced4da}",
  "tr.avg-row:hover{background:#dee2e6!important}",
  "td.avg-label{text-align:left;font-weight:700;font-size:12px;color:var(--accent);",
  "text-transform:uppercase;letter-spacing:.8px;padding:10px 16px}",
  "td.avg-val{font-weight:700;font-size:13px;color:var(--accent)}",
  "footer{text-align:center;padding:24px;font-size:11px;color:var(--muted);",
  "border-top:1px solid var(--border);margin-top:20px}",
  ".alerta{margin:16px 40px 0;padding:12px 18px;border-radius:var(--radius);font-size:13px;",
  "background:#fff8e1;border:1px solid #f0d27a;color:#7a5b00;display:flex;gap:10px;align-items:flex-start}",
  ".alerta .ico{font-weight:700;font-size:16px;line-height:1.2}",
  ".alerta strong{color:#5c4500}",
  ".stale-mark{color:#b8860b;font-size:11px;font-weight:700;margin-left:6px}",
  ".btn-actualizar{background:var(--accent);color:#fff;border:none;border-radius:6px;",
  "padding:5px 11px;font-family:'Lato',sans-serif;font-size:11px;font-weight:700;cursor:pointer;",
  "letter-spacing:.6px;text-transform:uppercase;margin-bottom:6px;transition:background .15s}",
  ".btn-actualizar:hover{background:#16244a}",
  ".gen{font-size:11px;color:var(--muted);margin-top:2px}",
  "@media(max-width:900px){",
  "header{padding:16px 20px;flex-wrap:wrap}.alerta{margin:12px 16px 0}",
  ".toolbar{padding:0 16px}main{padding:20px 16px}.header-meta{text-align:left}}"
)

# ---- Banner de alertas (datos desactualizados) ----
.banner_alertas <- function(pd) {
  al <- pd$alertas
  if (is.null(al)) return("")
  has_g <- !is.null(al$global); has_f <- length(al$fondos) > 0
  if (!has_g && !has_f) return("")
  msg <- "<div class='alerta'><span class='ico'>&#9888;</span><div>"
  if (has_g) msg <- paste0(msg, "<strong>", al$global, "</strong><br>")
  if (has_f) {
    nombres <- vapply(al$fondos, function(x) paste0(x$nombre, " (", x$fecha, ")"), character(1))
    msg <- paste0(msg, "<strong>Fondos sin el cierre del grupo:</strong> ",
                  paste(nombres, collapse = ", "), ".")
  }
  paste0(msg, "</div></div>")
}

#' Devuelve el HTML completo del dashboard.
render_dashboard_html <- function(pd, logo_b64 = NULL, generado = NULL) {
  mes_lbl <- pd$mes_ant_label %||% "Mes ant."
  cats <- pd$categorias

  # Panel resumen (todas las categorias con group headers)
  resumen_body <- ""
  for (cat in cats) {
    resumen_body <- paste0(resumen_body, .group_header(cat$nombre, mes_lbl))
    for (f in cat$fondos) resumen_body <- paste0(resumen_body, .row(f))
  }
  panel_resumen <- paste0(
    "<div class='tab-panel' id='panel-0' style='display:block'>",
    "<h2 class='cat-title'>Resumen &mdash; Todos los Fondos</h2>",
    "<div class='table-wrap'><table>", .thead(mes_lbl),
    "<tbody>", resumen_body, "</tbody></table></div></div>")

  # Paneles por categoria
  cat_panels <- ""
  for (i in seq_along(cats)) {
    rows <- paste0(paste0(vapply(cats[[i]]$fondos, .row, character(1)), collapse = ""),
                   .avg_row(cats[[i]]$fondos))
    cat_panels <- paste0(cat_panels,
      "<div class='tab-panel' id='panel-", i, "' style='display:none'>",
      "<h2 class='cat-title'>", cats[[i]]$nombre, "</h2>",
      "<div class='table-wrap'><table>", .thead(mes_lbl),
      "<tbody>", rows, "</tbody></table></div></div>")
  }

  n_cat <- length(cats)
  all_ids <- paste0("[\"panel-0\",\"panel-macro\",",
                    paste0(sprintf("\"panel-%d\"", seq_len(n_cat)), collapse = ","), "]")

  tab_btns <- "<button class='tab-btn active' onclick='showTab(\"panel-0\",this)'>Resumen</button>"
  tab_btns <- paste0(tab_btns, "<button class='tab-btn' onclick='showTab(\"panel-macro\",this)'>Macro</button>")
  for (i in seq_along(cats)) {
    short <- cats[[i]]$nombre
    short <- gsub("Renta Fija", "RF", short); short <- gsub("Renta Variable", "RV", short)
    short <- gsub("Alternativos", "Alt.", short); short <- gsub("De la Casa", "Casa", short)
    short <- gsub("Comparables Albion", "Comp. Albion", short)
    tab_btns <- paste0(tab_btns, "<button class='tab-btn' onclick='showTab(\"panel-", i, "\",this)'>", short, "</button>")
  }

  js <- paste0("var ALL_IDS=", all_ids, ";",
    "function showTab(id,btn){",
    "for(var i=0;i<ALL_IDS.length;i++){document.getElementById(ALL_IDS[i]).style.display='none';}",
    "document.getElementById(id).style.display='block';",
    "var b=document.querySelectorAll('.tab-btn');",
    "for(var j=0;j<b.length;j++){b[j].classList.remove('active');}",
    "btn.classList.add('active');}")

  logo_html <- if (!is.null(logo_b64) && nzchar(logo_b64))
    paste0("<img class='logo-img' src='data:image/jpeg;base64,", logo_b64, "' alt='Vizcaya Capital'>") else ""

  paste0(
    "<!DOCTYPE html><html lang='es'><head>",
    "<meta charset='UTF-8'><meta name='viewport' content='width=device-width,initial-scale=1.0'>",
    "<title>Pulso VCC | Vizcaya Capital</title>",
    "<style>", .CSS, "</style></head><body>",
    "<header>", logo_html,
    "<div class='header-divider'></div>",
    "<div class='header-text'><div class='brand'>Pulso VCC</div>",
    "<div class='subtitle'>Fondos Aprobados &mdash; Comit&eacute; de Inversiones</div></div>",
    "<div class='header-meta'>",
    "<button class='btn-actualizar' onclick=\"window.parent.postMessage('pulso-actualizar','*')\">&#x21bb; Actualizar</button>",
    "<div class='fecha'>Cierre ", pd$fecha_cierre, "</div>",
    if (!is.null(generado) && nzchar(generado)) paste0("<div class='gen'>Datos: ", generado, " (Chile)</div>") else "",
    "</div></header>",
    .banner_alertas(pd),
    "<div class='toolbar'><div class='tab-bar'>", tab_btns, "</div></div>",
    "<main>", panel_resumen, .panel_macro(pd), cat_panels, "</main>",
    "<footer>Vizcaya Capital &nbsp;&bull;&nbsp; Uso interno &nbsp;&bull;&nbsp; ",
    "Datos CMF, BCCh y Yahoo Finance</footer>",
    "<script>", js, "</script></body></html>")
}

if (!exists("%||%")) `%||%` <- function(a, b) if (is.null(a)) b else a
