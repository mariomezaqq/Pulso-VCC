# =============================================================================
# PULSO VCC — Shiny app (dashboard + administracion)
# - Dashboard: lee data/series_vc.rds (lo refresca el cron diario 11:00 Chile)
#   y dibuja el mismo dashboard del Comite + columna "Mes anterior" + alertas.
# - Administracion (abierta): subir dividendos (persistente), ingresar el VC
#   diario de fondos/indices manuales (la rent se calcula sola y se persiste),
#   corregir dividendos, descargar file_show.xlsx. Todo persiste en GitHub.
# =============================================================================
suppressMessages({
  library(shiny); library(bslib); library(DT)
  library(dplyr); library(lubridate); library(tibble); library(openxlsx); library(jsonlite)
})

# La app NO scrapea (eso lo hace el cron); por eso no carga scraper.R/indices.R
# (evita quantmod/rvest en el deploy). Solo lo que el dashboard necesita.
# github_store + admin_fondos van PRIMERO: asi en la nube bajamos la ultima lista
# curada de GitHub ANTES de armar los fondos (fondos_pulso.R la lee del disco).
source("R/github_store.R")
source("R/admin_fondos.R")
try(store_sync("fondos_curados.csv"), silent = TRUE)
for (f in c("fondos_pulso","calculos","dividendos","ajustes",
            "compute","ui_dashboard","excel_export"))
  source(file.path("R", paste0(f, ".R")))

# Folleto CMF (para autocompletar rent/TAC al agregar un fondo). Token-free, pero
# usa rvest/pdftools; si no cargan en el deploy, la app sigue y el boton avisa.
FOLLETO_OK <- tryCatch({ source("R/folletos.R"); source("R/folletos_datos.R"); TRUE },
                       error = function(e) { message("[app] folletos no disponible: ", e$message); FALSE })

# Token de GitHub para escritura (no se versiona; se incluye en el deploy)
if (file.exists("R/secret_token.R")) try(source("R/secret_token.R"), silent = TRUE)

DATA_RDS <- "data/series_vc.rds"
logo_b64 <- { p <- "www/logo.jpg"
  if (file.exists(p)) jsonlite::base64_enc(readBin(p, "raw", file.info(p)$size)) else NULL }

# Entidades manuales (fondos + indices) que se cargan a mano
ENTIDADES_MANUAL <- rbind(
  data.frame(Nombre = NOMBRES_FONDOS_MANUALES,  Tipo = "Fondo",  stringsAsFactors = FALSE),
  data.frame(Nombre = NOMBRES_INDICES_MANUALES, Tipo = "Indice", stringsAsFactors = FALSE)
)

# Catalogo CMF (comparador): fuente de run/serie/row correctos para agregar fondos
CATALOGO <- tryCatch(cargar_catalogo(), error = function(e) NULL)

# ---- Lectura de inputs persistidos ----
.leer_csv <- function(rel, default_df) {
  ruta <- store_sync(rel)
  if (!is.null(ruta) && file.exists(ruta))
    tryCatch(read.csv(ruta, stringsAsFactors = FALSE, fileEncoding = "UTF-8",
                      check.names = FALSE, colClasses = c(Fecha = "character")),
             error = function(e) default_df)
  else default_df
}
default_vc <- function() data.frame(Nombre = character(), Tipo = character(),
                                    Fecha = character(), Valor = numeric(), stringsAsFactors = FALSE)
default_ov <- function() data.frame(Fondo = character(), `Fecha limite` = character(),
                                    Monto = numeric(), check.names = FALSE, stringsAsFactors = FALSE)

# Serie (fecha, valor_cuota) de una entidad manual desde la tabla larga de VC
serie_de_vc <- function(vc_df, nombre) {
  s <- vc_df[vc_df$Nombre == nombre, c("Fecha", "Valor"), drop = FALSE]
  if (!nrow(s)) return(NULL)
  out <- tibble(fecha = as.Date(s$Fecha), valor_cuota = suppressWarnings(as.numeric(s$Valor))) %>%
    filter(!is.na(fecha), !is.na(valor_cuota)) %>% arrange(fecha) %>% distinct(fecha, .keep_all = TRUE)
  if (nrow(out)) out else NULL
}
construir_series_manual <- function(vc_df) {
  res <- list()
  for (nm in NOMBRES_FONDOS_MANUALES) { s <- serie_de_vc(vc_df, nm); if (!is.null(s)) res[[nm]] <- s }
  res
}
# Inyecta WTD/MTD/YTD de los indices manuales en la lista macro (calculados del VC)
aplicar_indices_manuales <- function(macro, vc_df, fc) {
  ref_wtd <- fc - 7; ref_mtd <- floor_date(fc, "month") - 1
  ref_ytd <- as.Date(sprintf("%d-12-31", year(fc) - 1))
  for (nm in NOMBRES_INDICES_MANUALES) {
    s <- serie_de_vc(vc_df, nm); if (is.null(s)) next
    for (j in seq_along(macro)) if (identical(macro[[j]]$nombre, nm)) {
      macro[[j]]$wtd <- .rent(s, ref_wtd, fc)
      macro[[j]]$mtd <- .rent(s, ref_mtd, fc)
      macro[[j]]$ytd <- .rent(s, ref_ytd, fc)
    }
  }
  macro
}

preparar_todo <- function(vc_df, ov_df = NULL) {
  store_sync("series_vc.rds")   # en la nube baja el ultimo dato del cron; en local mantiene el archivo
  if (!file.exists(DATA_RDS)) return(NULL)
  datos <- readRDS(DATA_RDS)
  fc <- as.Date(datos$cierre)
  # Dividendos (FIRES/FINRE) + factor de reparto (RGFMU) -> valor_cuota_ajustado
  store_sync("dividendos.xlsx")
  ruta_div <- if (file.exists("data/dividendos.xlsx")) "data/dividendos.xlsx" else NULL
  div <- fusionar_overrides(cargar_dividendos_pulso(ruta_div), ov_df)
  series_aj <- enriquecer_series_ajustadas(datos$series, div)
  macro <- aplicar_indices_manuales(datos$macro, vc_df, fc)
  pd <- construir_pulso_data(series_aj, fecha_cierre = fc, macro = macro,
                             rf_usa = datos$rf_usa, series_manual = construir_series_manual(vc_df))
  list(pd = pd, datos = datos, series_aj = series_aj, div = div)
}
preparar_pd <- function(vc_df, ov_df = NULL) { t <- preparar_todo(vc_df, ov_df); if (is.null(t)) NULL else t$pd }

# ============================== UI ==============================
ui <- page_navbar(
  title = "Pulso VCC", theme = bs_theme(version = 5),
  nav_panel("Dashboard",
    tags$script(HTML("window.addEventListener('message',function(e){if(e&&e.data==='pulso-actualizar'){Shiny.setInputValue('actualizar',(window.__pulsoN=(window.__pulsoN||0)+1),{priority:'event'});}});")),
    uiOutput("dash")),
  nav_panel("Administración",
    div(style = "max-width:1100px;margin:20px auto;padding:0 16px",
      h4("Panel de administración"),
      p(class = "text-muted", "Los cambios se guardan y persisten (en GitHub cuando está configurado). El dashboard se recalcula al instante."),
      card(card_header("➕ Agregar fondo desde el catálogo CMF"),
        p(class = "text-muted", "Busca el fondo en el catálogo del comparador (trae run/serie/row correctos de la CMF). Elige su categoría, completa los datos que quieras mostrar y agrégalo. Se guarda y se dispara la consulta a la CMF; en ~5 min recarga y aparecen sus rentabilidades."),
        selectizeInput("cat_fondo", "Fondo (busca por nombre)", choices = NULL, width = "100%",
                       options = list(placeholder = "Escribe para buscar en el catálogo...", maxOptions = 50)),
        uiOutput("cat_preview"),
        layout_columns(col_widths = c(4,4,4),
          selectInput("cat_hoja", "Categoría (hoja)", choices = NULL),
          textInput("cat_hoja_nueva", "…o nueva categoría", ""),
          textInput("cat_nombre", "Nombre a mostrar", "")),
        div(style = "margin:4px 0 10px",
          actionButton("cat_autocompletar", "🔎 Autocompletar TAC desde folleto CMF", class = "btn-outline-secondary btn-sm"),
          span(class = "text-muted", style = "font-size:12px;margin-left:8px",
               "Baja el folleto de la CMF y rellena el TAC. Las rentabilidades 2024/2025 se calculan solas del valor cuota tras agregar."),
          verbatimTextOutput("cat_auto_msg")),
        layout_columns(col_widths = c(2,2,2,2,2,2),
          textInput("cat_rent2024", "Rent. 2024", ""),
          textInput("cat_rent2025", "Rent. 2025", ""),
          textInput("cat_duracion", "Duración", ""),
          textInput("cat_liquidez", "Liquidez", ""),
          selectInput("cat_moneda", "Moneda", choices = c("CLP","UF","USD")),
          textInput("cat_tac", "TAC", "")),
        div(style = "margin-top:8px", actionButton("cat_add", "Agregar fondo", class = "btn-primary")),
        verbatimTextOutput("cat_msg")),
      card(card_header("Fondos actuales (quitar)"),
        p(class = "text-muted", "Selecciona una fila y quítala. El cambio se guarda al instante."),
        DTOutput("tbl_cur"),
        div(style = "margin-top:8px", actionButton("cat_del", "Quitar fondo seleccionado", class = "btn-outline-danger")),
        verbatimTextOutput("cat_del_msg")),
      layout_columns(col_widths = c(6,6),
        card(card_header("Dividendos (Boletín Bolsa)"),
          fileInput("div_file", NULL, accept = ".xlsx", buttonLabel = "Elegir...", placeholder = "ningún archivo"),
          verbatimTextOutput("div_msg")),
        card(card_header("Descargar Excel del dashboard"),
          p("file_show.xlsx con los datos actuales."),
          downloadButton("dl_xlsx", "Descargar file_show.xlsx", class = "btn-primary"))
      ),
      card(card_header("Dividendos cargados (boletín + correcciones)"),
        p(class = "text-muted", "Esto es lo que el dashboard usa para ajustar los retornos."),
        DTOutput("tbl_div_data")),
      card(card_header("Valor cuota manual del día"),
        p(class = "text-muted", "Ingresa el VC de cada fondo/índice manual; la rentabilidad se calcula sola. Se guarda en el historial."),
        layout_columns(col_widths = c(4,8),
          dateInput("vc_fecha", "Fecha del VC", value = Sys.Date() - 1, weekstart = 1, language = "es"),
          div(DTOutput("tbl_entry"),
              div(style = "margin-top:8px", actionButton("save_vc_dia", "Guardar VC del día", class = "btn-primary")),
              verbatimTextOutput("vc_msg")))
      ),
      card(card_header("Historial de VC manuales (corregir / borrar)"),
        DTOutput("tbl_vc"),
        div(style = "margin-top:8px",
          actionButton("del_vc", "Borrar filas seleccionadas"),
          actionButton("save_vc", "Guardar historial", class = "btn-primary")),
        verbatimTextOutput("vc_hist_msg")),
      card(card_header("Corregir / agregar dividendos manualmente"),
        p(class = "text-muted", "Fecha límite AAAA-MM-DD; monto en $ por cuota."),
        DTOutput("tbl_ov"),
        div(style = "margin-top:8px",
          actionButton("add_ov", "Agregar fila"), actionButton("save_ov", "Guardar correcciones", class = "btn-primary")),
        verbatimTextOutput("ov_msg"))
    )
  )
)

# ============================== SERVER ==============================
server <- function(input, output, session) {
  rv <- reactiveValues(
    tick  = 0,
    vc    = .leer_csv("manuales_vc.csv", default_vc()),
    ov    = .leer_csv("dividendos_overrides.csv", default_ov()),
    entry = data.frame(ENTIDADES_MANUAL, VC = NA_real_, stringsAsFactors = FALSE),
    cur   = cargar_curados()
  )

  # ---- Agregar / quitar fondos ----
  # Poblar el buscador del catalogo (server-side: son ~3400 series) y las hojas
  if (!is.null(CATALOGO))
    updateSelectizeInput(session, "cat_fondo", server = TRUE,
                         choices = stats::setNames(CATALOGO$key, CATALOGO$label))
  observe({
    hojas <- if (!is.null(rv$cur)) unique(rv$cur$hoja) else character()
    updateSelectInput(session, "cat_hoja", choices = hojas,
                      selected = isolate(input$cat_hoja) %||% (hojas[1] %||% ""))
  })

  fila_catalogo <- reactive({
    k <- input$cat_fondo
    if (is.null(k) || !nzchar(k) || is.null(CATALOGO)) return(NULL)
    f <- CATALOGO[CATALOGO$key == k, , drop = FALSE]
    if (!nrow(f)) NULL else as.list(f[1, ])
  })

  # Al elegir un fondo: mostrar run/serie/row y prellenar el nombre a mostrar
  observeEvent(input$cat_fondo, {
    f <- fila_catalogo(); if (is.null(f)) return()
    if (!nzchar(input$cat_nombre %||% ""))
      updateTextInput(session, "cat_nombre", value = .nombre_sugerido(f$nombre))
  })
  output$cat_preview <- renderUI({
    f <- fila_catalogo()
    if (is.null(f)) return(p(class = "text-muted", "Ningún fondo seleccionado."))
    HTML(sprintf("<div style='font-size:13px;color:#555'><b>run:</b> %s &nbsp; <b>serie:</b> %s &nbsp; <b>tipo:</b> %s &nbsp; <b>row:</b> <code>%s</code></div>",
                 f$run, f$serie, f$tipoentidad, f$row))
  })

  # Autocompletar TAC desde el folleto de la CMF (sin token). Las rentabilidades
  # NO salen del folleto: se calculan del valor cuota (como el pulso) en el cron
  # tras agregar el fondo. Aqui solo el TAC (Remuneracion Maxima del folleto).
  observeEvent(input$cat_autocompletar, {
    f <- fila_catalogo()
    if (is.null(f)) { output$cat_auto_msg <- renderText("Elige un fondo del catálogo primero."); return() }
    if (!isTRUE(FOLLETO_OK)) { output$cat_auto_msg <- renderText("Autocompletar no disponible en este servidor."); return() }
    fondo <- list(run = f$run, serie = f$serie, tipoentidad = f$tipoentidad, row = f$row)
    d <- withProgress(message = "Consultando el folleto en la CMF…", value = 0.5,
      tryCatch(datos_folleto_fondo(fondo, anios = c(2024, 2025), dir = "data/folletos",
                                   cookies = "", reusar = TRUE),
               error = function(e) { message("[autocompletar] ", e$message); NULL }))
    if (is.null(d)) { output$cat_auto_msg <- renderText("No se pudo leer el folleto de la CMF (puede no tener folleto)."); return() }
    tac <- fmt_pct_cl(d$tac)
    if (nzchar(tac)) {
      updateTextInput(session, "cat_tac", value = tac)
      output$cat_auto_msg <- renderText(paste0("✅ TAC del folleto: ", tac,
        ". Revísalo. Las rentabilidades 2024/2025 se calculan solas tras agregar."))
    } else {
      output$cat_auto_msg <- renderText("El folleto no trae TAC para este fondo; escríbelo a mano.")
    }
  })

  observeEvent(input$cat_add, {
    if (is.null(rv$cur)) { output$cat_msg <- renderText("No se pudo leer la lista de fondos (fondos_curados.csv)."); return() }
    f <- fila_catalogo()
    hoja <- if (nzchar(trimws(input$cat_hoja_nueva %||% ""))) trimws(input$cat_hoja_nueva) else (input$cat_hoja %||% "")
    tit  <- { m <- rv$cur$titulo[rv$cur$hoja == hoja]; if (length(m)) m[1] else hoja }
    res <- agregar_fondo_curado(rv$cur, f, hoja = hoja, titulo = tit, nombre = input$cat_nombre,
                                moneda = input$cat_moneda, rent2024 = input$cat_rent2024,
                                rent2025 = input$cat_rent2025, duracion = input$cat_duracion,
                                liquidez = input$cat_liquidez, tac = input$cat_tac)
    if (!isTRUE(res$ok)) { output$cat_msg <- renderText(paste("⚠", res$msg)); return() }
    rv$cur <- res$df
    msg <- guardar_curados(rv$cur, paste0("Panel admin: agregar fondo '", trimws(input$cat_nombre), "'"))
    refrescar_fondos_globales()
    disp <- gh_dispatch("refresh.yml")
    updateTextInput(session, "cat_nombre", value = ""); updateTextInput(session, "cat_hoja_nueva", value = "")
    for (id in c("cat_rent2024","cat_rent2025","cat_duracion","cat_liquidez","cat_tac"))
      updateTextInput(session, id, value = "")
    rv$tick <- rv$tick + 1
    output$cat_msg <- renderText(paste0("✅ Agregado. ", msg,
      if (disp$ok) " · Consultando la CMF: recarga (F5) en ~5 min para ver sus rentabilidades." else paste0(" · (scrape no disparado: ", disp$msg, ")")))
    showModal(modalDialog(title = "Fondo agregado",
      HTML(paste0("✅ <b>", trimws(input$cat_nombre), "</b> se agregó a <b>", hoja, "</b>.<br><br>",
                  "Se está consultando la CMF para traer sus rentabilidades.<br>",
                  "Recarga esta página (<b>F5</b>) en <b>~3 a 5 minutos</b>.")),
      easyClose = TRUE, footer = modalButton("Cerrar")))
  })

  output$tbl_cur <- renderDT({
    rv$tick
    df <- rv$cur
    if (is.null(df) || !nrow(df)) return(datatable(data.frame(Aviso = "Sin lista curada."), rownames = FALSE))
    vis <- data.frame(Nombre = df$nombre_excel, Categoría = df$hoja,
                      Run = df$run, Serie = df$serie,
                      Tipo = ifelse(tolower(df$es_manual) %in% c("true","1"), "Manual", df$tipoentidad),
                      check.names = FALSE, stringsAsFactors = FALSE)
    datatable(vis, rownames = FALSE, selection = "single",
              options = list(pageLength = 10, lengthMenu = list(c(10,25,50,-1), c("10","25","50","Todas"))))
  }, server = FALSE)

  observeEvent(input$cat_del, {
    sel <- input$tbl_cur_rows_selected
    if (is.null(rv$cur) || !length(sel)) { output$cat_del_msg <- renderText("Selecciona un fondo primero."); return() }
    nombre <- rv$cur$nombre_excel[sel]
    rv$cur <- rv$cur[-sel, , drop = FALSE]
    msg <- guardar_curados(rv$cur, paste0("Panel admin: quitar fondo '", nombre, "'"))
    refrescar_fondos_globales(); rv$tick <- rv$tick + 1
    output$cat_del_msg <- renderText(paste0("🗑 Quitado '", nombre, "'. ", msg))
  })

  output$dash <- renderUI({
    rv$tick
    t <- preparar_todo(rv$vc, rv$ov)
    if (is.null(t)) return(div(style = "padding:40px", h4("Aún no hay datos."),
      p("Corre scripts/actualizar_datos.R o espera al refresco automático.")))
    gen <- substr(gsub("T", " ", t$datos$generado %||% ""), 1, 16)
    tags$iframe(srcdoc = render_dashboard_html(t$pd, logo_b64, gen),
                allow = "fullscreen", allowfullscreen = NA,
                style = "width:100%;height:90vh;border:none")
  })

  # Botón: re-consultar la CMF ahora (dispara el workflow de GitHub Actions)
  observeEvent(input$actualizar, ignoreInit = TRUE, {
    res <- gh_dispatch("refresh.yml")
    showModal(modalDialog(title = "Actualizar datos",
      if (res$ok)
        HTML("✅ Se inició la actualización: la app está re-consultando la CMF.<br><br>En <b>~3 a 5 minutos</b> recarga esta página (F5) y verás los datos nuevos.")
      else paste("No se pudo iniciar:", res$msg),
      easyClose = TRUE, footer = modalButton("Cerrar")))
  })

  # ---- Dividendos: subir + persistir ----
  estado_div <- function() {
    store_sync("dividendos.xlsx")
    if (file.exists("data/dividendos.xlsx"))
      paste0("Archivo guardado: dividendos.xlsx (", format(file.info("data/dividendos.xlsx")$mtime, "%d/%m %H:%M"), ")")
    else "Aún no se ha subido ningún archivo de dividendos."
  }
  output$div_msg <- renderText(estado_div())
  observeEvent(input$div_file, {
    msg <- tryCatch(store_write_file("dividendos.xlsx", input$div_file$datapath, "Subir dividendos desde la app"),
                    error = function(e) paste("Error:", e$message))
    output$div_msg <- renderText(paste0(msg, "\n", estado_div())); rv$tick <- rv$tick + 1
  })

  # ---- Tabla de dividendos cargados ----
  output$tbl_div_data <- renderDT({
    rv$tick
    store_sync("dividendos.xlsx")
    ruta <- if (file.exists("data/dividendos.xlsx")) "data/dividendos.xlsx" else NULL
    div <- fusionar_overrides(cargar_dividendos_pulso(ruta), rv$ov)
    df <- do.call(rbind, lapply(names(div), function(nm) {
      e <- div[[nm]]; if (is.null(e) || !nrow(e)) return(NULL)
      data.frame(Fondo = nm, `Fecha límite` = as.character(e$fecha_limite),
                 `Monto ($/cuota)` = round(e$monto, 4), check.names = FALSE) }))
    if (is.null(df) || !nrow(df))
      df <- data.frame(Aviso = "No hay dividendos cargados. Sube el boletín de la Bolsa arriba.", check.names = FALSE)
    oc <- which(names(df) == "Fecha límite") - 1
    opts <- list(pageLength = 10, lengthMenu = list(c(10, 25, 50, -1), c("10","25","50","Todas")))
    if (length(oc) == 1) opts$order <- list(list(oc, "desc"))
    datatable(df, rownames = FALSE, options = opts)
  }, server = FALSE)

  # ---- Descargar Excel ----
  output$dl_xlsx <- downloadHandler(
    filename = function() "file_show.xlsx",
    content  = function(file) {
      t <- preparar_todo(rv$vc, rv$ov)
      generar_file_show(t$pd, t$datos, t$series_aj, t$div, rv$vc, file)
    })

  # ---- VC manual del día (entrada rápida) ----
  output$tbl_entry <- renderDT(datatable(rv$entry, editable = list(target = "cell", disable = list(columns = c(0,1))),
                                         rownames = FALSE, options = list(dom = "t")), server = FALSE)
  observeEvent(input$tbl_entry_cell_edit, { rv$entry <- editData(rv$entry, input$tbl_entry_cell_edit, rownames = FALSE) })
  observeEvent(input$save_vc_dia, {
    f <- as.character(input$vc_fecha)
    add <- rv$entry[!is.na(rv$entry$VC) & rv$entry$VC != "", , drop = FALSE]
    if (!nrow(add)) { output$vc_msg <- renderText("No ingresaste ningún VC."); return() }
    nuevas <- data.frame(Nombre = add$Nombre, Tipo = add$Tipo, Fecha = f,
                         Valor = suppressWarnings(as.numeric(add$VC)), stringsAsFactors = FALSE)
    keep <- rv$vc[!(paste(rv$vc$Nombre, rv$vc$Fecha) %in% paste(nuevas$Nombre, nuevas$Fecha)), , drop = FALSE]
    rv$vc <- rbind(keep, nuevas)
    rv$vc <- rv$vc[order(rv$vc$Nombre, rv$vc$Fecha), ]
    msg <- guardar_vc(); rv$entry$VC <- NA_real_
    output$vc_msg <- renderText(paste0("Guardados ", nrow(nuevas), " VC del ", f, ". ", msg)); rv$tick <- rv$tick + 1
  })

  # ---- Historial de VC (editable / borrar) ----
  guardar_vc <- function() store_write_text("manuales_vc.csv",
    paste(c(paste(names(rv$vc), collapse = ","),
            if (nrow(rv$vc)) apply(rv$vc, 1, function(x) paste(x, collapse = ","))), collapse = "\n"),
    "Actualizar VC manuales")
  output$tbl_vc <- renderDT(datatable(rv$vc, editable = TRUE, rownames = FALSE,
    options = list(pageLength = 10, order = list(list(2, "desc")),
                   lengthMenu = list(c(10, 25, 50, -1), c("10","25","50","Todas")))), server = FALSE)
  observeEvent(input$tbl_vc_cell_edit, { rv$vc <- editData(rv$vc, input$tbl_vc_cell_edit, rownames = FALSE) })
  observeEvent(input$del_vc, {
    sel <- input$tbl_vc_rows_selected
    if (length(sel)) { rv$vc <- rv$vc[-sel, , drop = FALSE] }
  })
  observeEvent(input$save_vc, { output$vc_hist_msg <- renderText(guardar_vc()); rv$tick <- rv$tick + 1 })

  # ---- Correcciones de dividendos ----
  output$tbl_ov <- renderDT(datatable(rv$ov, editable = TRUE, rownames = FALSE,
    options = list(pageLength = 10, order = list(list(1, "desc")),
                   lengthMenu = list(c(10, 25, 50, -1), c("10","25","50","Todas")))), server = FALSE)
  observeEvent(input$tbl_ov_cell_edit, { rv$ov <- editData(rv$ov, input$tbl_ov_cell_edit, rownames = FALSE) })
  observeEvent(input$add_ov, {
    rv$ov <- rbind(rv$ov, data.frame(Fondo = "", `Fecha limite` = "", Monto = NA_real_,
                                     check.names = FALSE, stringsAsFactors = FALSE))
  })
  observeEvent(input$save_ov, {
    msg <- store_write_text("dividendos_overrides.csv",
      paste(c(paste(names(rv$ov), collapse = ","),
              if (nrow(rv$ov)) apply(rv$ov, 1, function(x) paste(x, collapse = ","))), collapse = "\n"),
      "Corregir dividendos")
    output$ov_msg <- renderText(msg); rv$tick <- rv$tick + 1
  })
}

shinyApp(ui, server)
