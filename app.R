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
try(sincronizar_datos(), silent = TRUE)   # data/ al dia con GitHub (ver datos_ver en el server)
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
  # primero la API de GitHub (siempre fresca); el CDN raw puede traer una version
  # de hasta ~5 min atras y, al guardar desde esa sesion, borrar lo recien ingresado
  txt <- tryCatch(store_read_fresh_text(rel), error = function(e) NULL)
  if (!is.null(txt))
    return(tryCatch(read.csv(text = txt, stringsAsFactors = FALSE, check.names = FALSE, encoding = "UTF-8",
                             colClasses = c(Fecha = "character")), error = function(e) default_df))
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
# Serie automatica del IPSA: la oficial que guarda el cron (BCCh / datosmacro, en
# macro_series) + el cierre diario de Yahoo (MXIPSAGC.SN, data/ipsa_yahoo.csv, lo
# guarda el workflow ipsa_yahoo.yml tras el cierre). En la misma fecha manda la oficial.
serie_ipsa_auto <- function(datos) {
  of <- datos$macro_series[["IPSA"]]
  of <- if (!is.null(of) && nrow(of)) tibble(fecha = as.Date(of$fecha), valor_cuota = as.numeric(of$valor)) else NULL
  y <- if (file.exists("data/ipsa_yahoo.csv"))
    tryCatch(read.csv("data/ipsa_yahoo.csv", colClasses = "character"), error = function(e) NULL) else NULL
  y <- if (!is.null(y) && nrow(y)) tibble(fecha = as.Date(y$fecha), valor_cuota = suppressWarnings(as.numeric(y$cierre))) else NULL
  s <- bind_rows(of, y)
  if (!nrow(s)) return(NULL)
  s %>% filter(!is.na(fecha), !is.na(valor_cuota)) %>% distinct(fecha, .keep_all = TRUE) %>% arrange(fecha)
}

# Inyecta WTD/MTD/YTD de los indices manuales en la lista macro. Cada indice usa
# su VC manual y, en las fechas sin VC manual, la serie automatica de `auto`
# (ej. IPSA: oficial + Yahoo): si un dia no se ingresa a mano, igual queda al dia.
aplicar_indices_manuales <- function(macro, vc_df, fc, auto = list()) {
  ref_wtd <- fc - 7; ref_mtd <- floor_date(fc, "month") - 1
  ref_ytd <- as.Date(sprintf("%d-12-31", year(fc) - 1))
  ref_mes_ant_fin <- ref_mtd; ref_mes_ant_ini <- floor_date(ref_mes_ant_fin, "month") - 1
  for (nm in NOMBRES_INDICES_MANUALES) {
    s <- bind_rows(serie_de_vc(vc_df, nm), auto[[nm]])   # en la misma fecha manda el manual
    if (!nrow(s)) next
    s <- s %>% distinct(fecha, .keep_all = TRUE) %>% arrange(fecha)
    for (j in seq_along(macro)) if (identical(macro[[j]]$nombre, nm)) {
      macro[[j]]$wtd <- .rent(s, ref_wtd, fc)
      macro[[j]]$mtd <- .rent(s, ref_mtd, fc)
      macro[[j]]$ytd <- .rent(s, ref_ytd, fc)
      macro[[j]]$mes_ant <- .rent(s, ref_mes_ant_ini, ref_mes_ant_fin)
    }
  }
  macro
}

preparar_todo <- function(vc_df, ov_df = NULL) {
  # Lee SOLO del disco: series_vc.rds, fondos_curados.csv, dividendos.xlsx y
  # dividendos_calculados.csv los mantiene al dia sincronizar_datos() (al arrancar
  # y cada vez que cambia el commit de GitHub, via datos_ver en el server). Antes
  # se bajaban en cada render, y la lista de fondos podia quedar vieja hasta
  # reiniciar la app.
  if (!file.exists(DATA_RDS)) return(NULL)
  datos <- readRDS(DATA_RDS)
  fc <- as.Date(datos$cierre)
  # Dividendos (FIRES/FINRE) + factor de reparto (RGFMU) -> valor_cuota_ajustado
  ruta_div <- if (file.exists("data/dividendos.xlsx")) "data/dividendos.xlsx" else NULL
  div <- fusionar_overrides(cargar_dividendos_pulso(ruta_div), ov_df)
  series_aj <- enriquecer_series_ajustadas(series_por_nombre(datos$series), div)   # rds por run-serie -> nombre actual
  macro <- aplicar_indices_manuales(datos$macro, vc_df, fc, auto = list(IPSA = serie_ipsa_auto(datos)))
  series_man <- construir_series_manual(vc_df)
  # "MBI Deuda estructurada" es manual (VC ingresado a mano) pero sus dividendos SI
  # se cargan del boletin (ticker CFI-MBICHA, ver MAPEO_SEBRA); sin este ajuste
  # quedaban registrados en `div` pero nunca se sumaban al VC para la rentabilidad.
  if (!is.null(series_man[["MBI Deuda estructurada"]])) {
    ev <- div[["MBI Deuda estructurada"]]
    ev_t <- if (!is.null(ev)) tibble(fecha_limite = as.Date(ev$fecha_limite),
                                     monto = as.numeric(ev$monto), moneda = NA_character_) else NULL
    series_man[["MBI Deuda estructurada"]] <- aplicar_dividendos_a_historico(
      series_man[["MBI Deuda estructurada"]], ev_t, moneda_vc = "$")
  }
  pd <- construir_pulso_data(series_aj, fecha_cierre = fc, macro = macro,
                             rf_usa = datos$rf_usa, series_manual = series_man)
  list(pd = pd, datos = datos, series_aj = series_aj, div = div)
}
preparar_pd <- function(vc_df, ov_df = NULL) { t <- preparar_todo(vc_df, ov_df); if (is.null(t)) NULL else t$pd }

# ============================== UI ==============================
ui <- page_navbar(
  title = "Pulso VCC", theme = bs_theme(version = 5),
  header = tagList(
    # Reconexion automatica: shinyapps.io corta la conexion tras un rato sin
    # actividad y en cada redeploy. En vez de la pantalla gris "Disconnected from
    # the server", un aviso chico "Reconectando..." y la pagina se recarga sola en
    # cuanto el servidor responde (si estaba en otra pestaña, al volver a ella).
    # Tope de 3 recargas en 2 min por si la app fallara al cargar.
    tags$style(HTML("#shiny-disconnected-overlay,#ss-connect-dialog,#ss-overlay{display:none!important}
      #pulso-reconectando{display:none;position:fixed;right:16px;bottom:16px;z-index:9999;
        background:#333;color:#fff;padding:8px 14px;border-radius:6px;font-size:13px;opacity:.9}")),
    tags$div(id = "pulso-reconectando", "Reconectando…"),
    tags$script(HTML("(function(){var caido=false,esperando=false;
      function puedeRecargar(){try{var k='pulsoRecargas',a=JSON.parse(sessionStorage.getItem(k)||'[]'),t=Date.now();
        a=a.filter(function(x){return t-x<120000;});if(a.length>=3)return false;a.push(t);sessionStorage.setItem(k,JSON.stringify(a));}catch(e){}
        return true;}
      function esperarYRecargar(){if(esperando)return;esperando=true;
        (function intento(){fetch(location.href,{cache:'no-store'}).then(function(r){
            if(r.ok&&puedeRecargar())location.reload();else setTimeout(intento,3000);})
          .catch(function(){setTimeout(intento,3000);});})();}
      $(document).on('shiny:disconnected',function(){caido=true;
        document.getElementById('pulso-reconectando').style.display='block';
        if(document.visibilityState==='visible')esperarYRecargar();});
      document.addEventListener('visibilitychange',function(){if(caido&&document.visibilityState==='visible')esperarYRecargar();});})();"))),
  nav_panel("Dashboard",
    tags$script(HTML("window.addEventListener('message',function(e){if(e&&e.data==='pulso-actualizar'){Shiny.setInputValue('actualizar',(window.__pulsoN=(window.__pulsoN||0)+1),{priority:'event'});}});")),
    uiOutput("dash")),
  nav_panel("Administración",
    div(style = "max-width:1100px;margin:20px auto;padding:0 16px",
      h4("Panel de administración"),
      p(class = "text-muted", "Los cambios se guardan y persisten (en GitHub cuando está configurado). El dashboard se recalcula al instante."),
      card(card_header("➕ Agregar fondo (catálogo CMF o link de la CMF)"),
        p(class = "text-muted", "Busca el fondo en el catálogo del comparador (trae run/serie/row correctos de la CMF). Elige su categoría, completa los datos que quieras mostrar y agrégalo. Se guarda y se dispara la consulta a la CMF; en ~3 a 5 min aparecen solas sus rentabilidades."),
        selectizeInput("cat_fondo", "Fondo (busca por nombre)", width = "100%",
                       choices = if (!is.null(CATALOGO)) stats::setNames(CATALOGO$key, CATALOGO$label) else NULL,
                       options = list(placeholder = "Escribe para buscar en el catálogo...",
                                      maxOptions = 10000, closeAfterSelect = TRUE)),
        p(class = "text-muted", style = "font-size:13px;margin:-4px 0 6px",
          "¿Fondo nuevo que no está en el catálogo? Pega el link de su ficha en la CMF (cmfchile.cl → el fondo → cualquier pestaña) y elige la serie:"),
        layout_columns(col_widths = c(7, 2, 3),
          textInput("cat_link", NULL, placeholder = "https://www.cmfchile.cl/institucional/mercados/entidad.php?mercado=V&rut=…&tipoentidad=…&row=…"),
          actionButton("cat_link_buscar", "Buscar en la CMF", class = "btn-outline-secondary"),
          uiOutput("cat_link_serie_ui")),
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
          textInput("cat_rent2024", "Rent. 2024", "", placeholder = "vacío = se calcula"),
          textInput("cat_rent2025", "Rent. 2025", "", placeholder = "vacío = se calcula"),
          textInput("cat_duracion", "Duración", ""),
          textInput("cat_liquidez", "Liquidez", ""),
          selectInput("cat_moneda", "Moneda", choices = c("CLP","UF","USD")),
          textInput("cat_tac", "TAC", "")),
        layout_columns(col_widths = c(3, 3, 6),
          textInput("cat_ticker", "Ticker Boletín (dividendos)", ""),
          selectInput("cat_art107", "¿Acogido al art. 107 LIR?", choices = c("Sin definir" = "", "Sí", "No")),
          div(style = "font-size:12px;padding-top:30px", textOutput("cat_ticker_msg"))),
        div(style = "margin-top:8px", actionButton("cat_add", "Agregar fondo", class = "btn-primary")),
        verbatimTextOutput("cat_msg")),
      card(card_header("Editar fondos (categoría, rent, TAC, duración…)"),
        p(class = "text-muted", HTML("Haz <b>doble clic</b> en una celda para editarla: <b>Nombre</b>, categoría, rentabilidades, duración, liquidez, moneda, TAC, ticker, art. 107 (escribe <b>Sí</b> o <b>No</b>). Para mover un fondo de categoría, edita la columna <b>Categoría</b>. Luego <b>Guardar cambios</b>. Para eliminar, selecciona filas y <b>Eliminar</b>. <i>(Renombrar no pierde datos: las series se guardan por run + serie.)</i>")),
        uiOutput("revision_tickers"),
        DTOutput("tbl_cur"),
        div(style = "margin-top:8px",
          actionButton("cat_save", "💾 Guardar cambios", class = "btn-primary"),
          actionButton("cat_del", "Eliminar seleccionadas", class = "btn-outline-danger", style = "margin-left:8px")),
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

  # ---- Datos al dia sin reiniciar ----
  # Cada 60 s se mira el ultimo commit de GitHub (cron, panel admin de otra
  # sesion, workflow de dividendos). Si cambio, se bajan solo los archivos que
  # cambiaron y el dashboard se redibuja solo; si no, no se descarga nada.
  datos_ver <- reactivePoll(60000, session, checkFunc = version_datos, valueFunc = function() {
    cambios <- sincronizar_datos()
    if ("fondos_curados.csv" %in% cambios && !isTRUE(getOption("pulso.hardcode_only", FALSE)))
      refrescar_fondos_globales()
    .sync$sha
  })
  # La tabla del panel admin se recarga solo si la lista cambio en GitHub (otra
  # sesion o el cron la edito); asi no se guarda encima una version vieja.
  cur_blob <- .sync$blobs[["fondos_curados.csv"]]
  observeEvent(datos_ver(), {
    b <- .sync$blobs[["fondos_curados.csv"]]
    if (!identical(b, cur_blob)) { cur_blob <<- b; rv$cur <- cargar_curados(); rv$tick <- rv$tick + 1 }
  })

  # ---- Agregar / quitar fondos ----
  # El buscador de fondos ya trae las ~3500-5000 opciones (todo el catalogo) cargadas en el navegador
  # (ver selectizeInput en la UI): el filtrado es instantaneo, sin ida y vuelta
  # al servidor por cada letra (eso era lo que se sentia lento/no funcionaba
  # en shinyapps.io). Aqui solo faltan las categorias (hojas).
  observe({
    hojas <- if (!is.null(rv$cur)) unique(rv$cur$hoja) else character()
    updateSelectInput(session, "cat_hoja", choices = hojas,
                      selected = isolate(input$cat_hoja) %||% (hojas[1] %||% ""))
  })

  # El fondo a agregar sale del catalogo o del link de la CMF: vale el ultimo
  # que se uso (elegir en el buscador o buscar un link).
  fuente   <- reactiveVal("catalogo")
  fondo_link <- reactiveVal(NULL)   # list(nombre, run, tipoentidad, row, series)

  fila_catalogo <- reactive({
    if (fuente() == "link") {
      d <- fondo_link(); s <- input$cat_link_serie
      if (is.null(d) || is.null(s) || !nzchar(s)) return(NULL)
      return(list(nombre = d$nombre, run = d$run, serie = s, tipoentidad = d$tipoentidad, row = d$row))
    }
    k <- input$cat_fondo
    if (is.null(k) || !nzchar(k) || is.null(CATALOGO)) return(NULL)
    f <- CATALOGO[CATALOGO$key == k, , drop = FALSE]
    if (!nrow(f)) NULL else as.list(f[1, ])
  })

  # Al elegir un fondo: mostrar run/serie/row y prellenar el nombre a mostrar
  observeEvent(input$cat_fondo, {
    if (nzchar(input$cat_fondo %||% "")) fuente("catalogo")
    f <- fila_catalogo(); if (is.null(f)) return()
    if (!nzchar(input$cat_nombre %||% ""))
      updateTextInput(session, "cat_nombre", value = .nombre_sugerido(f$nombre))
  })

  # Fondo nuevo por link de la CMF: lee nombre y series vigentes de su ficha
  observeEvent(input$cat_link_buscar, {
    f <- parsear_link_cmf(input$cat_link)
    if (is.null(f)) {
      fondo_link(NULL)
      output$cat_msg <- renderText("⚠ El link no es de una ficha de fondo de la CMF (debe traer rut=…, tipoentidad=… y row=…).")
      return()
    }
    d <- withProgress(message = "Consultando el fondo en la CMF…", value = 0.5,
      tryCatch(datos_fondo_cmf(f), error = function(e) conditionMessage(e)))
    if (is.character(d)) { fondo_link(NULL); output$cat_msg <- renderText(paste("⚠ No se pudo leer el fondo:", d)); return() }
    if (!length(d$series)) { fondo_link(NULL); output$cat_msg <- renderText("⚠ La CMF no muestra series vigentes para este fondo."); return() }
    fondo_link(d); fuente("link")
    updateTextInput(session, "cat_nombre", value = .nombre_sugerido(d$nombre))
    output$cat_msg <- renderText(paste0("✅ Encontrado en la CMF: ", d$nombre, ". Elige la serie."))
  })
  output$cat_link_serie_ui <- renderUI({
    d <- fondo_link(); if (is.null(d)) return(NULL)
    selectInput("cat_link_serie", NULL, choices = stats::setNames(d$series, paste("Serie", d$series)))
  })

  output$cat_preview <- renderUI({
    f <- fila_catalogo()
    if (is.null(f)) return(p(class = "text-muted", "Ningún fondo seleccionado."))
    HTML(sprintf("<div style='font-size:13px;color:#555'><b>%s</b> &nbsp; <b>run:</b> %s &nbsp; <b>serie:</b> %s &nbsp; <b>tipo:</b> %s &nbsp; <b>row:</b> <code>%s</code></div>",
                 if (fuente() == "link") "Desde link CMF:" else "", f$run, f$serie, f$tipoentidad, f$row))
  })

  # Ticker del Boletin: se completa solo al elegir el fondo (por run + serie)
  observeEvent(fila_catalogo(), ignoreNULL = FALSE, {
    f <- fila_catalogo()
    if (is.null(f)) { output$cat_ticker_msg <- renderText(""); return() }
    if (identical(f$tipoentidad, "RGFMU")) {
      updateTextInput(session, "cat_ticker", value = "")
      output$cat_ticker_msg <- renderText("Fondo mutuo: sus repartos ya vienen en el valor cuota, no lleva ticker.")
      return()
    }
    sug <- ticker_sugerido(f$run, f$serie)
    updateTextInput(session, "cat_ticker", value = sug)
    output$cat_ticker_msg <- renderText(if (nzchar(sug))
      paste0("✅ Ticker del Boletín para run ", f$run, " serie ", f$serie, ": ", sug, ".")
      else "No aparece en el Boletín (sin dividendos desde 2024, o es nuevo). Si reparte dividendos, escribe su ticker.")
  })

  # Revision de tickers de la lista actual contra el Boletin
  revision <- reactive({ rv$tick; datos_ver(); revisar_tickers(rv$cur) })
  output$revision_tickers <- renderUI({
    r <- revision(); if (!nrow(r)) return(NULL)
    filas <- paste0("<tr><td>", htmltools::htmlEscape(r$Fondo), "</td><td>", r$Run, "-", r$Serie,
                    "</td><td>", ifelse(nzchar(r$Actual), r$Actual, "—"), "</td><td><b>", r$Boletin,
                    "</b></td><td>", r$Problema, "</td></tr>", collapse = "")
    div(class = "alert alert-warning", style = "font-size:13px",
      HTML(paste0("<b>⚠ ", nrow(r), " fondo(s) con el ticker distinto al del Boletín</b> (sus dividendos no se suman bien):",
                  "<table class='table table-sm' style='margin:6px 0'><tr><th>Fondo</th><th>Run-serie</th><th>Ticker actual</th><th>Boletín</th><th></th></tr>",
                  filas, "</table>")),
      actionButton("aplicar_tickers", "Usar los tickers del Boletín y guardar", class = "btn-warning btn-sm"))
  })
  observeEvent(input$aplicar_tickers, {
    r <- revision(); if (!nrow(r) || is.null(rv$cur)) return()
    for (k in seq_len(nrow(r))) {
      i <- which(rv$cur$run == r$Run[k] & rv$cur$serie == r$Serie[k])
      rv$cur$ticker_sebra[i] <- r$Boletin[k]
    }
    msg <- guardar_curados(rv$cur, paste0("Panel admin: tickers del Boletín para ", nrow(r), " fondo(s)"))
    refrescar_fondos_globales(); rv$tick <- rv$tick + 1
    output$cat_del_msg <- renderText(paste0("✅ Tickers actualizados (", paste(r$Fondo, collapse = ", "), "). ", msg))
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
    # Validaciones que evitan los errores tipicos al agregar (caso MBIQ):
    r24 <- trimws(input$cat_rent2024 %||% ""); r25 <- trimws(input$cat_rent2025 %||% "")
    if (nzchar(r24) && identical(r24, r25)) {
      output$cat_msg <- renderText(paste0("⚠ Rent. 2024 y 2025 son iguales (", r24, "): revisa. Si las dejas vacías, se calculan solas del valor cuota con dividendos."))
      return()
    }
    tk <- trimws(input$cat_ticker %||% "")
    sug <- if (!is.null(f) && !identical(f$tipoentidad, "RGFMU")) ticker_sugerido(f$run, f$serie) else ""
    if (!nzchar(tk) && nzchar(sug)) {
      output$cat_msg <- renderText(paste0("⚠ Falta el ticker: el Boletín trae ", sug, " para este fondo y serie. Sin ticker sus dividendos no se suman y la rentabilidad sale más baja."))
      return()
    }
    hoja <- if (nzchar(trimws(input$cat_hoja_nueva %||% ""))) trimws(input$cat_hoja_nueva) else (input$cat_hoja %||% "")
    tit  <- { m <- rv$cur$titulo[rv$cur$hoja == hoja]; if (length(m)) m[1] else hoja }
    res <- agregar_fondo_curado(rv$cur, f, hoja = hoja, titulo = tit, nombre = input$cat_nombre,
                                moneda = input$cat_moneda, rent2024 = r24,
                                rent2025 = r25, duracion = input$cat_duracion,
                                liquidez = input$cat_liquidez, tac = input$cat_tac,
                                ticker_sebra = tk, art107 = input$cat_art107)
    if (!isTRUE(res$ok)) { output$cat_msg <- renderText(paste("⚠", res$msg)); return() }
    rv$cur <- res$df
    msg <- guardar_curados(rv$cur, paste0("Panel admin: agregar fondo '", trimws(input$cat_nombre), "'"))
    refrescar_fondos_globales()
    disp <- gh_dispatch("refresh.yml", list(solo_fondos = paste0(f$run, "-", f$serie)))   # solo el fondo nuevo
    updateTextInput(session, "cat_nombre", value = ""); updateTextInput(session, "cat_hoja_nueva", value = "")
    if (fuente() == "link") { fondo_link(NULL); updateTextInput(session, "cat_link", value = "") }
    for (id in c("cat_rent2024","cat_rent2025","cat_duracion","cat_liquidez","cat_tac","cat_ticker"))
      updateTextInput(session, id, value = "")
    updateSelectInput(session, "cat_art107", selected = "")
    rv$tick <- rv$tick + 1
    output$cat_msg <- renderText(paste0("✅ Agregado. ", msg,
      if (disp$ok) " · Consultando la CMF solo para este fondo: aparece solo en ~2 min." else paste0(" · (scrape no disparado: ", disp$msg, ")")))
    showModal(modalDialog(title = "Fondo agregado",
      HTML(paste0("✅ <b>", trimws(input$cat_nombre), "</b> se agregó a <b>", hoja, "</b>.<br><br>",
                  "Se está consultando la CMF para traer sus rentabilidades.<br>",
                  "El dashboard se actualizará solo en <b>~2 minutos</b> (no hace falta recargar).")),
      easyClose = TRUE, footer = modalButton("Cerrar")))
  })

  # columnas visibles (orden = indice DT 0-based) -> columna real en rv$cur
  .colmap_cur <- c("hoja","nombre_excel","rent2024","rent2025","duracion",
                   "liquidez","moneda","tac","art107","ticker_sebra","run","serie","tipoentidad")
  .editables_cur <- c("hoja","nombre_excel","rent2024","rent2025","duracion","liquidez","moneda","tac","art107","ticker_sebra")

  output$tbl_cur <- renderDT({
    rv$tick
    df <- isolate(rv$cur)   # editar celdas NO debe re-renderizar (isolate)
    if (is.null(df) || !nrow(df)) return(datatable(data.frame(Aviso = "Sin lista curada."), rownames = FALSE))
    vis <- data.frame(
      Categoría = df$hoja, Nombre = df$nombre_excel,
      `Rent 2024` = df$rent2024, `Rent 2025` = df$rent2025,
      Duración = df$duracion, Liquidez = df$liquidez, Moneda = df$moneda,
      TAC = df$tac, `Art. 107` = df$art107, Ticker = df$ticker_sebra,
      Run = df$run, Serie = df$serie,
      Tipo = ifelse(tolower(df$es_manual) %in% c("true","1"), "Manual", df$tipoentidad),
      check.names = FALSE, stringsAsFactors = FALSE)
    datatable(vis, rownames = FALSE, selection = "multiple",
              editable = list(target = "cell", disable = list(columns = c(10, 11, 12))),
              options = list(pageLength = 15, lengthMenu = list(c(15, 30, 60, -1), c("15","30","60","Todas")),
                             columnDefs = list(list(className = "dt-center", targets = 2:12))))
  }, server = FALSE)

  # editar una celda -> actualiza rv$cur (sin re-render)
  observeEvent(input$tbl_cur_cell_edit, {
    info <- input$tbl_cur_cell_edit
    cc <- .colmap_cur[info$col + 1]
    if (length(cc) != 1 || !(cc %in% .editables_cur) || is.null(rv$cur) || info$row > nrow(rv$cur)) return()
    val <- as.character(info$value)
    if (cc == "nombre_excel") {
      val <- trimws(val); viejo <- rv$cur$nombre_excel[info$row]
      es_man <- tolower(trimws(rv$cur$es_manual[info$row])) %in% c("true","1","yes","si","sí")
      # los manuales guardan su VC por nombre (manuales_vc.csv): no se renombran aqui
      if (es_man || !nzchar(val)) {
        output$cat_del_msg <- renderText(if (es_man) "⚠ Los fondos manuales no se renombran desde aquí (su VC se guarda por nombre)." else "⚠ El nombre no puede quedar vacío.")
        rv$tick <- rv$tick + 1; return()
      }
      # renombrar: todas las filas del mismo fondo (run + serie, ej. si esta en 2
      # hojas); sus series siguen intactas porque se guardan por run-serie
      mismo <- which(rv$cur$run == rv$cur$run[info$row] & rv$cur$serie == rv$cur$serie[info$row] &
                     !tolower(trimws(rv$cur$es_manual)) %in% c("true","1","yes","si","sí"))
      if (val %in% rv$cur$nombre_excel[-mismo]) {
        output$cat_del_msg <- renderText(paste0("⚠ Ya existe otro fondo llamado '", val, "'. Usa otro nombre."))
        rv$tick <- rv$tick + 1; return()
      }
      rv$cur$nombre_excel[mismo] <- val; rv$cur$nombre_script[mismo] <- val
      if (!identical(viejo, val)) renombres[[viejo]] <<- val
      if (length(mismo) > 1) rv$tick <- rv$tick + 1
      return()
    }
    rv$cur[info$row, cc] <- val
  })
  # Renombres pendientes (viejo -> nuevo): al guardar, las correcciones de
  # dividendos de esos fondos (dividendos_overrides.csv, por nombre) se mueven al nombre nuevo
  renombres <- list()

  observeEvent(input$cat_save, {
    if (is.null(rv$cur)) { output$cat_del_msg <- renderText("No hay lista que guardar."); return() }
    rv$cur <- normalizar_titulos(rv$cur)
    msg <- guardar_curados(rv$cur, "Panel admin: editar lista de fondos")
    if (length(renombres) && nrow(rv$ov)) {
      fondo <- as.character(rv$ov$Fondo); mover <- fondo %in% names(renombres)
      if (any(mover)) {
        # resolver cadenas (A -> B -> C) por si se renombro dos veces antes de guardar
        for (k in which(mover)) {
          n <- fondo[k]; i <- 0
          while (!is.null(renombres[[n]]) && i < length(renombres)) { n <- renombres[[n]]; i <- i + 1 }
          fondo[k] <- n
        }
        rv$ov$Fondo <- fondo
        msg <- paste0(msg, " · Correcciones de dividendos: ", guardar_ov())
      }
    }
    renombres <<- list()
    refrescar_fondos_globales(); rv$tick <- rv$tick + 1
    output$cat_del_msg <- renderText(paste0("💾 Cambios guardados. ", msg,
      " · El dashboard se actualiza solo en ~1 min."))
  })

  observeEvent(input$cat_del, {
    sel <- input$tbl_cur_rows_selected
    if (is.null(rv$cur) || !length(sel)) { output$cat_del_msg <- renderText("Selecciona una o más filas primero."); return() }
    nombres <- paste(rv$cur$nombre_excel[sel], collapse = ", ")
    rv$cur <- rv$cur[-sel, , drop = FALSE]
    msg <- guardar_curados(rv$cur, paste0("Panel admin: quitar ", length(sel), " fondo(s)"))
    refrescar_fondos_globales(); rv$tick <- rv$tick + 1
    output$cat_del_msg <- renderText(paste0("🗑 Quitado(s): ", nombres, ". ", msg))
  })

  output$dash <- renderUI({
    rv$tick; datos_ver()
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
        HTML("✅ Se inició la actualización: la app está re-consultando la CMF.<br><br>En <b>~3 a 5 minutos</b> el dashboard se actualizará solo con los datos nuevos.")
      else paste("No se pudo iniciar:", res$msg),
      easyClose = TRUE, footer = modalButton("Cerrar")))
  })

  # ---- Dividendos: subir + persistir ----
  estado_div <- function() {
    store_sync_fresh("dividendos.xlsx")
    if (file.exists("data/dividendos.xlsx"))
      paste0("Archivo guardado: dividendos.xlsx (", format(file.info("data/dividendos.xlsx")$mtime, "%d/%m %H:%M"), ")")
    else "Aún no se ha subido ningún archivo de dividendos."
  }
  output$div_msg <- renderText(estado_div())
  observeEvent(input$div_file, {
    # Se sube recortado (recortar_boletin: solo lo que se usa, ~1/3 del peso). Si
    # el archivo no tiene la estructura esperada, se sube tal cual y se avisa.
    ruta <- input$div_file$datapath; nota <- ""
    rec <- tryCatch(recortar_boletin(ruta, tempfile(fileext = ".xlsx")),
                    error = function(e) { nota <<- paste0(" (sin recortar: ", conditionMessage(e), ")"); NULL })
    if (!is.null(rec)) ruta <- rec
    msg <- tryCatch(paste0(store_write_file("dividendos.xlsx", ruta, "Subir dividendos desde la app"), nota),
                    error = function(e) paste("Error:", e$message))
    output$div_msg <- renderText(paste0(msg, "\n", estado_div())); rv$tick <- rv$tick + 1
  })

  # ---- Tabla de dividendos cargados ----
  output$tbl_div_data <- renderDT({
    rv$tick
    store_sync_fresh("dividendos.xlsx")
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
  # editar celdas NO debe re-renderizar (isolate + tick), mismo motivo que tbl_cur:
  # re-renderizar en cada edicion resetea la tabla (pagina/orden) a mitad del tipeo.
  output$tbl_entry <- renderDT({
    rv$tick
    datatable(isolate(rv$entry), editable = list(target = "cell", disable = list(columns = c(0,1))),
             rownames = FALSE, options = list(dom = "t"))
  }, server = FALSE)
  observeEvent(input$tbl_entry_cell_edit, { rv$entry <- editData(rv$entry, input$tbl_entry_cell_edit, rownames = FALSE) })
  observeEvent(input$save_vc_dia, {
    f <- as.character(input$vc_fecha)
    add <- rv$entry[!is.na(rv$entry$VC) & rv$entry$VC != "", , drop = FALSE]
    if (!nrow(add)) { output$vc_msg <- renderText("No ingresaste ningún VC."); return() }
    nuevas <- data.frame(Nombre = add$Nombre, Tipo = add$Tipo, Fecha = f,
                         Valor = suppressWarnings(as.numeric(add$VC)), stringsAsFactors = FALSE)
    # Releer el archivo mas reciente de GitHub (no la copia en memoria de esta
    # sesion) antes de fusionar: si hay otra pestana/sesion abierta que guardo
    # despues de que esta sesion cargo, evita pisar/borrar esos datos.
    # OJO: store_sync/.leer_csv lee de raw.githubusercontent.com, un CDN con
    # cache de ~5 min -> guardar dos veces seguido (ej. corregir un VC recien
    # ingresado) releia la version VIEJA y borraba lo recien commiteado. Por
    # eso aqui se usa la API de contenidos de GitHub (siempre fresca).
    actual <- tryCatch({
      txt <- store_read_fresh_text("manuales_vc.csv")
      if (is.null(txt)) .leer_csv("manuales_vc.csv", default_vc())
      else read.csv(text = txt, stringsAsFactors = FALSE, check.names = FALSE,
                    colClasses = c(Fecha = "character"))
    }, error = function(e) rv$vc)
    keep <- actual[!(paste(actual$Nombre, actual$Fecha) %in% paste(nuevas$Nombre, nuevas$Fecha)), , drop = FALSE]
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
  # isolate + tick (idem tbl_cur): editar celdas NO debe re-renderizar, si no la
  # tabla se reordena (esta ordenada por Fecha desc) a mitad de la correccion y
  # la siguiente edicion cae en una fila distinta a la que se estaba corrigiendo.
  output$tbl_vc <- renderDT({
    rv$tick
    datatable(isolate(rv$vc), editable = TRUE, rownames = FALSE,
      options = list(pageLength = 10, order = list(list(2, "desc")),
                     lengthMenu = list(c(10, 25, 50, -1), c("10","25","50","Todas"))))
  }, server = FALSE)
  # VC (Nombre|Fecha) que esta sesion borro o cambio de nombre/fecha: al guardar
  # no se reviven aunque sigan en GitHub
  vc_quitados <- character()
  observeEvent(input$tbl_vc_cell_edit, {
    info <- input$tbl_vc_cell_edit
    if (info$col %in% c(0, 2) && info$row <= nrow(rv$vc))   # Nombre o Fecha: cambia la llave
      vc_quitados <<- c(vc_quitados, paste(rv$vc$Nombre[info$row], rv$vc$Fecha[info$row]))
    rv$vc <- editData(rv$vc, info, rownames = FALSE)
  })
  observeEvent(input$del_vc, {
    sel <- input$tbl_vc_rows_selected
    if (length(sel)) {
      vc_quitados <<- c(vc_quitados, paste(rv$vc$Nombre[sel], rv$vc$Fecha[sel]))
      rv$vc <- rv$vc[-sel, , drop = FALSE]; rv$tick <- rv$tick + 1
    }
  })
  # Guardar el historial editado: la tabla de esta sesion puede ser vieja (se
  # cargo al abrir la pagina). Antes de escribir, se suman los VC que estan en
  # GitHub y no en la tabla (ingresados desde otra sesion), salvo los que esta
  # sesion borro: asi corregir un valor nunca borra lo que otro ingreso.
  observeEvent(input$save_vc, {
    fresco <- .leer_csv("manuales_vc.csv", default_vc())
    k <- paste(fresco$Nombre, fresco$Fecha)
    faltan <- !(k %in% paste(rv$vc$Nombre, rv$vc$Fecha)) & !(k %in% vc_quitados)
    if (any(faltan) && all(names(rv$vc) %in% names(fresco))) {
      rv$vc <- rbind(rv$vc, fresco[faltan, names(rv$vc), drop = FALSE])
      rv$vc <- rv$vc[order(rv$vc$Nombre, rv$vc$Fecha), ]
    }
    msg <- guardar_vc(); vc_quitados <<- character()
    output$vc_hist_msg <- renderText(paste0(msg, if (any(faltan)) paste0(" (se conservaron ", sum(faltan), " VC ingresados desde otra sesión)") else ""))
    rv$tick <- rv$tick + 1
  })

  # ---- Correcciones de dividendos ----
  # isolate + tick (idem tbl_vc/tbl_cur): evita que re-renderizar en cada edicion
  # reordene la tabla (esta ordenada por Fecha limite desc) a mitad de la correccion.
  output$tbl_ov <- renderDT({
    rv$tick
    datatable(isolate(rv$ov), editable = TRUE, rownames = FALSE,
      options = list(pageLength = 10, order = list(list(1, "desc")),
                     lengthMenu = list(c(10, 25, 50, -1), c("10","25","50","Todas"))))
  }, server = FALSE)
  observeEvent(input$tbl_ov_cell_edit, { rv$ov <- editData(rv$ov, input$tbl_ov_cell_edit, rownames = FALSE) })
  observeEvent(input$add_ov, {
    rv$ov <- rbind(rv$ov, data.frame(Fondo = "", `Fecha limite` = "", Monto = NA_real_,
                                     check.names = FALSE, stringsAsFactors = FALSE))
    rv$tick <- rv$tick + 1
  })
  guardar_ov <- function() store_write_text("dividendos_overrides.csv",
      paste(c(paste(names(rv$ov), collapse = ","),
              if (nrow(rv$ov)) apply(rv$ov, 1, function(x) paste(x, collapse = ","))), collapse = "\n"),
      "Corregir dividendos")
  observeEvent(input$save_ov, {
    msg <- guardar_ov()
    output$ov_msg <- renderText(msg); rv$tick <- rv$tick + 1
  })
}

shinyApp(ui, server)
