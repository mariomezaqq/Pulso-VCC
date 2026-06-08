# =============================================================================
# CATALOGO CURADO PULSO VCC  (extraido de resumen_semanal_ONEDRIVE.R)
# Fondos aprobados, categorias, mapeo tickers (dividendos), indices y metadata.
# =============================================================================

# ---- Mapeo nombre_excel <-> ticker SEBRA (dividendos Bolsa) ----
MAPEO_SEBRA <- list(
  list(nombre_excel = "FI Albion",                         ticker_sebra = "CFIALBIONA"),
  list(nombre_excel = "Amc Biotech",                       ticker_sebra = NA),
  list(nombre_excel = "MBI Deuda Total (B)",               ticker_sebra = "CFIMBIDT-B"),
  list(nombre_excel = "FFMM LV ahorro capital (F)",        ticker_sebra = NA),
  list(nombre_excel = "Fynsa deuda chile",                 ticker_sebra = "CFIFYNSADA"),
  list(nombre_excel = "FFMM BTG renta local",              ticker_sebra = NA),
  list(nombre_excel = "MBI Deuda corporativa A",           ticker_sebra = "CFIMBDCAPV"),
  list(nombre_excel = "FFMM LV ahorro corporativo (F)",    ticker_sebra = NA),
  list(nombre_excel = "Moneda deuda chile (A)",            ticker_sebra = "CFIMDCHA"),
  list(nombre_excel = "LV Deuda chile (A)",                ticker_sebra = "CFILVCOR-A"),
  list(nombre_excel = "BTG deuda activa plus A",           ticker_sebra = "CFIBTGDAPA"),
  list(nombre_excel = "BTG deuda corporativa A",           ticker_sebra = "CFIBPDCCHA"),
  list(nombre_excel = "Quest renta global hedge",          ticker_sebra = "CFIQRGH"),
  list(nombre_excel = "Quest renta global A",              ticker_sebra = "CFIQRGA-E"),
  list(nombre_excel = "FFMM BTG renta latam high yield",   ticker_sebra = NA),
  list(nombre_excel = "Mbi global fixed income",           ticker_sebra = "CFI-MBIGFA"),
  list(nombre_excel = "Moneda deuda latam (R)",            ticker_sebra = "CFI-MDLATR"),
  list(nombre_excel = "MBI deuda plus A",                  ticker_sebra = "CFIMBIRF-A"),
  list(nombre_excel = "MBI RF plus dolar U",               ticker_sebra = "CFI-MBRFUS"),
  list(nombre_excel = "Moneda renta clp R",                ticker_sebra = "CFIMRCLPR"),
  list(nombre_excel = "LV Enfoque F",                      ticker_sebra = NA),
  list(nombre_excel = "LV Enfoque R",                      ticker_sebra = "CFMLVENFR"),
  list(nombre_excel = "Toesca chile equities",             ticker_sebra = "CFMTOEEQUB"),
  list(nombre_excel = "Conviccion mbi",                    ticker_sebra = "CFIMBICL-A"),
  list(nombre_excel = "Fynsa retorno total",               ticker_sebra = "CFIFYNTORA"),
  list(nombre_excel = "Moneda pionero A",                  ticker_sebra = "CFIPIONERO"),
  list(nombre_excel = "Mbi deuda alternativa A",           ticker_sebra = "CFIMBIDA-A"),
  list(nombre_excel = "BTG liquidez alternativa",          ticker_sebra = "CFIBTGPLAA"),
  list(nombre_excel = "BTG deuda estrategica A",           ticker_sebra = "CFIBTGDEA"),
  list(nombre_excel = "Toesca facturas",                   ticker_sebra = "CFITDPFI-E"),
  list(nombre_excel = "Ameris AAtech",                     ticker_sebra = "CFIAMSLPA"),
  list(nombre_excel = "Fynsa deuda privada",               ticker_sebra = "CFIFYNDEPA"),
  list(nombre_excel = "HMC renta global clp bp",           ticker_sebra = "CFIHMCRGPB"),
  list(nombre_excel = "Toesca deuda privada A",            ticker_sebra = "CFITODPA-E"),
  list(nombre_excel = "Toesca deuda privada X",            ticker_sebra = "CFITODPX-E"),
  list(nombre_excel = "Toesca deuda privada F",            ticker_sebra = "CFITODPF-E"),
  list(nombre_excel = "Quest Fin Inmb.",                   ticker_sebra = "CFIQFIA-E"),
  list(nombre_excel = "Fynsa Deuda Inmob.",                ticker_sebra = "CFIFYDIA-E"),
  list(nombre_excel = "Toesca us credit B",                ticker_sebra = "CFI-TUSCBE"),
  list(nombre_excel = "HMC renta global usd bp",           ticker_sebra = "CFI-HRGDBP"),
  list(nombre_excel = "HMC deuda privada global bp",       ticker_sebra = "CFI-HMCDGD"),
  list(nombre_excel = "MBI Deuda estructurada",            ticker_sebra = "CFI-MBICHA"),
  list(nombre_excel = "Falcom Chilean Fixed Income A",     ticker_sebra = "CFIFALCFIA"),
  list(nombre_excel = "BTG Credito Privado A (Desde el 26/03/2026)", ticker_sebra = "CFIBTGCRPA"),
  list(nombre_excel = "BTG Credito Privado B (Desde el 07/04/2026)", ticker_sebra = "CFIBTGCRPB"),
  list(nombre_excel = "FM BTG Chile Acción F",             ticker_sebra = NA)
)

# ---- Fondos / indices manuales ----
NOMBRES_FONDOS_MANUALES <- c("Amc Biotech", "Quest Fin Inmb.", "Fynsa Deuda Inmob.", "MBI Deuda estructurada")

NOMBRES_INDICES_MANUALES <- c("Legatruh")

# ---- Reglas especiales de calculo ----
FONDOS_YTD_DESDE_INICIO <- c(
  "BTG Credito Privado A (Desde el 26/03/2026)",
  "BTG Credito Privado B (Desde el 07/04/2026)"
)

FONDOS_CON_FACTOR_AJUSTE <- c(
  "Toesca chile equities",
  "LV Enfoque F",
  "LV Enfoque R",
  "FM BTG Chile Acción F"
)

# ---- Definicion de fondos ----
FONDOS <- list(

  # --- Fondos de Inversion Rescatables (FIRES) ---
  list(nombre = "MBI Deuda Total (B)",          run = "9202",  serie = "B",     row = "AAAw+cAAhAABP4OAAk"),
  list(nombre = "Fynsa deuda chile",            run = "9559",  serie = "B",     row = "AAAw%20cAAhAABP4RAA3"),
  list(nombre = "MBI DEUDA CORPORATIVA A",      run = "10162", serie = "A",     row = "AAAw%20cAAhAABQKEAAm"),
  list(nombre = "MONEDA DEUDA CHILE A",         run = "9212",  serie = "A",     row = "AAAw+cAAhAABP4PAAi"),
  list(nombre = "LV Deuda chile (A)",           run = "7210",  serie = "A",     row = "AAAw+cAAhAABP4IAAe"),
  list(nombre = "BTG deuda activa plus A",      run = "9358",  serie = "A",     row = "AABbsrAAjAAAAF9AAs"),
  list(nombre = "BTG deuda corporativa A",      run = "7184",  serie = "A",     row = "AAAw%20cAAhAABP4CAA6"),
  list(nombre = "Quest renta global hedge",     run = "9336",  serie = "UNICA", row = "AAAw%20cAAhAABP4NAA9"),
  list(nombre = "Mbi global fixed income",      run = "9950",  serie = "A",     row = "AAAw+cAAhAABQKAAAM"),
  list(nombre = "MBI deuda plus A",             run = "9077",  serie = "A",     row = "AAAw+cAAhAABP4JAAW"),
  list(nombre = "MBI RF plus dolar U",          run = "9091",  serie = "U",     row = "AAAw%20cAAhAABP4KAAG"),
  list(nombre = "Conviccion mbi",               run = "10381", serie = "A",     row = "AAAw+cAAhAABQKDAA6"),
  list(nombre = "Fynsa retorno total",          run = "9820",  serie = "A",     row = "AAAw%20cAAhAABP4WAAs"),
  list(nombre = "BTG liquidez alternativa",     run = "10145", serie = "A",     row = "AAAw%20cAAhAABQKBAAe"),
  list(nombre = "Toesca facturas",              run = "9970",  serie = "I",     row = "AAAw+cAAhAABQKEAAL"),
  list(nombre = "Ameris AAtech",                run = "9919",  serie = "A",     row = "AAAw%20cAAhAABQKBAAC"),
  list(nombre = "HMC renta global clp bp",      run = "9769",  serie = "BP",    row = "AAAw%20cAAhAABP4UAAt"),
  list(nombre = "HMC renta global usd bp",      run = "10283", serie = "BP",    row = "AAAw+cAAhAABQKGAAr"),
  list(nombre = "HMC deuda privada global",     run = "9769",  serie = "BP",    row = "AAAw%20cAAhAABQKDAAF"),
  list(nombre = "Mbi deuda alternativa A",      run = "9203",  serie = "A",     row = "ABbsrAAjAAAAF6AAO"),
  list(nombre = "Quest renta global A",         run = "9274",  serie = "A",     row = "AAAw%20cAAhAABP4NAAs"),
  list(nombre = "HMC deuda privada global bp",  run = "9914",  serie = "BP",    row = "AAAw%20cAAhAABQKDAAF"),
  list(nombre = "Falcom Chilean Fixed Income A",run = "9289",  serie = "A",     row = "AAAw+cAAhAABP4NAAv"),

  # --- Fondos de Inversion NO Rescatables (FINRE) ---
  list(nombre = "Moneda deuda latam R",      run = "7055",  serie = "R", row = "AABbsrAAjAAAAFyAAF",   tipoentidad = "FINRE"),
  list(nombre = "Moneda pionero A",          run = "7010",  serie = "A", row = "AABbsrAAjAAAAFxAAZ",   tipoentidad = "FINRE"),
  list(nombre = "Moneda renta clp R",        run = "7099",  serie = "R", row = "",                     tipoentidad = "FINRE"),
  list(nombre = "BTG deuda estrategica A",   run = "10641", serie = "A", row = "AAAw%20cAAhAABQK7AA",  tipoentidad = "FINRE"),
  list(nombre = "Fynsa deuda privada",       run = "9759",  serie = "A", row = "AAAw%20cAAhAABP4TAAl", tipoentidad = "FINRE"),
  list(nombre = "Toesca deuda privada A",    run = "9619",  serie = "A", row = "AAAw+cAAhAABP4TAAL",   tipoentidad = "FINRE"),
  list(nombre = "Toesca deuda privada X",    run = "9619",  serie = "X", row = "AAAw+cAAhAABP4TAAL",   tipoentidad = "FINRE"),
  list(nombre = "Toesca deuda privada F",    run = "9619",  serie = "F", row = "AAAw+cAAhAABP4TAAL",   tipoentidad = "FINRE"),
  list(nombre = "Toesca us credit B",        run = "10437", serie = "B", row = "AAAw+cAAhAABQK5AAH",   tipoentidad = "FINRE"),
  list(nombre = "FI Albion",                 run = "10757", serie = "A", row = "AABbsrAAjAAAAFyAAF",   tipoentidad = "FINRE"),
  list(nombre = "BTG Credito Privado A (Desde el 26/03/2026)", run = "10843", serie = "A", row = "AAAw+cAAhAABQlXAAA", tipoentidad = "FINRE"),
  list(nombre = "BTG Credito Privado B (Desde el 07/04/2026)", run = "10843", serie = "B", row = "AAAw+cAAhAABQlXAAA", tipoentidad = "FINRE"),

  # --- Fondos Mutuos (RGFMU) ---
  list(nombre = "LV Enfoque F",                   run = "8723", serie = "F", row = "AAAw%20cAAhAABPt9AAE", tipoentidad = "RGFMU"),
  list(nombre = "LV Enfoque R",                   run = "8723", serie = "R", row = "AAAw%20cAAhAABPt9AAE", tipoentidad = "RGFMU"),
  list(nombre = "FFMM BTG renta local A",          run = "8897", serie = "A", row = "AABbsrAAjAAAAFzAAl",   tipoentidad = "RGFMU"),
  list(nombre = "FFMM LV ahorro capital (F)",      run = "8263", serie = "F", row = "AABbsrAAjAAAACfAA1",   tipoentidad = "RGFMU"),
  list(nombre = "FFMM LV ahorro corporativo (F)",  run = "8315", serie = "F", row = "AAAw+cAAhAAAACbAAt",   tipoentidad = "RGFMU"),
  list(nombre = "FFMM BTG renta latam high yield", run = "8207", serie = "A", row = "AAAw%20cAAhAAAACfAAL", tipoentidad = "RGFMU"),
  list(nombre = "Toesca chile equities",           run = "9414", serie = "B", row = "AABbsrAAjAAAAF7AA2",   tipoentidad = "RGFMU"),
  list(nombre = "FM BCI Cartera dinámica Conservadora",          run = "8638", serie = "BPRIV", row = "AAAw+cAAhAABPt6AAA", tipoentidad = "RGFMU"),
  list(nombre = "FM BCI Cartera Patrimonial Conservadora",       run = "9063", serie = "BPRIV", row = "AAAw+cAAhAABP4GAA2", tipoentidad = "RGFMU"),
  list(nombre = "FM SURA Multiactivo Prudente",                  run = "8773", serie = "A",     row = "AABbsrAAjAAAAFtAAz",  tipoentidad = "RGFMU"),
  list(nombre = "FM LV Cuenta Activa Conservadora",              run = "9192", serie = "A",     row = "AABbsrAAjAAAAF9AAK",  tipoentidad = "RGFMU"),
  list(nombre = "FM BTG Gestión Conservadora",                   run = "9872", serie = "A",     row = "AAAw%20cAAhAABP4TAA", tipoentidad = "RGFMU"),
  list(nombre = "FM BTG Chile Acción F",                         run = "8898", serie = "F",     row = "AAAw%20cAAhAABP4EAAH", tipoentidad = "RGFMU")
)

# ---- Categorias y orden ----
CATEGORIAS <- list(

  list(hoja = "De la Casa", titulo = "De la Casa",
    fondos = list(
      list(nombre_script = "FI Albion",     nombre_excel = "FI Albion"),
      list(nombre_script = "__MANUAL__",    nombre_excel = "Amc Biotech")
    )
  ),

  list(hoja = "Renta Fija CP", titulo = "Renta Fija Corto Plazo",
    fondos = list(
      list(nombre_script = "MBI Deuda Total (B)",          nombre_excel = "MBI Deuda Total (B)"),
      list(nombre_script = "FFMM LV ahorro capital (F)",   nombre_excel = "FFMM LV ahorro capital (F)"),
      list(nombre_script = "Fynsa deuda chile",            nombre_excel = "Fynsa deuda chile"),
      list(nombre_script = "FFMM BTG renta local A",       nombre_excel = "FFMM BTG renta local")
    )
  ),

  list(hoja = "Renta Fija MP", titulo = "Renta Fija Mediano plazo",
    fondos = list(
      list(nombre_script = "MBI DEUDA CORPORATIVA A",        nombre_excel = "MBI Deuda corporativa A"),
      list(nombre_script = "FFMM LV ahorro corporativo (F)", nombre_excel = "FFMM LV ahorro corporativo (F)"),
      list(nombre_script = "MONEDA DEUDA CHILE A",           nombre_excel = "Moneda deuda chile (A)"),
      list(nombre_script = "LV Deuda chile (A)",             nombre_excel = "LV Deuda chile (A)"),
      list(nombre_script = "BTG deuda activa plus A",        nombre_excel = "BTG deuda activa plus A"),
      list(nombre_script = "BTG deuda corporativa A",        nombre_excel = "BTG deuda corporativa A"),
      list(nombre_script = "Falcom Chilean Fixed Income A",  nombre_excel = "Falcom Chilean Fixed Income A")
    )
  ),

  list(hoja = "Renta Fija Internacional", titulo = "Renta Fija Internacional",
    fondos = list(
      list(nombre_script = "Quest renta global hedge",        nombre_excel = "Quest renta global hedge"),
      list(nombre_script = "Quest renta global A",            nombre_excel = "Quest renta global A"),
      list(nombre_script = "FFMM BTG renta latam high yield", nombre_excel = "FFMM BTG renta latam high yield"),
      list(nombre_script = "Mbi global fixed income",         nombre_excel = "Mbi global fixed income"),
      list(nombre_script = "Moneda deuda latam R",            nombre_excel = "Moneda deuda latam (R)")
    )
  ),

  list(hoja = "Retorno total", titulo = "Retorno total",
    fondos = list(
      list(nombre_script = "MBI deuda plus A",    nombre_excel = "MBI deuda plus A"),
      list(nombre_script = "MBI RF plus dolar U", nombre_excel = "MBI RF plus dolar U"),
      list(nombre_script = "Moneda renta clp R",  nombre_excel = "Moneda renta clp R")
    )
  ),

  list(hoja = "Renta Variable local", titulo = "Renta Variable local",
    fondos = list(
      list(nombre_script = "LV Enfoque F",            nombre_excel = "LV Enfoque F"),
      list(nombre_script = "LV Enfoque R",            nombre_excel = "LV Enfoque R"),
      list(nombre_script = "Toesca chile equities",   nombre_excel = "Toesca chile equities"),
      list(nombre_script = "Conviccion mbi",          nombre_excel = "Conviccion mbi"),
      list(nombre_script = "Fynsa retorno total",     nombre_excel = "Fynsa retorno total"),
      list(nombre_script = "Moneda pionero A",        nombre_excel = "Moneda pionero A"),
      list(nombre_script = "FM BTG Chile Acción F",   nombre_excel = "FM BTG Chile Acción F")
    )
  ),

  list(hoja = "Alternativos CLP", titulo = "Alternativos CLP",
    fondos = list(
      list(nombre_script = "Mbi deuda alternativa A",  nombre_excel = "Mbi deuda alternativa A"),
      list(nombre_script = "BTG liquidez alternativa",  nombre_excel = "BTG liquidez alternativa"),
      list(nombre_script = "BTG deuda estrategica A",   nombre_excel = "BTG deuda estrategica A"),
      list(nombre_script = "Toesca facturas",           nombre_excel = "Toesca facturas"),
      list(nombre_script = "Ameris AAtech",             nombre_excel = "Ameris AAtech"),
      list(nombre_script = "Fynsa deuda privada",       nombre_excel = "Fynsa deuda privada"),
      list(nombre_script = "HMC renta global clp bp",   nombre_excel = "HMC renta global clp bp"),
      list(nombre_script = "Toesca deuda privada A",    nombre_excel = "Toesca deuda privada A"),
      list(nombre_script = "Toesca deuda privada X",    nombre_excel = "Toesca deuda privada X"),
      list(nombre_script = "Toesca deuda privada F",    nombre_excel = "Toesca deuda privada F"),
      list(nombre_script = "BTG Credito Privado A (Desde el 26/03/2026)", nombre_excel = "BTG Credito Privado A (Desde el 26/03/2026)"),
      list(nombre_script = "BTG Credito Privado B (Desde el 07/04/2026)", nombre_excel = "BTG Credito Privado B (Desde el 07/04/2026)"),
      list(nombre_script = "__MANUAL__", nombre_excel = "Quest Fin Inmb."),
      list(nombre_script = "__MANUAL__", nombre_excel = "Fynsa Deuda Inmob.")
    )
  ),

  list(hoja = "Alternativos USD", titulo = "Alternativos USD",
    fondos = list(
      list(nombre_script = "Toesca us credit B",         nombre_excel = "Toesca us credit B"),
      list(nombre_script = "HMC renta global usd bp",    nombre_excel = "HMC renta global usd bp"),
      list(nombre_script = "HMC deuda privada global bp",nombre_excel = "HMC deuda privada global bp"),
      list(nombre_script = "__MANUAL__",                 nombre_excel = "MBI Deuda estructurada")
    )
  ),

  list(hoja = "Comparables Albion", titulo = "Comparables Albion",
    fondos = list(
      list(nombre_script = "FI Albion",                              nombre_excel = "FI Albion"),
      list(nombre_script = "FM BCI Cartera dinámica Conservadora",   nombre_excel = "FM BCI Cartera dinámica Conservadora"),
      list(nombre_script = "FM BCI Cartera Patrimonial Conservadora",nombre_excel = "FM BCI Cartera Patrimonial Conservadora"),
      list(nombre_script = "FM SURA Multiactivo Prudente",           nombre_excel = "FM SURA Multiactivo Prudente"),
      list(nombre_script = "FM LV Cuenta Activa Conservadora",       nombre_excel = "FM LV Cuenta Activa Conservadora"),
      list(nombre_script = "FM BTG Gestión Conservadora",            nombre_excel = "FM BTG Gestión Conservadora")
    )
  )
)

# ---- Indices macro ----
INDICES <- list(
  list(nombre = "IPSA",           ticker = "",      fuente = "bcch"),
  list(nombre = "S&P500",         ticker = "^GSPC"),
  list(nombre = "Nasdaq",         ticker = "^NDX"),
  list(nombre = "ACWI",           ticker = "ACWI"),
  list(nombre = "Legatruh",       ticker = ""),
  list(nombre = "Oro",            ticker = "GC=F"),
  list(nombre = "Cobre",          ticker = "HG=F"),
  list(nombre = "Petroleo Brent", ticker = "BZ=F")
)

# ---- Metadata estatica por fondo ----
DATOS_FONDO <- list(
  "FI Albion"                         = list(rent2024="", rent2025="3,39%",   duracion="0",   liquidez="48 hrs",        moneda="CLP", tac=""),
  "Amc Biotech"                       = list(rent2024="",       rent2025="138,80%",        duracion="",    liquidez="",              moneda="USD", tac=""),
  "MBI Deuda Total (B)"               = list(rent2024="11,48%", rent2025="7,68%",   duracion="3",   liquidez="48 hrs",        moneda="CLP", tac="0,95%"),
  "FFMM LV ahorro capital (F)"        = list(rent2024="8,82%",  rent2025="5,75%",   duracion="2,9", liquidez="24 hrs",        moneda="CLP", tac="1,10%"),
  "Fynsa deuda chile"                 = list(rent2024="9,03%",  rent2025="6,00%",   duracion="2",   liquidez="t + 11",        moneda="CLP", tac="0,70%"),
  "FFMM BTG renta local"              = list(rent2024="9,47%",  rent2025="5,31%",   duracion="2,7", liquidez="24 hrs",        moneda="CLP", tac="0,50%"),
  "MBI Deuda corporativa A"           = list(rent2024="8,94%",  rent2025="8,29%",   duracion="4,5", liquidez="48 hrs",        moneda="CLP", tac="0,75%"),
  "FFMM LV ahorro corporativo (F)"    = list(rent2024="8,97%",  rent2025="6,84%",   duracion="4,3", liquidez="24 hrs",        moneda="CLP", tac="1,10%"),
  "Moneda deuda chile (A)"            = list(rent2024="9,60%",  rent2025="8,80%",   duracion="4,4", liquidez="t + 11",        moneda="CLP", tac="0,71%"),
  "LV Deuda chile (A)"                = list(rent2024="9,66%",  rent2025="8,27%",   duracion="4,2", liquidez="t +10",         moneda="CLP", tac="1,25%"),
  "BTG deuda activa plus A"           = list(rent2024="16,60%", rent2025="7,08%",   duracion="5,07",liquidez="t+10",          moneda="CLP", tac="1,19%"),
  "BTG deuda corporativa A"           = list(rent2024="13,03%", rent2025="8,49%",   duracion="4,7", liquidez="t +10",         moneda="CLP", tac="0,70%"),
  "Quest renta global hedge"          = list(rent2024="8,19%",  rent2025="9,97%",   duracion="5,7", liquidez="50 dias corr",  moneda="CLP", tac="0,90%"),
  "Quest renta global A"              = list(rent2024="6,72%",  rent2025="10,20%",  duracion="5,9", liquidez="50 dias corr",  moneda="USD", tac="0,90%"),
  "FFMM BTG renta latam high yield"   = list(rent2024="8,69%",  rent2025="4,46%",   duracion="6",   liquidez="6 dias",        moneda="USD", tac="2,38%"),
  "Mbi global fixed income"           = list(rent2024="4,52%",  rent2025="8,49%",   duracion="4,5", liquidez="11-179 dias",   moneda="USD", tac="1,18%"),
  "Moneda deuda latam (R)"            = list(rent2024="15,30%", rent2025="10,60%",  duracion="3,8", liquidez="4 veces al ano",moneda="USD", tac="1,64%"),
  "MBI deuda plus A"                  = list(rent2024="12,34%", rent2025="10,92%",  duracion="5,3", liquidez="48 hrs",        moneda="CLP", tac="1,84%"),
  "MBI RF plus dolar U"               = list(rent2024="10,95%", rent2025="10,86%",  duracion="5,2", liquidez="48 hrs",        moneda="USD", tac="1,00%"),
  "Moneda renta clp R"                = list(rent2024="15,00%", rent2025="10,60%",  duracion="3,1", liquidez="4 veces al ano",moneda="CLP", tac="1,55%"),
  "LV Enfoque F"                      = list(rent2024="6,09%",  rent2025="68,31%",  duracion="",    liquidez="t + 7",         moneda="CLP", tac="1,80%"),
  "LV Enfoque R"                      = list(rent2024="",       rent2025="28,74%(1/2 año)",duracion="",liquidez="t + 7",      moneda="CLP", tac="1,80%"),
  "Toesca chile equities"             = list(rent2024="6,50%",  rent2025="60,97%",  duracion="",    liquidez="t + 2",         moneda="CLP", tac="1,79%"),
  "Conviccion mbi"                    = list(rent2024="9,55%",  rent2025="37,25%",  duracion="",    liquidez="t+11",          moneda="CLP", tac="solo perf fee"),
  "Fynsa retorno total"               = list(rent2024="-1,11%", rent2025="63,47%",  duracion="",    liquidez="t+11",          moneda="CLP", tac="0,36%"),
  "Moneda pionero A"                  = list(rent2024="11,00%", rent2025="68,60%",  duracion="",    liquidez="ult dia mes",   moneda="CLP", tac="1,08%"),
  "Mbi deuda alternativa A"           = list(rent2024="9,45%",  rent2025="6,61%",   duracion="",    liquidez="t+11",          moneda="CLP", tac="1,13%"),
  "BTG liquidez alternativa"          = list(rent2024="9,77%",  rent2025="8,16%",   duracion="",    liquidez="10 dias",       moneda="CLP", tac="1,00%"),
  "BTG deuda estrategica A"           = list(rent2024="0,00%",  rent2025="8,62%",   duracion="",    liquidez="45 dias",       moneda="CLP", tac="1,40%"),
  "Toesca facturas"                   = list(rent2024="7,04%",  rent2025="6,35%",   duracion="",    liquidez="24 hrs",        moneda="CLP", tac="1,30%"),
  "Ameris AAtech"                     = list(rent2024="7,92%",  rent2025="6,70%",   duracion="",    liquidez="t+10",          moneda="CLP", tac="1,50%"),
  "Fynsa deuda privada"               = list(rent2024="8,30%",  rent2025="8,31%",   duracion="",    liquidez="hasta 60 dias", moneda="CLP", tac="1,79%"),
  "HMC renta global clp bp"           = list(rent2024="15,14%", rent2025="5,36%",   duracion="",    liquidez="11 a 90 dias",  moneda="CLP", tac="1,19%"),
  "Toesca deuda privada A"            = list(rent2024="10,20%", rent2025="8,87%",   duracion="",    liquidez="mensual",       moneda="CLP", tac="1,19%"),
  "Toesca deuda privada X"            = list(rent2024="4,59%(1/2 año)",rent2025="7,88%",duracion="",liquidez="mensual",       moneda="CLP", tac="1,30%"),
  "Toesca deuda privada F"            = list(rent2024="9,14%",  rent2025="7,93%",   duracion="",    liquidez="mensual",       moneda="CLP", tac="0,95%"),
  "Quest Fin Inmb."                   = list(rent2024="3,7%(1/2 año)",rent2025="9,44%",duracion="", liquidez="",              moneda="CLP", tac=""),
  "Fynsa Deuda Inmob."                = list(rent2024="4,02%",  rent2025="3,31%",   duracion="",    liquidez="",              moneda="CLP", tac=""),
  "Toesca us credit B"                = list(rent2024="10,18%", rent2025="3,91%",   duracion="",    liquidez="",              moneda="USD", tac="1,50%"),
  "HMC renta global usd bp"           = list(rent2024="13,12%", rent2025="4,73%",   duracion="",    liquidez="11 a 90 dias",  moneda="USD", tac="1,19%"),
  "HMC deuda privada global bp"       = list(rent2024="7,05%",  rent2025="-18,46%", duracion="",    liquidez="",              moneda="USD", tac="1,19%"),
  "Falcom Chilean Fixed Income A"     = list(rent2024="10,09%", rent2025="9,34%",   duracion="4,1", liquidez="T+10",          moneda="CLP", tac="0,975%"),
  "BTG Credito Privado A (Desde el 26/03/2026)" = list(rent2024="",rent2025="",duracion="",liquidez="Mensual",moneda="CLP",tac="1,40%"),
  "BTG Credito Privado B (Desde el 07/04/2026)" = list(rent2024="",rent2025="",duracion="",liquidez="Mensual",moneda="CLP",tac="0,80%"),
  "FM BTG Chile Acción F"             = list(rent2024="12,66%", rent2025="61,53%",  duracion="",    liquidez="T+10",          moneda="CLP", tac="1,19%"),
  "FM BCI Cartera dinámica Conservadora"    = list(rent2024="8,80%",  rent2025="9,63%",  duracion="",liquidez="",moneda="CLP",tac="1,20%"),
  "FM BCI Cartera Patrimonial Conservadora" = list(rent2024="8,71%",  rent2025="9,31%",  duracion="",liquidez="",moneda="CLP",tac="1,05%"),
  "FM SURA Multiactivo Prudente"            = list(rent2024="5,57%",  rent2025="8,22%",  duracion="",liquidez="",moneda="CLP",tac="2,39%"),
  "FM LV Cuenta Activa Conservadora"        = list(rent2024="8,21%",  rent2025="8,63%",  duracion="",liquidez="",moneda="CLP",tac=""),
  "FM BTG Gestión Conservadora"             = list(rent2024="11,96%", rent2025="7,92%",  duracion="",liquidez="",moneda="CLP",tac="1,63")
)
