"""Guarda el cierre diario del IPSA de Yahoo (^767564-CLP-GRTR / MXIPSAGC.SN) en data/ipsa_yahoo.csv.

Yahoo solo entrega el dato del dia (sin historico), por eso se acumula dia a dia.
Es la fuente del IPSA del Pulso para los dias que el BCCh aun no publica
(dashboard, actualizacion diaria y correo diario). Validado contra el IPSA
manual del 26-10 al 02-11-2026 (diferencias < 0,01%).
Lo corre .github/workflows/ipsa_yahoo.yml despues del cierre de la bolsa.
"""
import csv, json, os, sys, urllib.parse, urllib.request
from datetime import datetime, timedelta, timezone

# Mismo indice (MSCI IPSA, cierres identicos) en dos tickers de Yahoo: el de MSCI
# (el que se ve en es.finance.yahoo.com/quote/^767564-CLP-GRTR) y, de respaldo,
# el de la Bolsa de Santiago.
TICKERS, RUTA = ["^767564-CLP-GRTR", "MXIPSAGC.SN"], "data/ipsa_yahoo.csv"


def cierre(ticker):
    url = ("https://query1.finance.yahoo.com/v8/finance/chart/" + urllib.parse.quote(ticker)
           + "?interval=1d&range=5d")
    req = urllib.request.Request(url, headers={"User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64)"})
    with urllib.request.urlopen(req, timeout=30) as r:
        meta = json.load(r)["chart"]["result"][0]["meta"]
    precio, t = meta.get("regularMarketPrice"), meta.get("regularMarketTime")
    if precio is None or t is None:
        raise ValueError("sin regularMarketPrice")
    tz = timezone(timedelta(seconds=meta.get("gmtoffset", -10800)))
    return datetime.fromtimestamp(t, tz).date().isoformat(), precio


fecha = precio = TICKER = None
for TICKER in TICKERS:
    try:
        fecha, precio = cierre(TICKER)
        break
    except Exception as e:  # noqa: BLE001 - se prueba el siguiente ticker
        print(f"{TICKER}: {e}", file=sys.stderr)
if precio is None:
    sys.exit("Yahoo no entrego el IPSA con ningun ticker")

filas = {}
if os.path.exists(RUTA):
    with open(RUTA, encoding="utf-8") as f:
        filas = {r["fecha"]: r for r in csv.DictReader(f)}
filas[fecha] = {"fecha": fecha, "cierre": f"{precio:.2f}", "ticker": TICKER,
                "consultado_utc": datetime.now(timezone.utc).strftime("%Y-%m-%d %H:%M")}
with open(RUTA, "w", encoding="utf-8", newline="") as f:
    w = csv.DictWriter(f, fieldnames=["fecha", "cierre", "ticker", "consultado_utc"])
    w.writeheader()
    for k in sorted(filas):
        w.writerow(filas[k])
print(f"IPSA Yahoo {fecha}: {precio:.2f} ({len(filas)} dias guardados)")
