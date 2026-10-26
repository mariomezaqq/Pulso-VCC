"""Guarda el cierre diario del IPSA de Yahoo (MXIPSAGC.SN) en data/ipsa_yahoo.csv.

Yahoo solo entrega el dato del dia (sin historico) y el ticker se llama "MSCI IPSA
INDEX (con dividendos": por eso se acumula aparte, dia a dia, para compararlo con
el IPSA manual/oficial antes de usarlo en el dashboard. No entra en ningun calculo.
Lo corre .github/workflows/ipsa_yahoo.yml despues del cierre de la bolsa.
"""
import csv, json, os, sys, urllib.request
from datetime import datetime, timedelta, timezone

TICKER, RUTA = "MXIPSAGC.SN", "data/ipsa_yahoo.csv"
url = f"https://query1.finance.yahoo.com/v8/finance/chart/{TICKER}?interval=1d&range=5d"
req = urllib.request.Request(url, headers={"User-Agent": "Mozilla/5.0 (Windows NT 10.0; Win64; x64)"})
with urllib.request.urlopen(req, timeout=30) as r:
    res = json.load(r)["chart"]["result"][0]
meta = res["meta"]
tz = timezone(timedelta(seconds=meta.get("gmtoffset", -10800)))
precio, t = meta.get("regularMarketPrice"), meta.get("regularMarketTime")
if precio is None or t is None:
    sys.exit("Yahoo no trajo regularMarketPrice")
fecha = datetime.fromtimestamp(t, tz).date().isoformat()

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
