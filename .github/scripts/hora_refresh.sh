#!/usr/bin/env bash
# Decide si la corrida programada de refresh.yml debe actualizar los datos.
# GitHub atrasa los cron varias horas (el de las 11:00 corria a las 14-17), asi
# que el cron se programa cada hora y aqui se deja pasar solo la PRIMERA corrida
# que llega despues de las 11:00 de Chile en el dia: si ya hubo una
# actualizacion completa exitosa hoy desde las 11:00 (programada o manual), se
# salta. Las de "un solo fondo" (run-name "Solo fondo ...") no cuentan.
# Los disparos manuales (boton del panel, Actions) corren siempre.
#   uso: hora_refresh.sh <event_name>   -> imprime "correr=true|false"
set -uo pipefail
evento="${1:-schedule}"
repo="${GITHUB_REPOSITORY:-mariomezaqq/Pulso-VCC}"
if [ "$evento" = "workflow_dispatch" ]; then echo "correr=true"; exit 0; fi

hoy=$(TZ=America/Santiago date +%F); hhmm=$(TZ=America/Santiago date +%H%M)
if [ "$((10#$hhmm))" -lt 1100 ]; then
  echo "Chile $hoy $hhmm: todavia no son las 11:00" >&2; echo "correr=false"; exit 0
fi
desde=$(date -u -d "$(TZ=America/Santiago date -d "$hoy 11:00" '+%F %T %z')" +%FT%TZ)

api() { curl -sf -H "Accept: application/vnd.github+json" \
          ${GH_TOKEN:+-H "Authorization: Bearer $GH_TOKEN"} "https://api.github.com/repos/$repo/$1"; }
# si la API falla, mejor actualizar que saltarse el dia
ids=$(api "actions/workflows/refresh.yml/runs?status=success&per_page=50&created=%3E%3D$desde" |
      python3 -c 'import json,sys;[print(r["id"]) for r in json.load(sys.stdin)["workflow_runs"] if not r["display_title"].startswith("Solo fondo")]' 2>/dev/null) ||
  { echo "API no respondio: se actualiza" >&2; echo "correr=true"; exit 0; }
for id in $ids; do
  [ "$id" = "${GITHUB_RUN_ID:-}" ] && continue
  if api "actions/runs/$id/jobs" | python3 -c 'import json,sys
sys.exit(0 if any(j["name"] == "scrape" and j["conclusion"] == "success" for j in json.load(sys.stdin)["jobs"]) else 1)'; then
    echo "Ya se actualizo hoy despues de las 11:00 (run $id): se salta" >&2; echo "correr=false"; exit 0
  fi
done
echo "Chile $hoy $hhmm: primera corrida del dia despues de las 11:00 -> se actualiza" >&2
echo "correr=true"
