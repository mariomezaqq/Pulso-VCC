#!/usr/bin/env bash
# Commit + push de los datos que genera un workflow, robusto ante commits
# concurrentes (panel admin, otro workflow).
#   uso: commit-datos.sh "mensaje" archivo1 [archivo2 ...]
# Si el rebase choca en un archivo:
#   - data/fondos_curados.csv: gana la version de GitHub (lo editado en el panel);
#     lo que el bot rellenaba (rent vacias) se reintenta en la proxima corrida.
#   - cualquier otro (series, dividendos): gana lo recien generado por el bot.
# Si tras 3 intentos no logra hacer push, termina con error (el run queda en rojo).
set -uo pipefail
msg="$1"; shift
rama="${GITHUB_REF_NAME:-main}"
git config user.name  "github-actions[bot]"
git config user.email "github-actions[bot]@users.noreply.github.com"
git add "$@"
if git diff --staged --quiet; then echo "Sin cambios en: $*"; exit 0; fi
git commit -q -m "$msg"
for i in 1 2 3; do
  if ! git pull -q --rebase origin "$rama"; then
    for f in $(git diff --name-only --diff-filter=U); do
      # en un rebase, --ours = lo que ya esta en GitHub, --theirs = el commit del bot
      if [ "$f" = "data/fondos_curados.csv" ]; then git checkout --ours -- "$f"; else git checkout --theirs -- "$f"; fi
      git add -- "$f"; echo "Conflicto en $f resuelto"
    done
    GIT_EDITOR=true git rebase --continue || git rebase --skip || { git rebase --abort; echo "rebase fallido"; }
  fi
  if git push -q origin "HEAD:$rama"; then echo "push OK"; exit 0; fi
  echo "push reintento $i"; sleep $((5 * i))
done
echo "::error::No se pudo hacer push de: $*"
exit 1
