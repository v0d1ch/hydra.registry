#!/usr/bin/env bash
# Deploy hydra.registry: frontend (static files) + backend (server-side
# incremental cabal build + service restart). Observers/explorer are NOT
# touched — see the deployment wiki for those.
#
# Usage:
#   ./deploy.sh            # frontend + backend
#   ./deploy.sh frontend   # SPA only (rsync dist/ to the web root)
#   ./deploy.sh backend    # registry only (rsync repo, build, restart)
#
# Connection settings come from .deploy.env (gitignored — the repo is
# public, the origin address is not):
#   DEPLOY_SSH=user@host            # repo rsync target, build + systemctl
#   DEPLOY_WEB_SSH=user@host        # web-root rsync target (default: DEPLOY_SSH)
#   DEPLOY_WEB_ROOT=/var/www/site   # nginx docroot for the SPA
#   DEPLOY_URL=https://example.com  # public URL for the post-deploy health check
set -euo pipefail
cd "$(dirname "$0")"

MODE="${1:-all}"
case "$MODE" in all|frontend|backend) ;; *) echo "usage: $0 [all|frontend|backend]" >&2; exit 2 ;; esac

[ -f .deploy.env ] && . ./.deploy.env
: "${DEPLOY_SSH:?set DEPLOY_SSH in .deploy.env (e.g. user@server)}"
DEPLOY_WEB_SSH="${DEPLOY_WEB_SSH:-$DEPLOY_SSH}"
DEPLOY_WEB_ROOT="${DEPLOY_WEB_ROOT:-/var/www/hydra-registry}"
DEPLOY_URL="${DEPLOY_URL:-}"

# The frontend build needs node; re-enter through the dev shell if missing.
if [ "$MODE" != "backend" ] && ! command -v npm >/dev/null 2>&1; then
  exec nix develop --command "$0" "$MODE"
fi

log() { printf '\n\033[1;36m▸ %s\033[0m\n' "$*"; }

if [ "$MODE" != "backend" ]; then
  log "Building frontend"
  (cd website && npm run build)
fi

if [ "$MODE" != "frontend" ]; then
  log "Syncing repository to $DEPLOY_SSH"
  rsync -az --exclude=dist-newstyle --exclude=.git --exclude=node_modules \
    --exclude=pgdata --exclude=testnet/data --exclude=.deploy.env \
    ./ "$DEPLOY_SSH:code/hydra.registry/"

  log "Building backend on the server (incremental; first build after a
  hydra-rev bump recompiles the pinned hydra packages once)"
  # Streamed directly — no pipes, so a build failure stops the script here.
  ssh "$DEPLOY_SSH" 'cd ~/code/hydra.registry && nix develop --command bash -c "cd api && cabal build exe:hydra-registry-api"'

  log "Restarting hydra-registry"
  ssh "$DEPLOY_SSH" 'sudo systemctl restart hydra-registry && sleep 3 && systemctl is-active hydra-registry'
fi

if [ "$MODE" != "backend" ]; then
  log "Syncing frontend to $DEPLOY_WEB_SSH:$DEPLOY_WEB_ROOT"
  rsync -az website/dist/ "$DEPLOY_WEB_SSH:$DEPLOY_WEB_ROOT/"
fi

if [ -n "$DEPLOY_URL" ]; then
  log "Health check"
  curl -sf "$DEPLOY_URL/api/v1/health"; echo
  bundle=$(curl -sf "$DEPLOY_URL/" | grep -o 'index-[^"]*\.js' | head -1 || true)
  [ -n "$bundle" ] && echo "serving bundle: $bundle"
fi

log "Done"
