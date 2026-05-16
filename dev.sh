#!/usr/bin/env bash
set -euo pipefail
set -m  # enable job control so each background job gets its own process group

ROOT="$(cd "$(dirname "$0")" && pwd)"

# Optional network argument: ./dev.sh [preview|preprod]
NETWORK="${1:-}"
case "$NETWORK" in
  preview)  NODE_SOCKET="$ROOT/testnet/data/preview/node.socket"; NODE_MAGIC=2 ;;
  preprod)  NODE_SOCKET="$ROOT/testnet/data/preprod/node.socket"; NODE_MAGIC=1 ;;
  "")       NODE_SOCKET=""; NODE_MAGIC="" ;;
  *)        echo "Unknown network: $NETWORK (use 'preview' or 'preprod')"; exit 1 ;;
esac

# Ensure we're inside nix develop (provides pg, cabal, node, etc.)
if ! command -v initdb &>/dev/null; then
  echo "Not inside nix develop shell. Entering it now..."
  exec nix develop "$ROOT" --command "$0" "$@"
fi

PGDATA="${PGDATA:-/tmp/hydra-registry-pgdata}"
PGHOST="${PGHOST:-/tmp}"
DB_NAME="hydra_registry"
PIDS=()

cleanup() {
  echo ""
  echo "==> Shutting down..."
  # Kill entire process group of each child (catches piped processes too)
  for pid in "${PIDS[@]}"; do
    kill -- -"$pid" 2>/dev/null || kill "$pid" 2>/dev/null || true
  done
  wait 2>/dev/null || true
  if pg_ctl -D "$PGDATA" status &>/dev/null; then
    echo "==> Stopping PostgreSQL..."
    pg_ctl -D "$PGDATA" stop -m fast
  fi
  echo "==> Done."
}
trap 'cleanup; exit 130' INT
trap cleanup EXIT TERM

# ── Colors ──────────────────────────────────────────────────────────
RED=$'\033[0;31m'
GREEN=$'\033[0;32m'
BLUE=$'\033[0;34m'
YELLOW=$'\033[1;33m'
BOLD=$'\033[1m'
NC=$'\033[0m'

log()  { echo -e "${GREEN}==> $1${NC}"; }
warn() { echo -e "${YELLOW}==> $1${NC}"; }
err()  { echo -e "${RED}==> $1${NC}" >&2; }

# ── Prefixed output helper ──────────────────────────────────────────
run_with_prefix() {
  local prefix="$1"; shift
  local color="$1"; shift
  local padded
  printf -v padded "%-6s" "$prefix"
  "$@" 2>&1 | sed -u "s/^/${color}${BOLD}${padded}${NC} ${color}|${NC} /" &
  # Store both the sed PID and the background job's process group
  PIDS+=($!)
}

# ── PostgreSQL ──────────────────────────────────────────────────────
reset_postgres() {
  warn "Resetting corrupt PostgreSQL data directory at $PGDATA..."
  pg_ctl -D "$PGDATA" stop -m fast 2>/dev/null || true
  rm -rf "$PGDATA"
  log "Initializing fresh PostgreSQL data directory..."
  initdb -D "$PGDATA" --no-locale --encoding=UTF8 -A trust
  pg_ctl -D "$PGDATA" -l "$PGDATA/logfile" \
    -o "--unix_socket_directories='$PGHOST' --listen_addresses=''" \
    start
}

start_postgres() {
  if pg_ctl -D "$PGDATA" status &>/dev/null; then
    log "PostgreSQL already running"
  else
    if [ ! -d "$PGDATA" ]; then
      log "Initializing PostgreSQL data directory at $PGDATA"
      initdb -D "$PGDATA" --no-locale --encoding=UTF8 -A trust
    fi
    log "Starting PostgreSQL..."
    pg_ctl -D "$PGDATA" -l "$PGDATA/logfile" \
      -o "--unix_socket_directories='$PGHOST' --listen_addresses=''" \
      start
  fi

  if ! psql -h "$PGHOST" -lqt 2>/dev/null | grep -qw "$DB_NAME"; then
    log "Creating database $DB_NAME"
    createdb -h "$PGHOST" "$DB_NAME"
  else
    log "Database $DB_NAME exists"
  fi

  # Also create test database so `cabal test` works
  if ! psql -h "$PGHOST" -lqt 2>/dev/null | grep -qw "${DB_NAME}_test"; then
    log "Creating test database ${DB_NAME}_test"
    createdb -h "$PGHOST" "${DB_NAME}_test"
  fi

  # Verify the database is actually healthy by running VACUUM, which opens
  # every relation file. SELECT 1 is a constant and never touches heap files,
  # so it passes even when user table files are missing (stale catalog after
  # a partial /tmp clear). VACUUM fails if any registered relation is missing.
  if ! psql -h "$PGHOST" -d "$DB_NAME" -c "VACUUM" &>/dev/null; then
    warn "Database $DB_NAME is corrupt or inaccessible — resetting..."
    reset_postgres
    createdb -h "$PGHOST" "$DB_NAME"
    createdb -h "$PGHOST" "${DB_NAME}_test" 2>/dev/null || true
    log "Database recreated cleanly."
  fi
}

# ── Backend ─────────────────────────────────────────────────────────
start_backend() {
  log "Building backend..."
  cabal build all 2>&1 | tail -5
  log "Starting backend on :${HYDRA_HTTP_PORT:-8080}"
  local extra_env=""
  if [ -n "$NODE_SOCKET" ]; then
    extra_env="CARDANO_NODE_SOCKET_PATH=$NODE_SOCKET CARDANO_NODE_MAGIC=$NODE_MAGIC"
    log "Cardano node socket: $NODE_SOCKET (magic=$NODE_MAGIC)"
  fi
  run_with_prefix "api" "$BLUE" \
    env HYDRA_DB_CONN_STR="host=$PGHOST port=5432 dbname=$DB_NAME" \
    $extra_env \
    cabal run exe:hydra-registry-api
}

# ── Frontend ────────────────────────────────────────────────────────
start_frontend() {
  if [ ! -d "$ROOT/website/node_modules" ]; then
    log "Installing frontend dependencies..."
    (cd "$ROOT/website" && npm install)
  fi
  log "Starting frontend on :5173"
  run_with_prefix "web" "$YELLOW" \
    sh -c "cd '$ROOT/website' && npm run dev"
}

# ── Main ────────────────────────────────────────────────────────────
main() {
  echo ""
  echo -e "${GREEN}╔══════════════════════════════════════╗${NC}"
  echo -e "${GREEN}║      hydra.registry  dev server      ║${NC}"
  echo -e "${GREEN}╚══════════════════════════════════════╝${NC}"
  echo ""

  start_postgres
  echo ""
  start_backend

  log "Waiting for backend to be ready..."
  local elapsed=0
  until curl -sf "http://localhost:${HYDRA_HTTP_PORT:-8080}/api/v1/stats" &>/dev/null; do
    sleep 1
    elapsed=$((elapsed + 1))
    if [ "$elapsed" -ge 120 ]; then
      err "Backend did not become ready within 120s — check api logs above."
      exit 1
    fi
  done
  log "Backend ready."

  start_frontend

  echo ""
  log "All services running. Press Ctrl+C to stop."
  echo -e "  ${BLUE}API${NC}      → http://localhost:${HYDRA_HTTP_PORT:-8080}"
  echo -e "  ${YELLOW}Website${NC}  → http://localhost:5173"
  if [ -n "$NODE_SOCKET" ]; then
    echo -e "  ${GREEN}Node${NC}     → $NETWORK (socket: $NODE_SOCKET)"
  fi
  echo ""

  wait
}

main "$@"
