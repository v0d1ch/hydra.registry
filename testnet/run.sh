#!/usr/bin/env bash
set -euo pipefail
set -m

# ── Cardano-node runner for Hydra Registry e2e ──
# Starts a cardano-node on a public testnet, restored from a Mithril snapshot
# (with ancillary ledger state so replay is skipped).
#
# Hydra-nodes are started by ./testnet/hydra.sh in a separate terminal once
# this node is synced.
#
# Usage:
#   ./testnet/run.sh [preview|preprod]   (default: preview)

ROOT="$(cd "$(dirname "$0")" && pwd)"
NETWORK="${1:-preview}"
DATA_DIR="$ROOT/data/$NETWORK"

# ── Check prerequisites ──
for cmd in cardano-node cardano-cli; do
  if ! command -v "$cmd" &>/dev/null; then
    echo "ERROR: $cmd not found. Run from inside 'nix develop'."
    exit 1
  fi
done

# ── Colors ──
GREEN=$'\033[0;32m'
BLUE=$'\033[0;34m'
YELLOW=$'\033[1;33m'
CYAN=$'\033[0;36m'
BOLD=$'\033[1m'
NC=$'\033[0m'

log()  { echo -e "${GREEN}==> $1${NC}"; }
warn() { echo -e "${YELLOW}==> $1${NC}"; }

PIDS=()
cleanup() {
  echo ""
  log "Shutting down cardano-node..."
  for pid in "${PIDS[@]}"; do
    kill -- -"$pid" 2>/dev/null || kill "$pid" 2>/dev/null || true
  done
  wait 2>/dev/null || true
  log "Done."
}
trap 'cleanup; exit 130' INT
trap cleanup EXIT TERM

run_with_prefix() {
  local prefix="$1"; shift
  local color="$1"; shift
  local padded
  printf -v padded "%-6s" "$prefix"
  "$@" 2>&1 | sed -u "s/^/${color}${BOLD}${padded}${NC} ${color}|${NC} /" &
  PIDS+=($!)
}

# Preflight: kill any leftover cardano-node from a previous session that
# would clash with our database lock or socket path. Hard kills (e.g.
# pkill -9 in dev) leave behind LMDB locks; stale socket files block new
# binds.
preflight_cleanup() {
  if pgrep -f -- "cardano-node run.*--database-path $NODE_DB/db" >/dev/null 2>&1; then
    log "Preflight: killing leftover cardano-node from previous session..."
    pkill -TERM -f -- "cardano-node run.*--database-path $NODE_DB/db" 2>/dev/null || true
    sleep 3
    pkill -KILL -f -- "cardano-node run.*--database-path $NODE_DB/db" 2>/dev/null || true
    # Wait for the socket path (and any LMDB lock) to be released.
    local waited=0
    while [ "$waited" -lt 30 ]; do
      if ! pgrep -f -- "cardano-node run.*--database-path $NODE_DB/db" >/dev/null 2>&1; then
        log "Preflight: cardano-node gone after ${waited}s"
        break
      fi
      sleep 1
      waited=$((waited + 1))
    done
  fi
  # Stale unix socket from a hard kill — cardano-node refuses to bind if it
  # still exists on disk.
  if [ -S "$NODE_SOCKET" ]; then
    log "Preflight: removing stale socket $NODE_SOCKET"
    rm -f "$NODE_SOCKET"
  fi
}

# ── Network configuration ──
case "$NETWORK" in
  preview)
    MAGIC=2
    SHELLEY_GENESIS_URL="https://book.play.dev.cardano.org/environments/preview"
    ;;
  preprod)
    MAGIC=1
    SHELLEY_GENESIS_URL="https://book.play.dev.cardano.org/environments/preprod"
    ;;
  *)
    echo "Unknown network: $NETWORK (use 'preview' or 'preprod')"
    exit 1
    ;;
esac

# ── Download network configs if needed ──
CONFIGS_DIR="$DATA_DIR/configs"
mkdir -p "$CONFIGS_DIR"

download_config() {
  local file="$1"
  if [ ! -f "$CONFIGS_DIR/$file" ]; then
    log "Downloading $file..."
    curl -sL "$SHELLEY_GENESIS_URL/$file" -o "$CONFIGS_DIR/$file"
  fi
}

download_config "config.json"
download_config "topology.json"
download_config "byron-genesis.json"
download_config "shelley-genesis.json"
download_config "alonzo-genesis.json"
download_config "conway-genesis.json"
download_config "checkpoints.json"
download_config "peer-snapshot.json"

# ── Mithril aggregator configuration ──
MITHRIL_KEYS_BASE="https://raw.githubusercontent.com/input-output-hk/mithril/main/mithril-infra/configuration"
case "$NETWORK" in
  preview)
    MITHRIL_AGGREGATOR="https://aggregator.pre-release-preview.api.mithril.network/aggregator"
    MITHRIL_GENESIS_VKEY="$(curl -sf "$MITHRIL_KEYS_BASE/pre-release-preview/genesis.vkey")"
    MITHRIL_ANCILLARY_VKEY="$(curl -sf "$MITHRIL_KEYS_BASE/pre-release-preview/ancillary.vkey")"
    ;;
  preprod)
    MITHRIL_AGGREGATOR="https://aggregator.release-preprod.api.mithril.network/aggregator"
    MITHRIL_GENESIS_VKEY="$(curl -sf "$MITHRIL_KEYS_BASE/release-preprod/genesis.vkey")"
    MITHRIL_ANCILLARY_VKEY="$(curl -sf "$MITHRIL_KEYS_BASE/release-preprod/ancillary.vkey")"
    ;;
esac

# ── Mithril client ──
MITHRIL_FLAKE="github:input-output-hk/mithril/2603.1"
MITHRIL_CLIENT="nix run $MITHRIL_FLAKE#mithril-client-cli --"

# ── Create directories ──
NODE_DB="$DATA_DIR/node-db"
mkdir -p "$NODE_DB"

# ── Bootstrap from Mithril snapshot if node-db is empty ──
if [ ! -d "$NODE_DB/db/immutable" ] || [ -z "$(ls -A "$NODE_DB/db/immutable" 2>/dev/null)" ]; then
  log "Bootstrapping cardano-node from Mithril snapshot with ancillary ledger state (this may take a few minutes)..."
  export AGGREGATOR_ENDPOINT="$MITHRIL_AGGREGATOR"
  export GENESIS_VERIFICATION_KEY="$MITHRIL_GENESIS_VKEY"
  export ANCILLARY_VERIFICATION_KEY="$MITHRIL_ANCILLARY_VKEY"
  $MITHRIL_CLIENT cardano-db download latest \
    --download-dir "$NODE_DB" \
    --include-ancillary \
    --json
  log "Mithril snapshot restored (with ledger state — node will skip replay)."
else
  log "Node DB already exists, skipping Mithril bootstrap."
fi

# ── Start cardano-node ──
NODE_SOCKET="$DATA_DIR/node.socket"
preflight_cleanup
log "Starting cardano-node on $NETWORK (magic=$MAGIC)..."
run_with_prefix "node" "$BLUE" \
  cardano-node run \
    --topology "$CONFIGS_DIR/topology.json" \
    --database-path "$NODE_DB/db" \
    --socket-path "$NODE_SOCKET" \
    --config "$CONFIGS_DIR/config.json"

# Wait for socket
log "Waiting for cardano-node socket..."
for _ in $(seq 1 60); do
  if [ -S "$NODE_SOCKET" ]; then
    break
  fi
  sleep 1
done
if [ ! -S "$NODE_SOCKET" ]; then
  echo "ERROR: cardano-node socket not found after 60s"
  exit 1
fi

echo ""
echo -e "${GREEN}╔══════════════════════════════════════════════════════╗${NC}"
echo -e "${GREEN}║          Cardano-node Running                        ║${NC}"
echo -e "${GREEN}╚══════════════════════════════════════════════════════╝${NC}"
echo ""
echo -e "  ${BLUE}Network${NC}      → $NETWORK (magic=$MAGIC)"
echo -e "  ${BLUE}Socket${NC}       → $NODE_SOCKET"
echo -e "  ${BLUE}DB${NC}           → $NODE_DB/db"
echo ""
echo -e "  ${YELLOW}Check sync progress:${NC}"
echo -e "    CARDANO_NODE_SOCKET_PATH=$NODE_SOCKET cardano-cli query tip --testnet-magic $MAGIC"
echo ""
echo -e "  ${YELLOW}Once synced (in another terminal):${NC}"
echo -e "    ./testnet/hydra.sh $NETWORK"
echo ""
echo -e "  Press Ctrl+C to stop cardano-node."
echo ""

wait
