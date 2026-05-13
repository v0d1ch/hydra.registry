#!/usr/bin/env bash
set -euo pipefail

# ── Open the two Hydra heads launched by ./testnet/hydra.sh ──
# In Hydra 2.0.0, sending Init to one participant opens the head immediately
# (no separate Commit / CollectCom phase). After the head is Open, each
# participant can deposit funds via incremental commit (HTTP POST /commit
# → sign → submit on L1).
#
# Prereqs:
#   - cardano-node running (./testnet/run.sh)
#   - hydra-nodes running with funded keys (./testnet/hydra.sh)
#
# Usage:
#   ./testnet/open-heads.sh [preview|preprod] [--no-deposit]

ROOT="$(cd "$(dirname "$0")" && pwd)"
NETWORK="${1:-preview}"
NO_DEPOSIT=0
shift || true
for arg in "$@"; do
  case "$arg" in
    --no-deposit) NO_DEPOSIT=1 ;;
  esac
done

case "$NETWORK" in
  preview) MAGIC=2 ;;
  preprod) MAGIC=1 ;;
  *)
    echo "Unknown network: $NETWORK"
    exit 1
    ;;
esac

DATA_DIR="$ROOT/data/$NETWORK"
NODE_SOCKET="$DATA_DIR/node.socket"
ALICE_DIR="$DATA_DIR/alice"
IDA_DIR="$DATA_DIR/ida"
BOB_DIR="$DATA_DIR/bob"

if [ ! -S "$NODE_SOCKET" ]; then
  echo "ERROR: cardano-node socket not found at $NODE_SOCKET — run ./testnet/run.sh first"
  exit 1
fi
export CARDANO_NODE_SOCKET_PATH="$NODE_SOCKET"

# ── Colors ──
GREEN=$'\033[0;32m'
YELLOW=$'\033[1;33m'
RED=$'\033[0;31m'
CYAN=$'\033[0;36m'
NC=$'\033[0m'
log()  { echo -e "${GREEN}==>${NC} $1"; }
warn() { echo -e "${YELLOW}==>${NC} $1"; }
err()  { echo -e "${RED}==>${NC} $1"; }

# ── Hydra-node ports (must match hydra.sh) ──
ALICE_API_PORT=14001
IDA_H1_API_PORT=14011
BOB_API_PORT=14002
IDA_H2_API_PORT=14012

# ── WebSocket helpers ──
# Read the Greetings message and return its headStatus. Tolerant of timeouts
# / parse failures so the caller (running under `set -euo pipefail`) doesn't
# exit silently if hydra-node hasn't finished serving WS yet.
head_status() {
  local port="$1"
  local raw
  raw=$(timeout 5 websocat -1 "ws://localhost:$port" </dev/null 2>/dev/null | head -1) || raw=""
  if [ -n "$raw" ]; then
    local s
    s=$(printf '%s' "$raw" | jq -r '.headStatus // "Unknown"' 2>/dev/null) || s="Unknown"
    echo "$s"
  else
    echo "Unknown"
  fi
}

# Send a one-shot WS message synchronously.
send_ws() {
  local port="$1"
  local msg="$2"
  echo "$msg" | timeout 10 websocat -1 "ws://localhost:$port" >/dev/null 2>&1 || true
}

# Poll headStatus until it equals the target value, or timeout.
wait_for_status() {
  local port="$1"
  local target="$2"
  local timeout_secs="$3"
  local elapsed=0
  while [ "$elapsed" -lt "$timeout_secs" ]; do
    local s
    s=$(head_status "$port")
    if [ "$s" = "$target" ]; then
      return 0
    fi
    sleep 3
    elapsed=$((elapsed + 3))
  done
  return 1
}

# ── Init a head if not already open ──
init_head() {
  local label="$1"
  local init_port="$2"      # which node sends Init (must have fuel at its cardano.sk)
  local partner_port="$3"   # used to confirm both sides see HeadIsOpen

  local state
  state=$(head_status "$init_port")
  log "$label initiator headStatus = $state"

  case "$state" in
    Open)
      log "$label already Open, skipping Init"
      return 0
      ;;
    Initial|Initializing)
      log "$label already initialized but not Open — waiting for HeadIsOpen"
      ;;
    Idle)
      log "Sending Init to $label (port $init_port)"
      send_ws "$init_port" '{"tag":"Init"}'
      ;;
    *)
      warn "$label in unexpected state ($state) — attempting Init anyway"
      send_ws "$init_port" '{"tag":"Init"}'
      ;;
  esac

  log "Waiting for $label initiator headStatus=Open (poll up to 120s)..."
  if ! wait_for_status "$init_port" "Open" 120; then
    err "$label initiator did not reach Open within 120s (current: $(head_status "$init_port"))"
    return 1
  fi
  log "Waiting for $label partner headStatus=Open (poll up to 60s)..."
  if ! wait_for_status "$partner_port" "Open" 60; then
    warn "$label partner did not see Open within 60s (initiator is Open)"
  fi
  log "$label is Open"
}

# ── Deposit (incremental commit) into an open head ──
# Picks one UTxO at the funds address, POSTs to /commit, signs the returned
# tx with the funds signing key, submits on L1, and waits for the chain
# rollforward (we don't strictly wait for CommitFinalized here).
deposit_one_utxo() {
  local label="$1"
  local api_port="$2"
  local funds_sk="$3"
  local funds_addr_file="$4"
  local min_lovelace_buffer=5000000  # buffer for fees

  if [ ! -f "$funds_sk" ] || [ ! -f "$funds_addr_file" ]; then
    err "Missing funds key/addr for $label ($funds_sk, $funds_addr_file)"
    return 1
  fi

  local funds_addr
  funds_addr=$(cat "$funds_addr_file")

  log "Querying UTxO at $funds_addr for $label..."
  local utxo_file="/tmp/open-heads-utxo-$$-${label//[^A-Za-z0-9]/_}.json"
  cardano-cli query utxo --address "$funds_addr" --testnet-magic "$MAGIC" --output-json --out-file "$utxo_file"

  # Pick a UTxO with no datum/script and > buffer lovelace
  local utxo_ref
  utxo_ref=$(jq -r '
    [to_entries[] |
      select(.value.value.lovelace > '"$min_lovelace_buffer"') |
      select(.value.datumhash == null) |
      select(.value.referenceScript == null) |
      .key] | sort | first // ""' "$utxo_file")

  if [ -z "$utxo_ref" ]; then
    err "No suitable UTxO at $funds_addr — cannot deposit"
    return 1
  fi

  local lovelace
  lovelace=$(jq -r --arg ref "$utxo_ref" '.[$ref].value.lovelace' "$utxo_file")
  log "Using UTxO $utxo_ref ($lovelace lovelace) for $label deposit"

  local payload
  payload=$(jq -n --arg ref "$utxo_ref" --arg addr "$funds_addr" --argjson lov "$lovelace" \
    '{($ref): {address: $addr, value: {lovelace: $lov}}}')

  log "POST /commit on port $api_port..."
  local commit_response
  commit_response=$(curl -sf -X POST "http://localhost:$api_port/commit" \
    -H "Content-Type: application/json" \
    -d "$payload") || {
      err "POST /commit failed for $label"
      return 1
    }

  local cbor_hex
  cbor_hex=$(echo "$commit_response" | jq -r '.cborHex // .txCbor // empty')
  if [ -z "$cbor_hex" ]; then
    err "No cborHex in /commit response for $label: $commit_response"
    return 1
  fi

  # Wrap as Conway tx file for cardano-cli
  local draft_file="/tmp/open-heads-draft-$$-${label//[^A-Za-z0-9]/_}.tx"
  local signed_file="/tmp/open-heads-signed-$$-${label//[^A-Za-z0-9]/_}.tx"
  cat > "$draft_file" <<EOF
{
  "type": "Witnessed Tx ConwayEra",
  "description": "",
  "cborHex": "$cbor_hex"
}
EOF

  log "Signing deposit tx for $label..."
  cardano-cli conway transaction sign \
    --tx-file "$draft_file" \
    --signing-key-file "$funds_sk" \
    --testnet-magic "$MAGIC" \
    --out-file "$signed_file"

  log "Submitting deposit tx for $label..."
  cardano-cli conway transaction submit \
    --tx-file "$signed_file" \
    --testnet-magic "$MAGIC"

  log "$label deposit submitted — hydra-node will observe and finalize"
}

# ── Open both heads ──
echo ""
echo -e "${CYAN}═══ Opening heads ═══${NC}"
echo ""

# Head 1: Ida (carol keys) initiates because she has fuel; alice.sk is empty on $NETWORK
init_head "Head 1" "$IDA_H1_API_PORT" "$ALICE_API_PORT"

# Head 2: Bob initiates (his cardano.sk has fuel)
init_head "Head 2" "$BOB_API_PORT" "$IDA_H2_API_PORT"

if [ "$NO_DEPOSIT" -eq 1 ]; then
  log "--no-deposit set, skipping deposits"
  exit 0
fi

echo ""
echo -e "${CYAN}═══ Depositing into heads ═══${NC}"
echo ""

# Head 1: deposit from both Alice and Ida (whichever has funds) so the head
# has L2 UTxOs at both participant addresses. Each deposit_one_utxo failure
# is non-fatal — empty wallets simply skip.
deposit_one_utxo "Head 1 / Alice" "$ALICE_API_PORT" \
  "$ALICE_DIR/cardano-funds.sk" "$ALICE_DIR/cardano-funds.addr" || true

deposit_one_utxo "Head 1 / Ida" "$IDA_H1_API_PORT" \
  "$IDA_DIR/cardano-funds.sk" "$IDA_DIR/cardano-funds.addr" || true

# Head 2: deposit from Bob
deposit_one_utxo "Head 2 / Bob" "$BOB_API_PORT" \
  "$BOB_DIR/cardano-funds.sk" "$BOB_DIR/cardano-funds.addr" || true

echo ""
log "Done. Check head state:"
echo "  for p in $ALICE_API_PORT $IDA_H1_API_PORT $BOB_API_PORT $IDA_H2_API_PORT; do"
echo "    echo \"port \$p:\$(echo '' | timeout 2 websocat -n1 ws://localhost:\$p | jq -r .headStatus)\""
echo "  done"
