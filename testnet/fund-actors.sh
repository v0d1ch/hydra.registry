#!/usr/bin/env bash
set -euo pipefail

# ── Fund Alice, Ida, and Bob for testnet operations ──
#
# Two kinds of funding are managed:
#
#   1. cardano-funds.addr  — large balance used for L2 deposits into heads.
#      Ida and Bob are topped up from Alice (3000 ADA each). Alice must be
#      seeded from the faucet first if empty.
#
#   2. cardano.addr        — hydra-node key address; needs a small balance
#      (fuel) so the node can pay L1 tx fees when building deposit txs.
#      Each actor self-funds their own cardano.addr from their cardano-funds.addr.
#
# Usage:
#   ./testnet/fund-actors.sh [preview|preprod]

ROOT="$(cd "$(dirname "$0")" && pwd)"
NETWORK="${1:-preview}"

case "$NETWORK" in
  preview) MAGIC=2 ;;
  preprod) MAGIC=1 ;;
  *)
    echo "Unknown network: $NETWORK (use 'preview' or 'preprod')"
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

GREEN=$'\033[0;32m'
YELLOW=$'\033[1;33m'
RED=$'\033[0;31m'
CYAN=$'\033[0;36m'
NC=$'\033[0m'
log()  { echo -e "${GREEN}==>${NC} $1"; }
warn() { echo -e "${YELLOW}==>${NC} $1"; }
err()  { echo -e "${RED}==>${NC} $1"; }

# Thresholds and top-up amounts.
MIN_FUNDS_LOVELACE=1000000000   # 1000 ADA — minimum for L2 deposits
TOP_UP_FUNDS_LOVELACE=3000000000  # 3000 ADA — top-up amount
MIN_FUEL_LOVELACE=50000000      # 50 ADA  — minimum fuel for hydra-node fees
TOP_UP_FUEL_LOVELACE=100000000  # 100 ADA — fuel top-up amount

# ── Helpers ──
lovelace_at() {
  cardano-cli conway query utxo --testnet-magic "$MAGIC" \
    --address "$1" --output-json 2>/dev/null \
    | jq '[.[].value.lovelace // 0] | add // 0'
}

pick_utxo() {
  local addr="$1"
  local min_lv="$2"
  cardano-cli conway query utxo --testnet-magic "$MAGIC" \
    --address "$addr" --output-json 2>/dev/null \
    | jq -r --argjson min "$min_lv" \
      'to_entries | map(select(.value.value.lovelace >= $min)) | first | .key // empty'
}

submit_and_wait() {
  local label="$1"
  local tx_file="$2"
  local signed_file="$3"
  local sk="$4"
  local wait_addr="$5"
  local wait_min="$6"

  cardano-cli conway transaction sign \
    --testnet-magic "$MAGIC" \
    --tx-file "$tx_file" \
    --signing-key-file "$sk" \
    --out-file "$signed_file"

  cardano-cli conway transaction submit \
    --testnet-magic "$MAGIC" \
    --tx-file "$signed_file"

  local txid
  txid=$(cardano-cli conway transaction txid --tx-file "$signed_file")
  log "$label submitted: $txid"

  local elapsed=0
  while [ "$elapsed" -lt 120 ]; do
    local lv
    lv=$(lovelace_at "$wait_addr")
    [ "$lv" -ge "$wait_min" ] && return 0
    sleep 5
    elapsed=$((elapsed + 5))
  done
  warn "$label: timed out waiting for confirmation."
}

ada() { echo "$(( $1 / 1000000 )) ADA"; }

# ── Read addresses ──
ALICE_FUNDS_ADDR=$(cat "$ALICE_DIR/cardano-funds.addr")
IDA_FUNDS_ADDR=$(cat "$IDA_DIR/cardano-funds.addr")
BOB_FUNDS_ADDR=$(cat "$BOB_DIR/cardano-funds.addr")
ALICE_ADDR=$(cat "$ALICE_DIR/cardano.addr")
IDA_ADDR=$(cat "$IDA_DIR/cardano.addr")
BOB_ADDR=$(cat "$BOB_DIR/cardano.addr")

# ────────────────────────────────────────────────────────────────
# Part 1 — L2 deposit funds (cardano-funds.addr)
# ────────────────────────────────────────────────────────────────
echo ""
log "── Part 1: L2 deposit funds (cardano-funds.addr) ──"

ALICE_FUNDS_LV=$(lovelace_at "$ALICE_FUNDS_ADDR")
IDA_FUNDS_LV=$(lovelace_at "$IDA_FUNDS_ADDR")
BOB_FUNDS_LV=$(lovelace_at "$BOB_FUNDS_ADDR")

log "Alice funds : $(ada "$ALICE_FUNDS_LV")  ($ALICE_FUNDS_ADDR)"
log "Ida   funds : $(ada "$IDA_FUNDS_LV")  ($IDA_FUNDS_ADDR)"
log "Bob   funds : $(ada "$BOB_FUNDS_LV")  ($BOB_FUNDS_ADDR)"
echo ""

NEED_TOPUP=()
[ "$IDA_FUNDS_LV" -lt "$MIN_FUNDS_LOVELACE" ] && NEED_TOPUP+=("$IDA_FUNDS_ADDR")
[ "$BOB_FUNDS_LV" -lt "$MIN_FUNDS_LOVELACE" ] && NEED_TOPUP+=("$BOB_FUNDS_ADDR")

if [ "${#NEED_TOPUP[@]}" -gt 0 ]; then
  if [ "$ALICE_FUNDS_LV" -lt "$MIN_FUNDS_LOVELACE" ]; then
    err "Alice's funds address is below the minimum ($(ada "$ALICE_FUNDS_LV"))."
    echo -e "  Fund Alice from the faucet first, then re-run:"
    echo -e "  ${YELLOW}https://docs.cardano.org/cardano-testnets/tools/faucet${NC}"
    echo -e "  ${YELLOW}$ALICE_FUNDS_ADDR${NC}"
    exit 1
  fi

  REQUIRED=$(( ${#NEED_TOPUP[@]} * TOP_UP_FUNDS_LOVELACE ))
  if [ "$ALICE_FUNDS_LV" -lt "$REQUIRED" ]; then
    err "Alice has $(ada "$ALICE_FUNDS_LV") but needs $(ada "$REQUIRED") to top up all actors."
    echo -e "  ${YELLOW}https://docs.cardano.org/cardano-testnets/tools/faucet${NC}"
    echo -e "  ${YELLOW}$ALICE_FUNDS_ADDR${NC}"
    exit 1
  fi

  UTXO=$(pick_utxo "$ALICE_FUNDS_ADDR" "$REQUIRED")
  if [ -z "$UTXO" ]; then
    err "No single UTxO at Alice's funds address large enough to cover $(ada "$REQUIRED")."
    warn "Alice's UTxOs may be fragmented — send a consolidation tx first."
    exit 1
  fi

  log "Topping up ${#NEED_TOPUP[@]} actor(s) with $(ada "$TOP_UP_FUNDS_LOVELACE") each..."
  TX_OUT_ARGS=()
  for addr in "${NEED_TOPUP[@]}"; do
    TX_OUT_ARGS+=(--tx-out "${addr}+${TOP_UP_FUNDS_LOVELACE}")
  done

  cardano-cli conway transaction build \
    --testnet-magic "$MAGIC" \
    --tx-in "$UTXO" \
    "${TX_OUT_ARGS[@]}" \
    --change-address "$ALICE_FUNDS_ADDR" \
    --out-file /tmp/fund-actors-funds.tx

  submit_and_wait "Funds top-up" \
    /tmp/fund-actors-funds.tx /tmp/fund-actors-funds.signed.tx \
    "$ALICE_DIR/cardano-funds.sk" \
    "${NEED_TOPUP[0]}" "$MIN_FUNDS_LOVELACE"
else
  log "All actors have sufficient L2 deposit funds."
fi

# ────────────────────────────────────────────────────────────────
# Part 2 — Hydra-node fuel (cardano.addr)
# Each actor self-funds from their own cardano-funds.addr.
# ────────────────────────────────────────────────────────────────
echo ""
log "── Part 2: Hydra-node fuel (cardano.addr) ──"

ALICE_FUEL_LV=$(lovelace_at "$ALICE_ADDR")
IDA_FUEL_LV=$(lovelace_at "$IDA_ADDR")
BOB_FUEL_LV=$(lovelace_at "$BOB_ADDR")

log "Alice fuel : $(ada "$ALICE_FUEL_LV")  ($ALICE_ADDR)"
log "Ida   fuel : $(ada "$IDA_FUEL_LV")  ($IDA_ADDR)"
log "Bob   fuel : $(ada "$BOB_FUEL_LV")  ($BOB_ADDR)"
echo ""

fuel_actor() {
  local name="$1"
  local actor_dir="$2"
  local fuel_addr="$3"
  local funds_addr="$4"
  local current_lv="$5"

  if [ "$current_lv" -ge "$MIN_FUEL_LOVELACE" ]; then
    log "$name fuel is sufficient — skipping."
    return 0
  fi

  local source_lv
  source_lv=$(lovelace_at "$funds_addr")
  local needed=$(( TOP_UP_FUEL_LOVELACE + 200000 ))  # buffer for fees
  if [ "$source_lv" -lt "$needed" ]; then
    warn "$name: funds address only has $(ada "$source_lv") — cannot self-fuel. Top up funds address first."
    return 0
  fi

  local utxo
  utxo=$(pick_utxo "$funds_addr" "$needed")
  if [ -z "$utxo" ]; then
    warn "$name: no suitable UTxO at funds address to self-fuel."
    return 0
  fi

  log "Fuelling $name: sending $(ada "$TOP_UP_FUEL_LOVELACE") to $fuel_addr..."
  cardano-cli conway transaction build \
    --testnet-magic "$MAGIC" \
    --tx-in "$utxo" \
    --tx-out "${fuel_addr}+${TOP_UP_FUEL_LOVELACE}" \
    --change-address "$funds_addr" \
    --out-file /tmp/fuel-"$name".tx

  submit_and_wait "$name fuel" \
    /tmp/fuel-"$name".tx /tmp/fuel-"$name".signed.tx \
    "$actor_dir/cardano-funds.sk" \
    "$fuel_addr" "$MIN_FUEL_LOVELACE"
}

fuel_actor "Alice" "$ALICE_DIR" "$ALICE_ADDR" "$ALICE_FUNDS_ADDR" "$ALICE_FUEL_LV"
fuel_actor "Ida"   "$IDA_DIR"   "$IDA_ADDR"   "$IDA_FUNDS_ADDR"   "$IDA_FUEL_LV"
fuel_actor "Bob"   "$BOB_DIR"   "$BOB_ADDR"   "$BOB_FUNDS_ADDR"   "$BOB_FUEL_LV"

echo ""
log "Done. Run ./testnet/open-heads.sh $NETWORK to deposit into heads."
