#!/usr/bin/env bash
set -euo pipefail

# ── Fund Alice, Ida, and Bob funds addresses ──
# Checks each actor's funds address. If any are below MIN_LOVELACE, it
# redistributes from Alice. If Alice is also low, prints the faucet URL
# and exits so the user can top up manually.
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

# Minimum lovelace each actor must have to deposit into a head.
MIN_LOVELACE=1000000000   # 1000 ADA
# Amount to top up each underfunded actor with.
TOP_UP_LOVELACE=3000000000  # 3000 ADA

# ── Helpers ──
addr_of() { cat "$1/cardano-funds.addr"; }

lovelace_at() {
  local addr="$1"
  cardano-cli conway query utxo --testnet-magic "$MAGIC" \
    --address "$addr" --output-json 2>/dev/null \
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

# ── Check balances ──
ALICE_ADDR="$(addr_of "$ALICE_DIR")"
IDA_ADDR="$(addr_of "$IDA_DIR")"
BOB_ADDR="$(addr_of "$BOB_DIR")"

log "Checking balances on $NETWORK..."
echo -e "  ${CYAN}Alice${NC} : $ALICE_ADDR"
echo -e "  ${CYAN}Ida${NC}   : $IDA_ADDR"
echo -e "  ${CYAN}Bob${NC}   : $BOB_ADDR"
echo ""

ALICE_LV=$(lovelace_at "$ALICE_ADDR")
IDA_LV=$(lovelace_at "$IDA_ADDR")
BOB_LV=$(lovelace_at "$BOB_ADDR")

ada() { echo "$(( $1 / 1000000 )) ADA"; }
log "Alice : $(ada "$ALICE_LV")"
log "Ida   : $(ada "$IDA_LV")"
log "Bob   : $(ada "$BOB_LV")"
echo ""

# Collect actors that need topping up (excluding Alice — she's the source).
NEED_TOPUP=()
[ "$IDA_LV" -lt "$MIN_LOVELACE" ] && NEED_TOPUP+=("$IDA_ADDR")
[ "$BOB_LV" -lt "$MIN_LOVELACE" ] && NEED_TOPUP+=("$BOB_ADDR")

if [ "${#NEED_TOPUP[@]}" -eq 0 ] && [ "$ALICE_LV" -ge "$MIN_LOVELACE" ]; then
  log "All actors have sufficient funds — nothing to do."
  exit 0
fi

# ── Alice needs topping up too ──
if [ "$ALICE_LV" -lt "$MIN_LOVELACE" ]; then
  err "Alice's funds address is below the minimum ($(ada "$ALICE_LV") < $(ada "$MIN_LOVELACE"))."
  echo ""
  echo -e "  Fund Alice from the testnet faucet, then re-run this script:"
  echo -e "  ${YELLOW}https://docs.cardano.org/cardano-testnets/tools/faucet${NC}"
  echo -e "  ${YELLOW}$ALICE_ADDR${NC}"
  exit 1
fi

if [ "${#NEED_TOPUP[@]}" -eq 0 ]; then
  log "Ida and Bob are funded. Alice is low but not critical — nothing to do."
  exit 0
fi

# ── Build top-up transaction ──
REQUIRED=$(( ${#NEED_TOPUP[@]} * TOP_UP_LOVELACE ))
if [ "$ALICE_LV" -lt "$REQUIRED" ]; then
  err "Alice has $(ada "$ALICE_LV") but needs $(ada "$REQUIRED") to top up all actors."
  echo ""
  echo -e "  Fund Alice from the testnet faucet, then re-run this script:"
  echo -e "  ${YELLOW}https://docs.cardano.org/cardano-testnets/tools/faucet${NC}"
  echo -e "  ${YELLOW}$ALICE_ADDR${NC}"
  exit 1
fi

UTXO=$(pick_utxo "$ALICE_ADDR" "$REQUIRED")
if [ -z "$UTXO" ]; then
  err "No single UTxO at Alice's address large enough to cover $(ada "$REQUIRED")."
  warn "Alice's UTxOs may be fragmented. Send a consolidation tx first."
  exit 1
fi

log "Topping up $(${#NEED_TOPUP[@]}) actor(s) with $(ada "$TOP_UP_LOVELACE") each from Alice..."

TX_OUT_ARGS=()
for addr in "${NEED_TOPUP[@]}"; do
  TX_OUT_ARGS+=(--tx-out "${addr}+${TOP_UP_LOVELACE}")
done

cardano-cli conway transaction build \
  --testnet-magic "$MAGIC" \
  --tx-in "$UTXO" \
  "${TX_OUT_ARGS[@]}" \
  --change-address "$ALICE_ADDR" \
  --out-file /tmp/fund-actors.tx

cardano-cli conway transaction sign \
  --testnet-magic "$MAGIC" \
  --tx-file /tmp/fund-actors.tx \
  --signing-key-file "$ALICE_DIR/cardano-funds.sk" \
  --out-file /tmp/fund-actors.signed.tx

cardano-cli conway transaction submit \
  --testnet-magic "$MAGIC" \
  --tx-file /tmp/fund-actors.signed.tx

TXID=$(cardano-cli conway transaction txid --tx-file /tmp/fund-actors.signed.tx)
log "Submitted: $TXID"
log "Waiting for confirmation..."

# Poll until all topped-up actors show the funds.
ELAPSED=0
while [ "$ELAPSED" -lt 120 ]; do
  ALL_FUNDED=1
  for addr in "${NEED_TOPUP[@]}"; do
    lv=$(lovelace_at "$addr")
    if [ "$lv" -lt "$MIN_LOVELACE" ]; then
      ALL_FUNDED=0
      break
    fi
  done
  if [ "$ALL_FUNDED" -eq 1 ]; then
    log "All actors funded."
    break
  fi
  sleep 5
  ELAPSED=$((ELAPSED + 5))
done

if [ "$ELAPSED" -ge 120 ]; then
  warn "Timed out waiting for confirmation — tx may still be in mempool."
fi

echo ""
log "Done. Run ./testnet/open-heads.sh $NETWORK to deposit into heads."
