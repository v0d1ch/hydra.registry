#!/usr/bin/env bash
set -euo pipefail
set -m

# ── Hydra-nodes runner for Hydra Registry e2e ──
# Assumes ./testnet/run.sh is already running cardano-node on the same network.
# Generates participant keys (Alice, Ida, Bob), fetches protocol parameters,
# then starts four hydra-node processes wired into two heads:
#
#   Head 1: Alice + Ida   (API :14001, :14011)
#   Head 2: Bob   + Ida   (API :14002, :14012)
#
# Ida is the same actor in both heads; that shared participation is what
# the registry uses to discover relay routes between Head 1 and Head 2.
#
# Usage:
#   ./testnet/hydra.sh [preview|preprod]   (default: preview)

ROOT="$(cd "$(dirname "$0")" && pwd)"
NETWORK="${1:-preview}"
DATA_DIR="$ROOT/data/$NETWORK"

# ── Log capture ──
# Tee everything from this script (and its hydra-node children, since their
# stdout already flows through us) to a per-network log file. The previous
# session's log is rotated to .prev so the latest run always lives at
# data/<network>/hydra.log.
LOG_FILE="$DATA_DIR/hydra.log"
mkdir -p "$DATA_DIR"
[ -f "$LOG_FILE" ] && mv "$LOG_FILE" "${LOG_FILE}.prev" 2>/dev/null || true
exec > >(tee "$LOG_FILE") 2>&1
echo "==> hydra.sh started $(date -u +%Y-%m-%dT%H:%M:%SZ) — log: $LOG_FILE"

# ── Check prerequisites ──
for cmd in cardano-cli hydra-node; do
  if ! command -v "$cmd" &>/dev/null; then
    echo "ERROR: $cmd not found. Run from inside 'nix develop'."
    exit 1
  fi
done

# ── Colors ──
RED=$'\033[0;31m'
GREEN=$'\033[0;32m'
YELLOW=$'\033[1;33m'
CYAN=$'\033[0;36m'
BOLD=$'\033[1m'
NC=$'\033[0m'

log()  { echo -e "${GREEN}==> $1${NC}"; }
warn() { echo -e "${YELLOW}==> $1${NC}"; }

PIDS=()

# Ports for close+fanout cleanup; populated by the launch section below.
# We send Close/Fanout from a node whose --cardano-signing-key has fuel:
#   Head 1: ida-h1 port (carol.sk → fuel)
#   Head 2: bob   port (bob.sk   → fuel)
HEAD1_CLOSE_PORT=14011
HEAD2_CLOSE_PORT=14002

# Send Close + Fanout to a single head, polling the WS Greetings for the
# expected status transitions. Skips if the head is Idle / already
# Final / unreachable. Set HYDRA_SKIP_CLOSE=1 to bypass entirely.
close_and_fanout_head() {
  local label="$1"
  local close_port="$2"
  local status
  status=$(timeout 3 websocat -1 "ws://localhost:$close_port" </dev/null 2>/dev/null | head -1 | jq -r '.headStatus // "Unknown"' 2>/dev/null)

  case "$status" in
    Open|Initial|Initializing)
      log "Closing $label (status=$status, sending {tag:Close})..."
      printf '%s\n' '{"tag":"Close"}' | timeout 5 websocat -1 "ws://localhost:$close_port" >/dev/null 2>&1 || true
      ;;
    Closed|Contesting|FanoutPossible)
      log "$label already in close phase ($status), continuing to fanout..."
      ;;
    Final|Finalized)
      log "$label already $status, skipping"
      return 0
      ;;
    Unknown)
      log "$label unreachable ($status), skipping"
      return 0
      ;;
    *)
      log "$label in $status, skipping graceful close"
      return 0
      ;;
  esac

  # Wait for FanoutPossible (or directly Final).
  local elapsed=0
  while [ "$elapsed" -lt 240 ]; do
    local s
    s=$(timeout 2 websocat -1 "ws://localhost:$close_port" </dev/null 2>/dev/null | head -1 | jq -r '.headStatus // "Unknown"' 2>/dev/null)
    case "$s" in
      FanoutPossible)
        log "$label ready for fanout after ${elapsed}s, sending {tag:Fanout}..."
        printf '%s\n' '{"tag":"Fanout"}' | timeout 5 websocat -1 "ws://localhost:$close_port" >/dev/null 2>&1 || true
        break
        ;;
      Final|Finalized)
        log "$label finalized at $s"
        return 0
        ;;
    esac
    sleep 5
    elapsed=$((elapsed + 5))
  done

  # Wait for Final.
  elapsed=0
  while [ "$elapsed" -lt 120 ]; do
    local s
    s=$(timeout 2 websocat -1 "ws://localhost:$close_port" </dev/null 2>/dev/null | head -1 | jq -r '.headStatus // "Unknown"' 2>/dev/null)
    if [ "$s" = "Final" ] || [ "$s" = "Finalized" ]; then
      log "$label finalized after ${elapsed}s wait"
      return 0
    fi
    sleep 3
    elapsed=$((elapsed + 3))
  done
  warn "$label did not finalize within 120s of fanout"
}

close_and_fanout() {
  if [ "${HYDRA_SKIP_CLOSE:-0}" = "1" ]; then
    log "HYDRA_SKIP_CLOSE=1 set, skipping graceful close"
    return 0
  fi
  log "Closing heads gracefully (waits up to ~contestation-period × 2 per head)..."
  close_and_fanout_head "Head 1" "$HEAD1_CLOSE_PORT"
  close_and_fanout_head "Head 2" "$HEAD2_CLOSE_PORT"
}

CLEANUP_RAN=0
hard_kill() {
  log "Hard kill: skipping graceful close, killing hydra-nodes now."
  for pid in "${PIDS[@]}"; do
    kill -- -"$pid" 2>/dev/null || kill "$pid" 2>/dev/null || true
  done
  pkill -f -- "$DATA_DIR/.*hydra-state.*/bin/etcd" 2>/dev/null || true
  wait 2>/dev/null || true
  exit 130
}
cleanup() {
  if [ "$CLEANUP_RAN" -eq 1 ]; then return 0; fi
  CLEANUP_RAN=1
  echo ""
  # A second ctrl-c during cleanup short-circuits the (slow) graceful
  # close-and-fanout and hard-kills the nodes immediately.
  trap hard_kill INT
  log "Closing heads gracefully — press Ctrl-C again to skip and hard-kill."
  # Hydra-nodes must still be alive for Close/Fanout to reach them.
  close_and_fanout
  log "Shutting down hydra-nodes..."
  for pid in "${PIDS[@]}"; do
    kill -- -"$pid" 2>/dev/null || kill "$pid" 2>/dev/null || true
  done
  # Sweep up embedded etcd subprocesses spawned from our data dir
  pkill -f -- "$DATA_DIR/.*hydra-state.*/bin/etcd" 2>/dev/null || true
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

# Preflight: kill any leftover hydra-nodes/etcds from a previous session
# that would clash with the ports we're about to bind. Aggressive kills
# (pkill -9) leave orphan etcd children, and kernel TIME_WAIT can hold
# listen ports for several seconds after the parent dies.
preflight_cleanup() {
  local killed=0
  if pgrep -f -- "--persistence-dir $DATA_DIR" >/dev/null 2>&1; then
    log "Preflight: killing leftover hydra-nodes from previous session..."
    pkill -TERM -f -- "--persistence-dir $DATA_DIR" 2>/dev/null || true
    killed=1
  fi
  if pgrep -f -- "$DATA_DIR/.*hydra-state.*/bin/etcd" >/dev/null 2>&1; then
    log "Preflight: killing orphan etcds from previous session..."
    pkill -TERM -f -- "$DATA_DIR/.*hydra-state.*/bin/etcd" 2>/dev/null || true
    killed=1
  fi
  if [ "$killed" -eq 1 ]; then
    sleep 2
    pkill -KILL -f -- "--persistence-dir $DATA_DIR" 2>/dev/null || true
    pkill -KILL -f -- "$DATA_DIR/.*hydra-state.*/bin/etcd" 2>/dev/null || true
    local waited=0
    while [ "$waited" -lt 30 ]; do
      if ! ss -tln 2>/dev/null | grep -qE ":(14001|14002|14011|14012|15001|15002|15011|15012|2379|2380|2389|2390)\b"; then
        log "Preflight: ports clear after ${waited}s"
        return 0
      fi
      sleep 1
      waited=$((waited + 1))
    done
    warn "Preflight: ports still busy after 30s — startup may fail"
  fi
}

# ── Network configuration ──
case "$NETWORK" in
  preview) MAGIC=2 ;;
  preprod) MAGIC=1 ;;
  *)
    echo "Unknown network: $NETWORK (use 'preview' or 'preprod')"
    exit 1
    ;;
esac

NODE_SOCKET="$DATA_DIR/node.socket"
if [ ! -S "$NODE_SOCKET" ]; then
  echo "ERROR: cardano-node socket not found at $NODE_SOCKET"
  echo "  Start it first with: ./testnet/run.sh $NETWORK"
  exit 1
fi
export CARDANO_NODE_SOCKET_PATH="$NODE_SOCKET"

# ── Participant directories ──
ALICE_DIR="$DATA_DIR/alice"
IDA_DIR="$DATA_DIR/ida"
BOB_DIR="$DATA_DIR/bob"
mkdir -p "$ALICE_DIR" "$IDA_DIR" "$BOB_DIR"
mkdir -p "$IDA_DIR/hydra-state-head1" "$IDA_DIR/hydra-state-head2"

# ── Source pre-funded actor keys from hydra-cluster ──
# Hydra-cluster ships three actors (alice, bob, carol) each with a pair
# of cardano keys: <actor>.sk for protocol identity (used as
# --cardano-signing-key) and <actor>-funds.sk for the wallet that holds
# UTxOs we deposit into the head.
HYDRA_CLUSTER_CREDS="${HYDRA_CLUSTER_CREDS:-/home/v0d1ch/code/hydra/hydra-cluster/config/credentials}"
if [ ! -d "$HYDRA_CLUSTER_CREDS" ]; then
  echo "ERROR: hydra-cluster credentials dir not found at $HYDRA_CLUSTER_CREDS"
  echo "  Override with HYDRA_CLUSTER_CREDS=/path/to/credentials"
  exit 1
fi

# Map our role → hydra-cluster actor:
#   Alice (Head 1)            → alice
#   Ida   (in both heads)     → carol
#   Bob   (Head 2)            → bob
provision_actor() {
  local dir="$1"
  local actor="$2"
  local name="$3"
  for kind in "" "-funds"; do
    local src_sk="$HYDRA_CLUSTER_CREDS/${actor}${kind}.sk"
    local src_vk="$HYDRA_CLUSTER_CREDS/${actor}${kind}.vk"
    local dst_sk="$dir/cardano${kind}.sk"
    local dst_vk="$dir/cardano${kind}.vk"
    local dst_addr="$dir/cardano${kind}.addr"
    if [ ! -f "$dst_sk" ]; then
      log "Provisioning $name ${actor}${kind} keys..."
      cp "$src_sk" "$dst_sk"
      cp "$src_vk" "$dst_vk"
      cardano-cli address build \
        --payment-verification-key-file "$dst_vk" \
        --testnet-magic "$MAGIC" \
        --out-file "$dst_addr"
    fi
  done
  if [ ! -f "$dir/hydra.vk" ]; then
    log "Generating hydra (L2) keys for $name..."
    hydra-node gen-hydra-key --output-file "$dir/hydra"
  fi
}

provision_actor "$ALICE_DIR" alice "Alice"
provision_actor "$IDA_DIR"   carol "Ida"
provision_actor "$BOB_DIR"   bob   "Bob"

# ── Display addresses ──
echo ""
echo -e "${CYAN}═══ Participant Addresses ($NETWORK) ═══${NC}"
echo -e "  ${GREEN}Alice${NC}      : $(cat "$ALICE_DIR/cardano.addr")"
echo -e "  ${GREEN}Alice-funds${NC}: $(cat "$ALICE_DIR/cardano-funds.addr")"
echo -e "  ${GREEN}Ida${NC}        : $(cat "$IDA_DIR/cardano.addr")"
echo -e "  ${GREEN}Ida-funds${NC}  : $(cat "$IDA_DIR/cardano-funds.addr")"
echo -e "  ${GREEN}Bob${NC}        : $(cat "$BOB_DIR/cardano.addr")"
echo -e "  ${GREEN}Bob-funds${NC}  : $(cat "$BOB_DIR/cardano-funds.addr")"
echo ""

# ── Fetch protocol parameters (needed by hydra-node) ──
PROTOCOL_PARAMS="$DATA_DIR/protocol-parameters.json"
if [ ! -f "$PROTOCOL_PARAMS" ]; then
  log "Fetching Conway-era protocol parameters (node must be synced)..."
  for _ in $(seq 1 120); do
    if cardano-cli conway query protocol-parameters --testnet-magic "$MAGIC" --out-file "$PROTOCOL_PARAMS" 2>/dev/null; then
      log "Protocol parameters saved."
      break
    fi
    sleep 5
  done
  if [ ! -f "$PROTOCOL_PARAMS" ]; then
    warn "Could not fetch protocol parameters after 10 minutes."
    warn "Confirm cardano-node is fully synced:"
    warn "  cardano-cli query tip --testnet-magic $MAGIC"
    exit 1
  fi
fi

# ── Hydra-node ports ──
# Bumped out of the 4xxx/5xxx range to avoid colliding with hydra-cluster
# tests in other repos (which default to API :4001, peer :5001, etc).
ALICE_HYDRA_PORT=15001
ALICE_API_PORT=14001
IDA_H1_HYDRA_PORT=15011
IDA_H1_API_PORT=14011

BOB_HYDRA_PORT=15002
BOB_API_PORT=14002
IDA_H2_HYDRA_PORT=15012
IDA_H2_API_PORT=14012

preflight_cleanup

# Wipe persistence so every run starts with fresh head IDs (no stale etcd
# clusters, no stale event logs from previous heads). The hydra-cluster keys
# at the top level are kept; only the per-head state subdirs are removed.
wipe_persistence() {
  for d in \
    "$ALICE_DIR/hydra-state" \
    "$BOB_DIR/hydra-state" \
    "$IDA_DIR/hydra-state-head1" \
    "$IDA_DIR/hydra-state-head2"; do
    if [ -d "$d" ]; then
      rm -rf "$d"
    fi
  done
  mkdir -p "$IDA_DIR/hydra-state-head1" "$IDA_DIR/hydra-state-head2"
  log "Wiped hydra persistence dirs (fresh head IDs on this run)"
}
wipe_persistence

# Each hydra-node has the same shape — only the keys, ports, and
# persistence dir differ between the four invocations. start_node packs
# that up so the launches below stay readable.
#
#   $1 prefix  $2 colour  $3 node-id
#   $4 api-port  $5 own listen-port  $6 peer listen-port
#   $7 self-dir  $8 peer-dir  $9 persistence-dir
start_node() {
  run_with_prefix "$1" "$2" \
    hydra-node \
      --node-id "$3" \
      --api-host 0.0.0.0 \
      --api-port "$4" \
      --listen "127.0.0.1:$5" \
      --peer "127.0.0.1:$6" \
      --hydra-scripts-tx-id "86288ee01e76589955d4a5cc4d7fe105bec1a3c4d14a1b48fc134dd20add66c4" \
      --hydra-signing-key "$7/hydra.sk" \
      --cardano-signing-key "$7/cardano.sk" \
      --hydra-verification-key "$8/hydra.vk" \
      --cardano-verification-key "$8/cardano.vk" \
      --ledger-protocol-parameters "$PROTOCOL_PARAMS" \
      --deposit-period 300s \
      --contestation-period 300s \
      --testnet-magic "$MAGIC" \
      --node-socket "$NODE_SOCKET" \
      --persistence-dir "$9"
}

# ── Head 1: Alice + Ida ──
log "Starting Head 1: Alice + Ida..."
start_node alice  "$GREEN"  alice  "$ALICE_API_PORT"  "$ALICE_HYDRA_PORT"  "$IDA_H1_HYDRA_PORT"  "$ALICE_DIR" "$IDA_DIR"   "$ALICE_DIR/hydra-state"
sleep 2  # let alice's etcd start before the peer joins
start_node ida-h1 "$YELLOW" ida-h1 "$IDA_H1_API_PORT" "$IDA_H1_HYDRA_PORT" "$ALICE_HYDRA_PORT"   "$IDA_DIR"   "$ALICE_DIR" "$IDA_DIR/hydra-state-head1"

# Let Head 1's etcd cluster fully form before bringing up Head 2
sleep 4

# ── Head 2: Bob + Ida ──
log "Starting Head 2: Bob + Ida..."
start_node bob    "$CYAN"   bob    "$BOB_API_PORT"    "$BOB_HYDRA_PORT"    "$IDA_H2_HYDRA_PORT"  "$BOB_DIR"   "$IDA_DIR"   "$BOB_DIR/hydra-state"
sleep 2
start_node ida-h2 "$RED"    ida-h2 "$IDA_H2_API_PORT" "$IDA_H2_HYDRA_PORT" "$BOB_HYDRA_PORT"     "$IDA_DIR"   "$BOB_DIR"   "$IDA_DIR/hydra-state-head2"

echo ""
echo -e "${GREEN}╔══════════════════════════════════════════════════════╗${NC}"
echo -e "${GREEN}║          Hydra-nodes Running                         ║${NC}"
echo -e "${GREEN}╚══════════════════════════════════════════════════════╝${NC}"
echo ""
echo -e "  ${GREEN}Head 1 (Alice + Ida)${NC}"
echo -e "    Alice API → ws://localhost:$ALICE_API_PORT"
echo -e "    Ida   API → ws://localhost:$IDA_H1_API_PORT"
echo ""
echo -e "  ${CYAN}Head 2 (Bob + Ida)${NC}"
echo -e "    Bob   API → ws://localhost:$BOB_API_PORT"
echo -e "    Ida   API → ws://localhost:$IDA_H2_API_PORT"
echo ""
# ── Auto-init: wait for APIs to come up, then open both heads ──
# Persistence is wiped on every run, so heads always start Idle. Auto-init
# means the user can register / deposit immediately after this script
# starts. Set HYDRA_SKIP_INIT=1 to opt out (e.g. when manually testing
# initial-state behaviour).
if [ "${HYDRA_SKIP_INIT:-0}" = "1" ]; then
  warn "HYDRA_SKIP_INIT=1 set — heads will stay Idle until you run ./testnet/open-heads.sh"
else
  log "Waiting for all 4 hydra-node APIs to come online..."
  for port in $ALICE_API_PORT $IDA_H1_API_PORT $BOB_API_PORT $IDA_H2_API_PORT; do
    for _ in $(seq 1 60); do
      if curl -sf -m 1 "http://localhost:$port/protocol-parameters" >/dev/null 2>&1; then
        break
      fi
      sleep 1
    done
  done
  log "Auto-initializing heads via open-heads.sh --no-deposit..."
  if "$ROOT/open-heads.sh" "$NETWORK" --no-deposit; then
    log "Heads opened. Use these head IDs to register:"
    for p in $IDA_H1_API_PORT $BOB_API_PORT; do
      hid=$(echo '' | timeout 3 websocat -1 "ws://localhost:$p" 2>/dev/null | head -1 | jq -r '.hydraHeadId // "<idle>"' 2>/dev/null)
      echo "    port $p → $hid"
    done
  else
    warn "open-heads.sh exited non-zero — heads may not be Open. Check the log above."
  fi
fi

echo ""
echo -e "  ${YELLOW}Next steps:${NC}"
echo -e "  1. Start the registry: ./dev.sh"
echo -e "  2. Register both heads at http://localhost:5173/register"
echo -e "     (any one port from each head — e.g. $ALICE_API_PORT for Head 1, $BOB_API_PORT for Head 2)"
echo -e "  3. Test the relay flow on http://localhost:5173"
echo ""
echo -e "  Press Ctrl+C to gracefully Close + Fanout heads, then stop hydra-nodes."
echo ""

wait
