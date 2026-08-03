# Hydra Registry — Architecture

> This document is the source of truth for how the Hydra Registry payment-relay system is put together. Update it whenever significant structure, modules, tables, threads, or routes change. Smaller fixes do not need an entry here; cross-cutting refactors do.

---

## 1. What this system is

The Hydra Registry is a **cross-Hydra-head payment relay** for Cardano. It indexes L2 UTxO state from a set of registered Hydra nodes, exposes that state through a Blockfrost-compatible HTTP API for wallets, and — the headline feature — routes payments across multiple heads via an HTLC (Hash Time-Locked Contract) cascade so funds locked into one head can be claimed in another without ever leaving L2.

The hops are stitched together by **shared participants**: if Alice runs a node in head A and Bob runs a node in head B, and both share a third participant (Ida, the bridge), then a payment from Alice to Bob locks an HTLC in A redeemable by Ida, and Ida re-locks an equivalent HTLC in B redeemable by Bob, both pinned to the same payment hash. Bob reveals the preimage to claim in B, Ida sees the preimage on chain (technically: in the L2 snapshot) and uses it to claim in A.

There are three deployable artifacts:

| Artifact | Path | Purpose |
| --- | --- | --- |
| `hydra-registry-api` | `api/` | Haskell Servant HTTP API + indexer + relay graph + HTLC watcher |
| Website | `website/` | React/Vite single-page UI, served by the API binary as static files |
| Tools | `tools/` | Browser-side HTLC wallet helper for manual lock/claim |

Plus a `testnet/` harness for running cardano-node + multiple hydra-nodes locally.

---

## 2. High-level topology

```mermaid
graph TB
  subgraph "Browser"
    UI[React SPA]
    WALLET[Cardano wallets — integration TODO]
  end

  subgraph "Registry process"
    API[Servant HTTP server]
    INDEXER[Indexer thread]
    SIDECAR[Explorer sidecar thread]
    SWEEP[Expiry sweep thread]
    HTLC[HTLC watcher]
    GRAPH[(Relay graph TVar)]
    SLOT[(latestChainSlot TVar)]
    QUEUE[(HydraEvent TQueue)]
  end

  subgraph "External"
    PG[(PostgreSQL)]
    AGENT1[hydra-registry-agent A]
    AGENT2[hydra-registry-agent B]
    NODE1[hydra-node A WS]
    NODE2[hydra-node B WS]
    EXPLORER[hydra-explorer HTTP]
  end

  UI -->|HTTP /api/v1/*| API
  WALLET -.->|sign tx| UI
  API <-->|Hasql pool| PG
  INDEXER --> QUEUE
  AGENT1 -->|POST /agent/events| API
  AGENT2 -->|POST /agent/events| API
  AGENT1 -->|WS read-only| NODE1
  AGENT2 -->|WS read-only| NODE2
  API --> QUEUE
  INDEXER --> PG
  INDEXER --> SLOT
  INDEXER --> HTLC
  SIDECAR --> EXPLORER
  SIDECAR --> PG
  SIDECAR --> GRAPH
  SWEEP --> PG
  API --> GRAPH
  API --> SLOT
  API --> PG
```

The box labelled *Registry process* is one binary, one OS process. All threads share the same Hasql connection pool, the same `HydraEvent` queue, and the same set of TVars described in §4.4.

---

## 3. Repository layout

```
hydra.registry/
├── agent/                     # hydra-registry-agent — standalone cabal package.
│   │                          # Hackage deps only (no CHaP, no Cardano tree), so it
│   │                          # builds with stock GHC and ships via the flake:
│   │                          #   nix run github:v0d1ch/hydra.registry#hydra-registry-agent
│   ├── src/Agent/
│   │   ├── BinaryHash.hs      # agent's own SHA-256 (X-Agent-Binary-Hash)
│   │   ├── EventPusher.hs     # register + push events/pparams to registry
│   │   └── ReadOnly.hs        # type-enforced read-only node WS conn
│   └── app/Main.hs            # one-way telemetry loop
├── api/                       # Haskell Servant backend
│   ├── app/Main.hs            # process entry point + thread spawn
│   ├── src/
│   │   ├── Api.hs             # Servant route table + handlers
│   │   ├── Api/Types.hs       # request/response records
│   │   ├── Api/Validation.hs  # Cardano address shape checks
│   │   ├── Cache.hs           # generic TTL cache (TVar Map)
│   │   ├── Config.hs          # env-var loading
│   │   ├── Db.hs              # session runner + CRUD
│   │   ├── Db/Schema.hs       # Rel8 table definitions
│   │   ├── Explorer/
│   │   │   ├── Client.hs      # JSON parser for hydra-explorer
│   │   │   ├── Members.hs     # member-list → participant tuples
│   │   │   └── Sidecar.hs     # poll loop + graph rebuild
│   │   ├── Hydra/
│   │   │   ├── Client.hs      # WS listener + HydraEvent ADT
│   │   │   └── Htlc.hs        # CBOR encoding for datum/redeemer
│   │   ├── L1/
│   │   │   └── HeadScan.hs    # participants + TVL from head UTxOs via local nodes
│   │   ├── Tx/
│   │   │   └── Builder.hs     # native cardano-api tx building: lock/claim/refund/publish
│   │   ├── Indexer.hs         # event loop + reconnectAllHeads
│   │   ├── Logging.hs         # structured JSON logging
│   │   ├── Metrics.hs         # Prometheus counters
│   │   ├── Middleware/RateLimit.hs
│   │   └── Relay/
│   │       ├── EventBus.hs    # STM broadcast TChan of RouteEvents
│   │       ├── ExpirySweep.hs # invoice/route expiry
│   │       ├── Graph.hs       # Dijkstra + route → HTLC expansion
│   │       ├── HtlcWatcher.hs # detect lock/claim from snapshots; publishes
│   │       └── Slot.hs        # slot ↔ POSIX-ms per network
│   └── test/                  # hspec test suite
├── website/                   # React/Vite SPA
│   ├── src/
│   │   ├── api/client.ts      # typed wrapper over fetch
│   │   ├── components/
│   │   ├── context/NetworkContext.tsx
│   │   └── pages/
│   ├── public/
│   └── dist/                  # build output, served by the API
├── tools/                     # browser HTLC wallet helper (Vite)
├── testnet/                   # cardano-node + hydra-node harness
│   ├── run.sh                 # start cardano-node (Mithril snapshot)
│   ├── hydra.sh               # launch 4 hydra-nodes / 2 heads
│   ├── open-heads.sh
│   ├── data/                  # per-network state, keys, logs
│   └── scripts/               # always-fails.plutus etc
├── .github/workflows/ci.yml   # backend tests + frontend build
├── htlc.plutus                # PlutusScriptV3 HTLC validator (CBOR)
├── flake.nix                  # GHC, cabal, node, postgres dev shell
├── dev.sh                     # one-shot dev launcher
├── README.md / RUN.md
└── ARCHITECTURE.md            # this file
```

---

## 4. Backend (`api/`)

### 4.1 Stack

- **Language:** Haskell, GHC 9.6.7 (from hydra's `cabalOnly` dev shell; see §8.1)
- **Web:** Servant (`API` type at `Api.hs:80`)
- **DB:** Rel8 over Hasql, PostgreSQL 16
- **Concurrency:** `async` + STM (`TVar`, `TQueue`)
- **WebSockets:** `Network.WebSockets` for hydra-node connections
- **Cardano:** `hydra-cardano-api` / `cardano-api` (Conway era), built from the
  local `~/code/hydra` checkout as cabal source packages against CHaP
- **Logging:** Aeson-encoded JSON to stdout
- **Build:** Cabal, no Stack; `api/cabal.project` mirrors hydra's own
  index-states, `allow-newer` and plutus constraints so the shared
  `~/.cabal/store` is reused between the two projects

### 4.2 Module map

```mermaid
graph LR
  Main[app/Main.hs] --> Config
  Main --> Logging
  Main --> Db
  Main --> Indexer
  Main --> Api
  Main --> Sidecar[Explorer.Sidecar]
  Main --> Sweep[Relay.ExpirySweep]
  Main --> Cache
  Main --> Metrics

  Api --> Api.Types
  Api --> Api.Validation
  Api --> Db
  Api --> Hydra.Client
  Api --> Hydra.Htlc
  Api --> Relay.Graph
  Api --> Relay.Slot
  Api --> Indexer

  Indexer --> Db
  Indexer --> Hydra.Client
  Indexer --> Relay.HtlcWatcher

  Db --> Db.Schema

  Sidecar --> Explorer.Client
  Sidecar --> Explorer.Members
  Sidecar --> Db
  Sidecar --> Relay.Graph

  Hydra.Htlc --> Htlc.plutus[htlc.plutus]
```

### 4.3 Startup sequence

The order is deliberate; each step depends on the state set up by the previous one. Reference: `api/app/Main.hs`.

1. **Load config** — `HYDRA_DB_CONN_STR`, ports, `HYDRA_HTLC_SCRIPT_HASH`, `HYDRA_HTLC_SCRIPT_CBOR`, explorer URL/poll interval, `HYDRA_DEFAULT_NETWORK` (network label for registered heads the explorer hasn't indexed yet), static dir.
2. **Open Hasql pool**, run `Db.initDb` (idempotent `CREATE TABLE IF NOT EXISTS` + `ALTER TABLE … ADD COLUMN IF NOT EXISTS`). Schema migrations live entirely in `Db.initDb`.
3. **Allocate shared state:**
   - `eventQueue :: TQueue HydraEvent`
   - `chainSlotVar :: TVar Int64` (initial `0`)
   - `relayGraphVar :: TVar Graph.RelayGraph`
   - rate-limit map, address cache, metrics handles
4. **Spawn indexer thread** (`Indexer.startIndexer`). Reads `eventQueue` forever; on each event updates DB rows for the head, runs `HtlcWatcher.processUtxoSnapshot` if the HTLC script hash is configured, and bumps `chainSlotVar` from any `currentSlot` carried in a Greetings event.
5. **Reconnect to all registered heads** (`Indexer.reconnectAllHeads`). For each row in `heads`, open a WebSocket to `host:port` and push parsed events into `eventQueue`.
6. **Spawn explorer sidecar** (`Explorer.Sidecar.runSidecar`). Polls `hydra-explorer` every N seconds, syncs `explorer_heads` and `head_participants`, then atomically rebuilds the relay graph into `relayGraphVar`. The graph rebuild is independent of the explorer poll — sidecar swallows poll failures so the graph stays current even when explorer is unreachable.
7. **Spawn expiry sweep** (`Relay.ExpirySweep.runSweep`, every 60 s).
8. **Spawn rate-limit cleanup** (every 60 s).
9. **Build `AppEnv`** bundling all the above.
10. **Start Warp** with middleware stack `requestLogger → CORS → rateLimit → metrics`. Graceful shutdown cancels every `async` on `SIGTERM` / `SIGINT`.

### 4.4 Shared state

| State | Type | Producers | Consumers |
| --- | --- | --- | --- |
| `eventQueue` | `TQueue HydraEvent` | per-head WebSocket listeners | indexer |
| `chainSlotVar` | `TVar Int64` (monotonic) | indexer (via `bumpChainSlot`) | `handleFindRoutes`, `handleLockTx`, `handleClaimTx` (validity bounds, timeout derivation) |
| `relayGraphVar` | `TVar Graph.RelayGraph` | sidecar | `handleFindRoutes`, `handleRelayGraph` |
| `relayEventBus` | `EventBus` (`TChan RouteEvent` broadcast) | `HtlcWatcher` (lock/claim/completion), `handleSubmitPreimage` (preimage reveal) | per-route SSE handler at `GET /relay/payments/{r}/events` |
| `pool` | `Hasql.Pool` | n/a | every handler + every background thread |
| `addressCache` | `Cache [UtxoResponse]` | `handleAddressUtxos` (write-through) | same handler |
| `metrics` | `Metrics` (IORef counters) | middleware | `handleMetrics` |

Deliberate constraint: **no handler writes to `chainSlotVar` or `eventQueue`**. Only the indexer / WS listeners do. Handlers read.

### 4.5 Database schema

```mermaid
erDiagram
  heads ||--o{ utxos : "head_id"
  heads ||--o{ head_participants : "head_id"
  heads ||--o{ route_hops : "head_id (logical)"
  invoices ||--o{ payment_routes : "invoice_id"
  payment_routes ||--o{ route_hops : "route_id"
  explorer_heads ||..|| heads : "head_id (no FK)"

  heads {
    text head_id PK
    text host
    int port
    text status
    int snapshot_number
    timestamp created_at
    timestamp updated_at
    timestamp last_message_at
    bool is_bridge
    bigint bridge_fee_lovelace
    text ref_script_utxo
  }
  utxos {
    text tx_hash PK
    int output_index PK
    text head_id FK
    text address
    bigint lovelace
    jsonb assets
    text datum_hash
    jsonb inline_datum
    text reference_script_hash
  }
  head_participants {
    text head_id PK
    text address PK
    text vkey
    text on_chain_id
    bigint committed_lovelace
    text committed_tx_ref
  }
  explorer_heads {
    text head_id PK
    text network
    int network_magic
    text version
    text status
    jsonb members
    text seed_tx_in
    timestamp first_seen_at
    timestamp last_updated_at
  }
  invoices {
    text invoice_id PK
    text receiver_on_chain_id
    text payment_hash
    bigint amount_lovelace
    text memo
    text status
    timestamp expires_at
  }
  payment_routes {
    text route_id PK
    text invoice_id FK
    text sender_address
    text receiver_address
    bigint amount_lovelace
    text status
    jsonb route_path
    bigint total_fee
    text network
  }
  route_hops {
    text hop_id PK
    text route_id FK
    int hop_index
    text head_id
    text bridge_address
    text sender_address
    text receiver_address
    text htlc_status
    text htlc_tx_hash
    text secret_hash
    text preimage
    bigint timeout_slot
    bigint fee_lovelace
    timestamp locked_at
    timestamp claimed_at
  }
```

Schema and column declarations live in `api/src/Db/Schema.hs`; CRUD in `api/src/Db.hs`. `explorer_heads` deliberately has **no FK** to `heads` — the explorer indexes every Hydra head on chain, regardless of whether anyone has registered it with us.

### 4.6 API surface

Servant type in `api/src/Api.hs:80`. Routes group thematically:

| Group | Sample routes | Notes |
| --- | --- | --- |
| Health & metadata | `GET /`, `GET /api/v1/health`, `GET /api/v1/stats`, `GET /api/v1/metrics` | metrics is Prometheus text format |
| Head management | `POST /api/v1/heads/register`, `GET /api/v1/heads/check`, `GET/DELETE /api/v1/heads/{id}`, `POST /api/v1/heads/{id}/ref-script` | `ref-script` registers the head's published HTLC reference UTxO |
| L2 UTxO querying | `GET /api/v1/heads/{id}/addresses/{addr}/{balance,utxos}`, `GET /addresses/{addr}/utxos`, `POST /api/txs/utxoForAddresses` | last two speak standard wallet-backend wire formats (end-to-end wallet integration is TODO) |
| Explorer | `GET /api/v1/explorer/heads`, `…/participants`, `GET /api/v1/explorer/stats`, `GET /api/v1/addresses/{addr}/heads` | reads `explorer_heads` + `head_participants` |
| Relay graph & invoices | `GET /api/v1/relay/graph`, `POST /api/v1/relay/invoices`, `GET …/{id}` | graph response includes nodes + edges for the UI viz |
| Routing & payments | `POST /api/v1/relay/routes`, `POST …/{id}/execute`, `GET /api/v1/relay/payments/{id}`, `POST /api/v1/relay/preimage/{hash}` | preimage broadcast unblocks bridge claims |
| HTLC blueprints | `GET /api/v1/htlc/validator`, `POST /api/v1/relay/payments/{routeId}/hops/{i}/{lock,claim,refund}-tx` | returns CBOR + slot bounds; **caller signs and submits** |
| Server-built tx (Conway envelope) | `POST /api/v1/relay/payments/{r}/hops/{i}/{lock,claim,refund}-tx-cbor`, `POST /api/v1/heads/{id}/publish-ref-script-tx-cbor` | server fetches head's `/protocol-parameters`, picks a wallet UTxO + collateral from the indexed snapshot, builds the tx natively in-process via `hydra-cardano-api` (`Tx.Builder`), returns Conway envelope JSON for the user to sign **offline** with their own keys and submit to their **own** hydra-node's `POST /transaction`; the registry learns the outcome from the agent event stream |
| Agent push | `POST /api/v1/agent/register`, `POST /api/v1/agent/events`, `PUT /api/v1/agent/heads/{id}/protocol-parameters` | one-way telemetry from operator machines; identity is the Bearer per-agent secret alone (registry stores only its SHA-256). The self-reported `X-Agent-Binary-Hash` is recorded as fleet telemetry — never an access-control input, since a hostile binary could report anything |
| Live state | `GET /api/v1/relay/payments/{r}/events` (SSE), `GET /api/v1/relay/participants/{pkh}/routes` | SSE pushes lock/claim/preimage/completion events; participant routes feed shows roles + computed eligible actions per hop |
| Static files | catch-all `Raw` | serves `website/dist/` |

### 4.7 HTLC payment cascade

This is the central flow. Reference handlers in `Api.hs`; on-chain validator in `htlc.plutus`.

```mermaid
sequenceDiagram
  autonumber
  participant Sender
  participant API as Registry API
  participant Bridge as Bridge (shared participant)
  participant Receiver
  participant DB as PostgreSQL
  participant HeadA as Head A WS
  participant HeadB as Head B WS

  Receiver->>API: POST /relay/invoices (paymentHash, amount)
  API->>DB: insert invoices
  Sender->>API: POST /relay/routes (invoiceId, senderOnChainId, network)
  API->>DB: read graph, persist payment_routes + route_hops
  Note right of API: hops = [Sender→Bridge in A, Bridge→Receiver in B]<br/>both bound to same paymentHash, monotone timeouts

  Sender->>API: POST /relay/payments/{r}/hops/0/lock-tx
  API-->>Sender: LockTxBlueprint (datumCbor, validityUpper, minAda)
  Sender->>HeadA: signed lock tx (NewTx)
  HeadA->>API: snapshot with new HTLC UTxO
  API->>DB: HtlcWatcher detects lock, route_hops.htlc_status=locked
  Bridge->>API: POST /relay/payments/{r}/hops/1/lock-tx
  API-->>Bridge: LockTxBlueprint
  Bridge->>HeadB: signed lock tx
  HeadB->>API: snapshot
  API->>DB: hop 1 locked

  Receiver->>API: POST /relay/preimage/{hash} {preimage}
  API->>DB: persist preimage on matching hops
  Receiver->>API: POST /relay/payments/{r}/hops/1/claim-tx
  API-->>Receiver: ClaimTxBlueprint (redeemer = preimage)
  Receiver->>HeadB: signed claim tx
  HeadB->>API: snapshot
  API->>DB: hop 1 claimed
  Bridge->>API: POST /relay/payments/{r}/hops/0/claim-tx
  API-->>Bridge: ClaimTxBlueprint
  Bridge->>HeadA: signed claim tx
  HeadA->>API: snapshot
  API->>DB: hop 0 claimed → route status = settled
```

Important properties:

- **Atomicity**: receiver claims first; revealing the preimage on chain (in the head snapshot) is what lets every upstream bridge claim. If receiver never claims, every locked hop refunds at timeout.
- **Timeouts are monotone-decreasing downstream**: `routeToResponse` anchors the *receiver-side* hop at `chainSlot + secondsRemaining` and steps every upstream hop later by `hopTimeoutMarginSlots` (600 slots ≈ 10 min) via the `hopTimeoutSlot` helper in `Api.hs`. That gives a bridge time to claim its upstream lock after seeing the preimage land downstream, without losing the cascade's safety property.
- **`datum.timeout` is POSIX-ms, not slot**. The Plutus validator compares against `tx.validity_range`, which Plutus exposes as `POSIXTime ms`. `Relay.Slot.slotToPosixMs` does the conversion.
- **Validity upper bounds are clamped to `min(timeoutSlot - safety, chainTip + ERA_SAFE_WINDOW)`** to avoid `TimeTranslationPastHorizon` from the head ledger. See `clampValidityUpper` in `Api.hs`.
- **Min-ada on lock outputs** is dynamic: 7 ADA when the validator is inlined as the output's reference script, ~2 ADA when the head has published a shared reference UTxO via `POST /heads/{id}/ref-script`. The constants live in `Api.hs` (`htlcLockMinAdaInlineLovelace`, `htlcLockMinAdaSharedLovelace`).
- **Blueprints surface fee + collateral guidance**. Lock blueprints carry `recommendedFeeLovelace`; claim/refund blueprints additionally carry `collateralRequiredLovelace` so the caller can size `--tx-in-collateral` + `--tx-out-return-collateral` + `--tx-total-collateral` without learning that the head ledger demands `ceil(fee × collateralPercentage / 100)` from a `TxInvalid` error.

### 4.8 Detection: `Relay.HtlcWatcher`

The watcher is invoked from `Indexer.processEvent` on every Greetings/SnapshotConfirmed event. It diffs the new UTxO set against the previous one and:

- A **new** UTxO at the HTLC script address whose datum's `payment_hash` matches a `route_hops.secret_hash` → mark that hop `locked`, record `htlc_tx_hash`. Address comparison is **bech32-equality against the script address derived from the configured script hash for every supported network** (`htlcScriptAddresses`), not a substring match against hex — bech32 is base32 so the hex hash never appears literally.
- A **disappeared** HTLC UTxO whose tx ref matches a previously-locked hop → mark `claimed`. The watcher doesn't try to extract the preimage from the redeemer; the API receives it via `POST /relay/preimage/{hash}` and propagates.

`Hydra.Client.parseHydraMessage` for `SnapshotConfirmed` merges `snapshot.utxo` ∪ `snapshot.utxoToCommit` so the indexer sees an incremental commit's deposit on the *first* snapshot that incorporates it, instead of waiting for a follow-up snapshot.

### 4.9 External services

| Service | Direction | How |
| --- | --- | --- |
| PostgreSQL | bidirectional | Hasql pool, conn str via `HYDRA_DB_CONN_STR` |
| hydra-node WebSockets | inbound events, outbound `NewTx` | one socket per registered head; URL = `ws://host:port` |
| hydra-explorer | inbound polling | HTTP GET on `HYDRA_EXPLORER_URL/heads`, every `HYDRA_EXPLORER_POLL_INTERVAL` seconds (default 30 s) |
| local cardano-nodes | state queries | `L1.HeadScan` (in the sidecar loop): `queryUTxOByAddress` over every published head-validator address per network (`HYDRA_L1_SOCKET_{PREPROD,PREVIEW,MAINNET}`); yields participants (participation-token names) and TVL (head UTxO lovelace) with zero datum decoding |
| cardano-node / cardano-cli | tx assembly: **none** | tx assembly is native (`hydra-cardano-api`); clients sign/submit themselves |

Chain *time* still flows exclusively through Hydra Greetings (`currentSlot`); the L1 scan reads UTxO state only.

---

## 5. Frontend (`website/`)

### 5.1 Stack

- **Framework:** React 19 + TypeScript
- **Build:** Vite 8
- **Router:** `react-router-dom`
- **Animation:** `framer-motion`
- **Visualisation:** custom SVG/Canvas (no D3, no vis-network)
- **State:** React hooks + one `NetworkContext`; `localStorage` for wizard persistence and selected network. **No Redux/Zustand**.

### 5.2 Pages

| Route | File | Purpose |
| --- | --- | --- |
| `/` | `pages/Landing.tsx` | hero + stats + animated counter |
| `/register` | `pages/Register.tsx` | 2-step head registration wizard |
| `/explorer` | `pages/Explorer.tsx` | browse heads filtered by network/status |
| `/invoice` | `pages/CreateInvoice.tsx` | create payment invoices |
| `/routes` | `pages/RouteExplorer.tsx` | find + display routes for a given invoice |
| `/payments/:paymentId` | `pages/PaymentTracker.tsx` | live status across hops |
| `/balance` | `pages/Balance.tsx` | per-address balance grouped by head |
| `/docs` | `pages/Docs.tsx` | inline docs |

### 5.3 API client

`website/src/api/client.ts` is a thin typed wrapper over `fetch`. Base URL comes from `import.meta.env.VITE_API_BASE_URL ?? ''`, so:

- in dev, Vite proxies `/api` and `/addresses` to `http://localhost:8080` (`vite.config.ts`);
- in prod, the SPA is served by the API binary itself (Servant `Raw` + `serveDirectoryWebApp`), so `''` resolves to the same origin.

### 5.4 Wallet integration

There is **no in-page wallet SDK integration** today, and end-to-end integration with external wallets is an open TODO. Lock/claim/refund blueprints are returned as CBOR + metadata; the user signs out-of-band with `cardano-cli` and submits the signed envelope to their own hydra-node's `POST /transaction` (the UI prints both commands).

### 5.5 How the frontend is served in prod

```mermaid
graph LR
  build[npm run build] --> dist[website/dist/]
  dist --> servant[Servant Raw handler]
  servant --> serveDir[serveDirectoryWebApp staticDir]
  serveDir --> browser[Browser]
```

`AppEnv.staticDir` defaults to `./website/dist` and is configurable via env. The same Warp port serves both the API and the SPA.

---

## 6. Testnet harness (`testnet/`)

Three scripts orchestrate a full local Cardano + Hydra setup:

1. `testnet/run.sh <network>` — boots `cardano-node` for `preview` or `preprod`, restoring from a Mithril snapshot to skip multi-day replay.
2. `testnet/hydra.sh <network>` — generates per-participant keys, launches **four hydra-nodes forming two heads** (Alice+Ida and Bob+Ida; Ida is the shared participant / bridge), tees logs.
3. `testnet/open-heads.sh <network>` — opens both heads and (optionally) commits funds via incremental commit.

State per network lives under `testnet/data/{preview,preprod}/`: protocol params, genesis, per-actor keys, hydra logs.

`testnet/scripts/always-fails.plutus` is the unspendable-address helper used to publish the permanent L1 HTLC reference UTxO.

---

## 7. Tools (`tools/`)

A standalone Vite + Node browser app at `tools/wallet.html` (entry: `tools/main.js`). Imports `@emurgo/cardano-serialization-lib-browser`, hardcodes the HTLC script CBOR, talks to Blockfrost for L1 lookups. Used for manual lock/claim during development; will be replaced by a proper bridge-agent CLI for operators.

---

## 8. Development workflow

```mermaid
graph LR
  user[Developer] --> nix[nix develop]
  nix --> dev[./dev.sh]
  dev --> pg[PostgreSQL on /tmp socket]
  dev --> backend[cabal run :8080]
  dev --> frontend[npm run dev :5173]
  frontend -->|proxy /api| backend
  backend --> pg
```

- `nix develop` provides GHC 9.6.7, cabal, Node, PostgreSQL, websocat, jq, hydra-node, cardano-node.
- `./dev.sh` initialises a local Postgres in the repo, builds and runs the API on `:8080`, runs Vite on `:5173` with proxy.
- For end-to-end relay testing: in separate terminals, `testnet/run.sh preview`, `testnet/hydra.sh preview`, `testnet/open-heads.sh preview`. Then register the heads via `POST /api/v1/heads/register` from the UI.

### 8.1 Dev shell & toolchain

The dev shell (`flake.nix`) extends **hydra's `cabalOnly` shell** (input
`github:cardano-scaling/hydra/master`, rev pinned by `flake.lock`)
instead of assembling its own GHC:
that inherits the haskell.nix GHC 9.6.7 plus every Cardano C library
(libsodium-vrf, libblst, libsecp256k1, librust_accumulator, lmdb,
liburing) needed to compile the hydra packages. Three rules keep it
working:

- **One glibc.** The flake's `nixpkgs` follows `hydra/nixpkgs`, and any
  library that gets **linked into the Haskell build** (libpq!) must come
  from `nixpkgs-2411` (glibc 2.40) — the generation the haskell.nix GHC
  was built against. Mixing generations fails at build time with
  ``version `GLIBC_2.42' not found`` when hsc2hs probe binaries load the
  library.
- **One cabal config.** `api/cabal.project` copies hydra's index-states,
  `allow-newer`, plutus `constraints` and `package *` stanzas verbatim,
  and pulls the hydra libraries via `source-repository-package` at the
  same master rev the flake locks. Keeping these identical lets the
  shared `~/.cabal/store/ghc-9.6.7` serve both projects.
- **Stale pkg-config units.** If a `*-configure` store unit (e.g.
  `postgresql-libpq-configure`) was built against a different shell, its
  baked-in library paths survive environment changes; evict the unit dir
  plus its `package.db/*.conf` and `ghc-pkg recache` to force rebuild.

Shell and source packages are pinned to the **same hydra master rev** —
no local hydra checkout is involved anywhere, by policy. To bump hydra:
`nix flake update hydra`, copy the new rev into the
`source-repository-package` stanza, and re-mirror index-states and
constraints if hydra's own `cabal.project` changed them.

---

## 9. CI

`.github/workflows/ci.yml`, two jobs:

- **test** — `ubuntu-latest`, Postgres 16 service, Nix install, cabal cache, `cabal build all` + `cabal test` against the service DB.
- **frontend** — Node 22, `npm ci`, `tsc -b`, `vite build`.

---

## 10. Conventions and constraints worth knowing

- **Schema migrations:** every column or table change is an idempotent statement appended to `Db.initDb`. There is no Liquibase/Flyway/etc. The DB is small enough that this is fine.
- **No backwards-compat shims:** if a column is removed from the schema, the corresponding code is deleted, not deprecated.
- **No mocked DB in tests:** integration specs hit a real Postgres (`TEST_DB_CONN_STR`).
- **TDD:** new behaviour gets a failing test first, then implementation.
- **Routing identity:** the registry routes by Cardano **key hash** (= 28-byte hash of the participant's `--cardano-signing-key`, also called `OnChainId` in hydra-node), not by bech32 wallet address. Receivers pick where claimed funds land at claim-tx build time.
- **Slot vs POSIX-ms:** anything compared against `tx.validity_range` inside a Plutus script is POSIX-ms; anything compared against `tx_validity_lower/upper_bound` at the Cardano-CLI level is slot. The `route_hops.timeout_slot` column is a slot; `datum.timeout` in the HTLC datum is POSIX-ms. Don't mix them.
- **`localhost` resolves to IPv6 first** on Linux glibc, but `hydra-node` binds IPv4 only. `Hydra.Client.normalizeHost` rewrites `"localhost"` → `"127.0.0.1"` before any WS connect; non-loopback hostnames are passed through. If you need to register a hydra-node by hostname (not IP) and it doesn't have an A-record, document it explicitly — there's no fallback past the rewrite today.

---

## 11. Open / in-flight work

These are tracked outside this doc (in conversation tasks and memory). At a glance:

- **Bridge-operator automation** — auto-relay HTLCs for a third-party bridge operator (planned, not built). Must run operator-side and submit only to the operator's own node: the registry-side agent is deliberately one-way and must stay that way.
- **Smoother manual UX** — eventually folding tools/ into a proper signed-flow inside the SPA.
- **Per-head shared HTLC reference script** — schema + endpoint landed; clients still need to actually publish a UTxO and POST it.

---

*Last full review:* 2026-04-30. Update the date and the relevant sections together when significant structural change lands.

*Recent updates:*
- 2026-08-03 — **Binary-hash allowlist removed; hash is telemetry only**: `HYDRA_AGENT_ALLOWED_HASHES` and both 403 checks (register allowlist + `requireAgent` allowlist/mismatch) are gone. Rationale: the hash is self-reported, so it can never be an access-control input — enforcing it was security theater that also would have locked out `nix run`/source-built agents and (via the mismatch check) locked out every agent after a binary upgrade until it re-registered. Identity now rests on the per-agent secret alone; `requireAgent` records a changed reported hash onto `agent_registrations.binary_hash` (new `Db.updateAgentBinaryHash`), and the `X-Agent-Binary-Hash` header is optional. Server-side only — released agents are unaffected.
- 2026-08-03 — **Hydra dependency pinned to master rev — local checkout retired entirely**: `api/cabal.project` replaces the eight absolute `/home/v0d1ch/code/hydra/*` source paths with a `source-repository-package` at hydra master rev `7b169d66d` (subdirs: prelude, cardano-api, plutus, plutus-extras, tx, node, test-utils; chain-observer dropped from the project — the registry never used it; deployment obtains observers via `nix build github:cardano-scaling/hydra/<rev>#hydra-chain-observer`). `flake.nix` hydra input moved `2.2.0` → `master` (locked to the same rev; GHC stays 9.6.7). Policy per Sasha: `~/code/hydra` is not used for anything, even locally — bump the rev in both files deliberately. The registry now builds on any machine, and the local clone's state (branches, rebases) can no longer break or silently alter builds.
- 2026-08-03 — **Agent split into a standalone package + distribution channels**: `agent/` is its own cabal package with Hackage-only dependencies (websockets, aeson, http-client(-tls), crypton — no CHaP, no Cardano tree, no local hydra checkout), so operators can audit and build it without the registry's toolchain. Distribution: `nix run github:v0d1ch/hydra.registry#hydra-registry-agent` (flake `packages` output via stock `haskellPackages.callCabal2nix`), GitHub Release binaries + sha256 built by `.github/workflows/release-agent.yml` on `agent-v*` tags (the sha256 is the value to pin in `HYDRA_AGENT_ALLOWED_HASHES`), or `cabal build` inside `agent/` with plain GHC. Setup page step 03 rewritten accordingly (was: clone + build inside `api/`, which required the private-path `cabal.project`). Agent modules and the executable stanza left `api/`; `directory` dep dropped from the api library.
- 2026-08-03 — **Agent is strictly one-way; command queue removed**: operators won't hand a third-party binary a write channel to their hydra-node, so the registry can no longer submit transactions to any node, in any mode. Removed: `POST /heads/{id}/submit`, `POST /agent/commands/poll`, `POST /agent/commands/{id}/result`, modules `Agent.CommandPoller`, `Agent.CommandQueue`, `Hydra.Submit`, table `agent_commands` (dropped by migration), `AppEnv.commandWaiters`. The agent keeps two pushes only: node events → `POST /agent/events` and startup protocol-params → `PUT /agent/heads/{id}/protocol-parameters` (`pushProtocolParams` moved into `Agent.EventPusher`). Users submit signed envelopes to their own node's `POST /transaction` (which waits and returns the verdict); the SPA (`Dashboard` tx panel, `Setup` step 4) prints the curl instead of posting to the registry. `HYDRA_DIRECT_WS` now gates only read paths (registration probe, legacy `fetchPP`).
- 2026-07-25 — **`HYDRA_DIRECT_WS` flag** (phase 2 of the security inversion, default **off**): all remaining registry→node connections are gated — `POST /heads/register` and `GET /heads/check` return 403, the submit fallback and `fetchPP` HTTP fallback return 503, and startup `reconnectAllHeads` is skipped. Production relies exclusively on the agent push model; `dev.sh` sets `HYDRA_DIRECT_WS=true` for local testnet workflows. Also dropped the `UNIQUE(host, port)` constraint on `heads` — push-model agents all report `127.0.0.1:4001`, which made the second agent-created head collide.
- 2026-07-25 — **Agent command queue** (security inversion): the registry no longer needs inbound access to user hydra-nodes for the push-model flow. Agents push their node's protocol parameters at startup (`head_protocol_params`, read by `fetchPP` before any legacy HTTP fetch) and poll `POST /agent/commands/poll` (~2s) for queued work; `POST /heads/{id}/submit` enqueues the signed tx for heads with a live agent (seen ≤90s) and waits up to 30s on an in-process `Agent.CommandQueue` TMVar for the agent-reported verdict, falling back to direct WS only for legacy agent-less heads. New modules `Agent.CommandQueue` (server-side rendezvous) and `Agent.CommandPoller` (agent-side executor); new tables `agent_commands`, `head_protocol_params`; shared `requireAgent` auth. Remaining direct-WS paths (`reconnectAllHeads`, registration validation) are legacy and slated for a dev-mode flag.
- 2026-07-25 — **L1 head scan** (`L1.HeadScan`, runs in the sidecar loop): for an Open head, one UTxO at the head validator address carries the headId (state-token policy), the participants (participation-token names = OnChainIds), and the TVL (lovelace) — no datum decoding, so it works across every published hydra version (vendored script-hash list mirrors hydra-chain-observer's registry). Fills `head_participants` and the new `explorer_heads.total_value_lovelace` from local cardano-nodes (`HYDRA_L1_SOCKET_*` env vars). This is the registry-side answer to the chain observer only fully parsing Init txs of its own protocol version. Same date: relay-graph endpoint returns all Open heads as nodes (previously edge-connected only), `GET /` SPA serving moved to nginx with client-route fallback, network selector added to the mobile menu, network selection persisted in localStorage.
- 2026-07-24 — `HYDRA_DEFAULT_NETWORK` env var (default `Preview`): the sidecar's relay-graph rebuild previously hardcoded `"Preview"` as the network for registered heads the explorer hadn't indexed yet, which silently broke route-finding on any other network when the explorer was unavailable. Production deployments set it to the network they serve (e.g. `Preprod`); the explorer-reported network still wins once a head is indexed.
- 2026-07-13 — Toolchain migration + native transaction building:
  - Dev shell now extends hydra 2.2.0's `cabalOnly` shell (GHC 9.6.7, haskell.nix) instead of a self-assembled GHC 9.10 package set; `nixpkgs` follows `hydra/nixpkgs`, postgres comes from `nixpkgs-2411` to keep one glibc across the toolchain (§8.1).
  - `api/cabal.project` gained CHaP + the hydra packages from `~/code/hydra` as source packages, mirroring hydra's own solver configuration for store reuse.
  - `Tx.Builder` rewritten from `cardano-cli conway transaction build-raw` shell-out to **native, pure tx construction via `hydra-cardano-api`** (Conway). Same four builders and `BuildResult` envelope shape; no temp dirs, no subprocess, no cardano-cli runtime dependency. Script-integrity hash now computed from the head's decoded `PParams`. `TxBuilderSpec` asserts on decoded tx bodies (inputs, inline datum round-trip, redeemer exec-units, collateral, validity bounds) instead of argv strings.
- 2026-04-30 — Smooth-UX foundation: SSE event stream for live SPA timelines, server-side tx assembly via cardano-cli shell-out (the SPA never touches signing keys), and a per-pkh dashboard feed with computed action eligibility.
  - `Relay.EventBus` — STM broadcast `TChan` of `HopLocked` / `HopClaimed` / `PreimageRevealed` / `RouteCompleted`. Watcher publishes; SSE handler at `GET /relay/payments/{r}/events` filters per route.
  - `GET /relay/participants/{pkh}/routes` — dashboard feed with `roles` + `actions` computed by `participantActionsFor` (rules: locker can lock when upstream is ready; receiver can claim when preimage is in DB; sender can refund after timeout).
  - `Tx.Builder` — wraps `cardano-cli conway transaction build-raw` for lock/claim/refund/publish-ref-script. Pure argv generation is exposed for unit tests; the IO wrapper writes datum/redeemer/protocol-params to a per-request temp dir, runs cardano-cli, returns Conway envelope JSON.
  - `Hydra.Submit` — one-shot WS connect that forwards signed CBOR as `NewTx` and waits for `TxValid` / `TxInvalid` (15-second timeout, 200-message read budget).
  - New endpoints: `POST /relay/payments/{r}/hops/{i}/{lock,claim,refund}-tx-cbor`, `POST /heads/{id}/publish-ref-script-tx-cbor`, `POST /heads/{id}/submit`.
- 2026-04-30 — Manual e2e flushed out 5 latent bugs; all fixed with regression tests:
  - `HtlcWatcher` now compares bech32 script addresses (was a no-op hex substring on bech32).
  - `parseHydraMessage` merges `snapshot.utxo ∪ snapshot.utxoToCommit` (was missing incremental-commit deposits).
  - Lock/claim/refund blueprints surface `recommendedFeeLovelace` and `collateralRequiredLovelace`.
  - Per-hop `hopTimeoutSlot` makes route timeouts strictly monotone-decreasing downstream.
  - `Hydra.Client.normalizeHost` forces IPv4 for `localhost` to dodge the IPv6-first resolver default.
