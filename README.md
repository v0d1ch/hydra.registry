# hydra.registry

Cross-head payment routing for Cardano Hydra.

hydra.registry is an open-source service that indexes [Hydra Head](https://hydra.family) UTxO state and exposes it through wallet-compatible APIs. It bridges the gap between Hydra's off-chain L2 state and Cardano wallets, so users can see their Hydra funds directly in Lace, Nami, Yoroi, and other wallets — no custom UI needed.

It also provides a **payment relay** that routes cross-head payments via HTLC (Hashed TimeLock Contracts), enabling users to send funds between Hydra heads through bridge operators who earn fees for relaying.

## What it does

- **Connects** to Hydra nodes via WebSocket and indexes every confirmed UTxO snapshot in real-time
- **Discovers** Hydra heads on-chain via a hydra-explorer sidecar that polls and syncs head status automatically
- **Indexes** head participants by address, enabling fast lookups of which heads an address participates in
- **Routes payments** between Hydra heads via HTLC contracts, with Dijkstra pathfinding weighted by bridge operator fees
- **Serves** a Blockfrost-compatible REST API that wallets like Lace and Nami can query as if it were an L1 provider
- **Supports** a Yoroi-compatible endpoint for wallets using the Emurgo backend format
- **Provides** a web interface for exploring heads, registering as a bridge operator, creating invoices, finding routes, and tracking payments in real-time

## Wallet compatibility

| Wallet | Format | Status |
|--------|--------|--------|
| Lace | Blockfrost | Supported |
| Nami | Blockfrost | Supported |
| Yoroi | Yoroi API | Supported |
| Flint | Blockfrost | Likely compatible |
| Eternl | Proprietary | Planned |
| VESPR | Proprietary | Planned |

## Architecture

```
  ┌──────────────┐                  ┌──────────────────┐
  │  Hydra Node  │ ◄── WebSocket    │  hydra-explorer  │ ◄── Polling
  └──────┬───────┘                  └────────┬─────────┘
         │                                   │
  ┌──────▼───────┐                  ┌────────▼─────────┐
  │   Indexer    │                  │ Explorer Sidecar  │
  │  (WS live)  │                  │  (on-chain sync)  │
  └──────┬───────┘                  │  + participants   │
         │                          │  + relay graph    │
         │                          └────────┬─────────┘
         │                                   │
         └──────────────┬────────────────────┘
                 ┌──────▼───────┐
                 │  PostgreSQL  │  heads, utxos, explorer_heads,
                 │              │  head_participants, invoices,
                 │              │  payment_routes, route_hops
                 └──────┬───────┘
                        │
        ┌───────────────┼───────────────┐
        │               │               │
┌───────▼───┐  ┌────────▼────┐  ┌───────▼──────┐
│ Wallet    │  │ Relay       │  │  Website     │
│ APIs      │  │ (HTLC)     │  │              │
└───────────┘  └─────────────┘  └──────────────┘
 Lace/Nami/     Invoices,        Explorer, Register,
 Yoroi          Routes,          Invoices, Routes,
                Payments         Payment Tracker
```

## API endpoints

### Wallet-compatible (root level)

| Method | Path | Description |
|--------|------|-------------|
| `GET` | `/addresses/{address}/utxos` | Blockfrost-compatible UTxO list (Lace, Nami) |
| `POST` | `/api/txs/utxoForAddresses` | Yoroi-compatible UTxO query |

### Registry API (`/api/v1/`)

| Method | Path | Description |
|--------|------|-------------|
| `GET` | `/api/v1/health` | Health check (includes DB connectivity) |
| `GET` | `/api/v1/stats` | Live stats (head count, UTxOs, explorer heads, status breakdown) |
| `POST` | `/api/v1/heads/register` | Register a Hydra head (optional: bridge + fee) |
| `GET` | `/api/v1/heads?count=N&page=P` | List registered heads (paginated) |
| `GET` | `/api/v1/heads/{headId}` | Head details enriched with on-chain explorer data |
| `GET` | `/api/v1/heads/{headId}/addresses` | Distinct addresses in a head |
| `GET` | `/api/v1/heads/{headId}/addresses/{address}/balance` | Aggregated balance |
| `GET` | `/api/v1/heads/{headId}/addresses/{address}/utxos` | UTxOs in a specific head |
| `DELETE` | `/api/v1/admin/heads/{headId}` | Deregister a head |
| `GET` | `/api/v1/metrics` | Prometheus-format metrics |

### Explorer API (`/api/v1/explorer/`)

| Method | Path | Description |
|--------|------|-------------|
| `GET` | `/api/v1/explorer/heads?count=N&page=P&status=S&network=N` | List on-chain heads (filterable) |
| `GET` | `/api/v1/explorer/heads/{headId}` | On-chain head details from hydra-explorer |

### Address Index API (`/api/v1/addresses/`)

| Method | Path | Description |
|--------|------|-------------|
| `GET` | `/api/v1/addresses/{address}/heads` | Heads an address participates in (with committed amounts) |

### Relay API (`/api/v1/relay/`)

| Method | Path | Description |
|--------|------|-------------|
| `POST` | `/api/v1/relay/invoices` | Create a payment invoice (receiver generates secret hash) |
| `GET` | `/api/v1/relay/invoices/{invoiceId}` | Get invoice details |
| `POST` | `/api/v1/relay/routes` | Find routes for an invoice (Dijkstra pathfinding) |
| `POST` | `/api/v1/relay/routes/{routeId}/execute` | Initiate HTLC payment along a route |
| `GET` | `/api/v1/relay/payments/{paymentId}` | Payment status with per-hop HTLC tracking |

## Project structure

```
api/                          Haskell backend (Servant + rel8 + PostgreSQL)
  app/Main.hs                 Entry point
  src/
    Api.hs                    API type, handlers, CORS
    Api/Types.hs              Request/response types (wallet, explorer, relay)
    Api/Validation.hs         Cardano address validation
    Cache.hs                  TTL-based in-memory cache
    Config.hs                 Environment variable configuration
    Db.hs                     Database queries (rel8 + hasql)
    Db/Schema.hs              Table schemas (heads, utxos, explorer_heads,
                               head_participants, invoices, payment_routes,
                               route_hops)
    Explorer/Client.hs        hydra-explorer data types and JSON parsing
    Explorer/Members.hs       Parse participant data from explorer members JSON
    Explorer/Sidecar.hs       Polling service: on-chain discovery, participant
                               sync, relay graph rebuild
    Hydra/Client.hs           WebSocket client, message parsing
    Indexer.hs                Event processing, head registration
    Logging.hs                Structured JSON logging
    Metrics.hs                Prometheus metrics
    Middleware/RateLimit.hs    IP-based rate limiting
    Relay/Graph.hs            Dijkstra pathfinding, relay graph construction
  test/                       Test suite (108 tests)

website/                      React frontend (Vite + TypeScript)
  src/
    context/NetworkContext.tsx  Network selection context (Mainnet/Testnet/All)
    pages/Landing.tsx          Landing page with stats
    pages/Register.tsx         Head registration + bridge operator toggle
    pages/Explorer.tsx         On-chain head explorer with filters
    pages/CreateInvoice.tsx    HTLC invoice creation form
    pages/RouteExplorer.tsx    Find and execute payment routes
    pages/PaymentTracker.tsx   Real-time per-hop HTLC payment tracking
    components/                Navbar (with network selector), Footer,
                               AnimatedCounter, Typewriter,
                               MouseSpotlight, ParticleField
    api/client.ts              Typed API client (all endpoints)
    styles/global.css          Dark cyberpunk theme styles
```

## Getting started

See [RUN.md](RUN.md) for detailed setup and development instructions.

Quick version — start everything with one command:

```bash
./dev.sh
```

This starts PostgreSQL, the Haskell backend, and the Vite frontend. It auto-enters the Nix dev shell if needed.

Or manually:

```bash
nix develop                    # Enter dev shell (GHC 9.10, Node, PostgreSQL)
cd api && cabal run hydra-registry-api   # Start backend on :8080
cd website && npm run dev      # Start frontend on :5173 (proxies to backend)
```

## License

Apache-2.0
