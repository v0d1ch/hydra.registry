# hydra.registry

Unified L2 state for Cardano wallets.

hydra.registry is an open-source service that indexes [Hydra Head](https://hydra.family) UTxO state and exposes it through wallet-compatible APIs. It bridges the gap between Hydra's off-chain L2 state and Cardano wallets, so users can see their Hydra funds directly in Lace, Nami, Yoroi, and other wallets — no custom UI needed.

## What it does

- **Connects** to Hydra nodes via WebSocket and indexes every confirmed UTxO snapshot in real-time
- **Serves** a Blockfrost-compatible REST API that wallets like Lace and Nami can query as if it were an L1 provider
- **Supports** a Yoroi-compatible endpoint for wallets using the Emurgo backend format
- **Provides** a web interface for registering Hydra heads and viewing live network stats

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
                    ┌──────────────┐
                    │  Hydra Node  │ ◄── WebSocket
                    └──────┬───────┘
                           │
                    ┌──────▼───────┐
                    │   Indexer    │  Listens for SnapshotConfirmed events
                    └──────┬───────┘
                           │
                    ┌──────▼───────┐
                    │  PostgreSQL  │  Stores heads + UTxOs
                    └──────┬───────┘
                           │
              ┌────────────┼────────────┐
              │            │            │
     ┌────────▼──┐  ┌──────▼──┐  ┌─────▼──────┐
     │ Blockfrost│  │  Yoroi  │  │  Website   │
     │   API     │  │   API   │  │            │
     └───────────┘  └─────────┘  └────────────┘
        Lace/Nami     Yoroi       Register heads
                                  View stats
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
| `GET` | `/api/v1/stats` | Live stats (head count, UTxOs, status breakdown) |
| `POST` | `/api/v1/heads/register` | Register a Hydra head by host and port |
| `GET` | `/api/v1/heads?count=N&page=P` | List registered heads (paginated) |
| `GET` | `/api/v1/heads/{headId}` | Head details (status, UTxO count, timestamps) |
| `GET` | `/api/v1/heads/{headId}/addresses` | Distinct addresses in a head |
| `GET` | `/api/v1/heads/{headId}/addresses/{address}/balance` | Aggregated balance |
| `GET` | `/api/v1/heads/{headId}/addresses/{address}/utxos` | UTxOs in a specific head |
| `DELETE` | `/api/v1/admin/heads/{headId}` | Deregister a head |
| `GET` | `/api/v1/metrics` | Prometheus-format metrics |

## Project structure

```
api/                          Haskell backend (Servant + rel8 + PostgreSQL)
  app/Main.hs                 Entry point
  src/
    Api.hs                    API type, handlers, CORS
    Api/Types.hs              Request/response types
    Api/Validation.hs         Cardano address validation
    Cache.hs                  TTL-based in-memory cache
    Config.hs                 Environment variable configuration
    Db.hs                     Database queries (rel8 + hasql)
    Db/Schema.hs              Table schemas (heads, utxos)
    Hydra/Client.hs           WebSocket client, message parsing
    Indexer.hs                Event processing, head registration
    Logging.hs                Structured JSON logging
    Metrics.hs                Prometheus metrics
    Middleware/RateLimit.hs    IP-based rate limiting
  test/                       Test suite (88 tests)

website/                      React frontend (Vite + TypeScript)
  src/
    pages/Landing.tsx          Landing page with stats
    pages/Register.tsx         Head registration form
    components/                Navbar, Footer, AnimatedCounter
    api/client.ts              Typed API client
    styles/global.css          Dark theme styles
```

## Getting started

See [RUN.md](RUN.md) for detailed setup and development instructions.

Quick version:

```bash
nix develop                    # Enter dev shell (GHC 9.10, Node, PostgreSQL)
cd api && cabal run hydra-registry-api   # Start backend on :8080
cd website && npm run dev      # Start frontend on :5173 (proxies to backend)
```

## License

Apache-2.0
