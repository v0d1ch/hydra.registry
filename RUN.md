# Running hydra.registry

## Prerequisites

- [Nix](https://nixos.org/download/) with flakes enabled
- PostgreSQL (provided by the Nix dev shell, but you need a running instance)
- Node.js 18+ (provided by Nix dev shell)

## Quick start

Run the all-in-one dev script from the project root:

```bash
./dev.sh
```

This handles everything: enters the Nix dev shell (if not already in one), starts PostgreSQL, builds and runs the backend on `:8080`, and starts the Vite frontend on `:5173`. Press Ctrl+C to stop all services.

You can override defaults with environment variables:

```bash
PGDATA=/my/pgdata HYDRA_HTTP_PORT=3000 ./dev.sh
```

## Manual setup

### 1. Enter the development shell

```bash
nix develop
```

This provides GHC 9.10.3, cabal, Node.js, PostgreSQL tools, HLS, and fourmolu.

### 2. Start PostgreSQL

If you don't already have a PostgreSQL instance running, start one locally:

```bash
# Initialize a data directory (one-time)
initdb -D /tmp/pgdata

# Start the server on a Unix socket in /tmp
pg_ctl -D /tmp/pgdata -l /tmp/pgdata/logfile -o "--unix_socket_directories='/tmp'" start

# Create the database
createdb -h /tmp hydra_registry
```

The API creates its tables automatically on startup (`CREATE TABLE IF NOT EXISTS`).

### 3. Build and run the backend

```bash
cd api
cabal build all
cabal run hydra-registry-api
```

The server starts on port 8080 by default. You should see structured JSON log output:

```json
{"timestamp":"...","level":"info","message":"Listening","port":8080}
```

### 4. Build and run the frontend

In a separate terminal:

```bash
cd website
npm install        # first time only
npm run dev        # starts Vite dev server on :5173
```

The Vite dev server proxies `/api` requests to the backend at `localhost:8080`.

For production, build the static files and let the backend serve them:

```bash
cd website
npm run build      # outputs to website/dist/
```

The Haskell backend serves `website/dist/` at `/` automatically.

### 5. Register a Hydra head

Via the web UI at `http://localhost:5173/register`, or via curl:

```bash
curl -X POST http://localhost:8080/api/v1/heads/register \
  -H "Content-Type: application/json" \
  -d '{"host": "your-hydra-node.example.com", "port": 4001}'
```

To register as a bridge operator (relay payments for a fee):

```bash
curl -X POST http://localhost:8080/api/v1/heads/register \
  -H "Content-Type: application/json" \
  -d '{"host": "your-hydra-node.example.com", "port": 4001, "bridge": true, "feeLovelace": 500000}'
```

### 6. Create a payment invoice

The receiver creates an invoice with a SHA-256 hash of their secret:

```bash
curl -X POST http://localhost:8080/api/v1/relay/invoices \
  -H "Content-Type: application/json" \
  -d '{
    "receiverAddress": "addr1q...",
    "paymentHash": "a1b2c3...64hex",
    "amountLovelace": 50000000,
    "memo": "Payment for service",
    "expiresInSeconds": 3600
  }'
```

### 7. Find and execute a payment route

The sender finds routes and executes one:

```bash
# Find routes
curl -X POST http://localhost:8080/api/v1/relay/routes \
  -H "Content-Type: application/json" \
  -d '{
    "invoiceId": "inv-abc123",
    "senderAddress": "addr1q...",
    "network": "Mainnet"
  }'

# Execute chosen route
curl -X POST http://localhost:8080/api/v1/relay/routes/route-xyz/execute

# Track payment status
curl http://localhost:8080/api/v1/relay/payments/route-xyz
```

## Configuration

All configuration is via environment variables with sensible defaults:

| Variable | Default | Description |
|----------|---------|-------------|
| `HYDRA_DB_CONN_STR` | `host=/tmp port=5432 dbname=hydra_registry` | PostgreSQL connection string |
| `HYDRA_HTTP_PORT` | `8080` | HTTP server port |
| `HYDRA_RATE_LIMIT` | `100` | Max requests per IP per minute |
| `HYDRA_HEALTH_TIMEOUT` | `120` | Seconds before a silent head is marked unreachable |
| `HYDRA_STATIC_DIR` | `./website/dist` | Directory for static website files |
| `HYDRA_EXPLORER_URL` | `https://explorer.hydra.family` | Base URL for hydra-explorer (on-chain head discovery) |
| `HYDRA_EXPLORER_POLL_INTERVAL` | `120` | Seconds between hydra-explorer polling cycles |
| `HYDRA_HTLC_SCRIPT_HASH` | *(none)* | Script hash for HTLC contract detection in heads (enables relay routing) |

Example:

```bash
HYDRA_DB_CONN_STR="host=localhost port=5432 dbname=hydra_registry user=myuser password=secret" \
HYDRA_HTTP_PORT=3000 \
HYDRA_HTLC_SCRIPT_HASH="abc123..." \
cabal run hydra-registry-api
```

## Running tests

### Unit tests only (no database required)

```bash
cd api
cabal test --test-option='--skip=DbIntegration' --test-option='--skip=API'
```

### Full test suite (requires PostgreSQL)

Create the test database first:

```bash
createdb -h /tmp hydra_registry_test
```

Then run:

```bash
cd api
cabal test
```

To use a different test database:

```bash
TEST_DB_CONN_STR="host=localhost port=5432 dbname=my_test_db" cabal test
```

The test suite includes 108 tests:
- API integration tests — all endpoints including explorer (18 tests)
- JSON roundtrip tests for all API types (17 tests)
- Database CRUD — heads, UTxOs, explorer heads, filtering, aggregation (26 tests)
- Address validation — Bech32, hex, base58, property tests (20 tests)
- Hydra message parsing — Greetings, SnapshotConfirmed, HeadIsClosed, HeadIsFinalized (15 tests)
- Explorer client — JSON parsing of on-chain head entries (4 tests)
- Rate limiter — per-client tracking, cleanup (6 tests)
- Config loading (2 tests)

## Website pages

The frontend (at `http://localhost:5173`) includes:

| Page | Path | Description |
|------|------|-------------|
| Landing | `/` | Overview, live stats, wallet compatibility |
| Explorer | `/explorer` | Browse on-chain heads by status and network |
| Register | `/register` | Register a head, optionally as bridge operator |
| Invoice | `/invoice` | Create HTLC payment invoice (receiver side) |
| Routes | `/routes` | Find and execute payment routes (sender side) |
| Payment Tracker | `/payments/:id` | Real-time per-hop HTLC status tracking |

A network selector in the navbar filters explorer and routing data by Mainnet, Testnet, or All.

## Production deployment

1. Build the frontend: `cd website && npm run build`
2. Build the backend: `cd api && cabal build`
3. Set up PostgreSQL with proper authentication
4. Set environment variables (especially `HYDRA_DB_CONN_STR`, `HYDRA_HTLC_SCRIPT_HASH`)
5. Run behind a reverse proxy (nginx/caddy) with TLS termination
6. The backend serves both the API and the website from a single binary
