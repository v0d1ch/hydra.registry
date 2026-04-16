# Running hydra.registry

## Prerequisites

- [Nix](https://nixos.org/download/) with flakes enabled
- PostgreSQL (provided by the Nix dev shell, but you need a running instance)
- Node.js 18+ (provided by Nix dev shell)

## Development setup

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

## Configuration

All configuration is via environment variables with sensible defaults:

| Variable | Default | Description |
|----------|---------|-------------|
| `HYDRA_DB_CONN_STR` | `host=/tmp port=5432 dbname=hydra_registry` | PostgreSQL connection string |
| `HYDRA_HTTP_PORT` | `8080` | HTTP server port |
| `HYDRA_RATE_LIMIT` | `100` | Max requests per IP per minute |
| `HYDRA_HEALTH_TIMEOUT` | `120` | Seconds before a silent head is marked unreachable |
| `HYDRA_STATIC_DIR` | `./website/dist` | Directory for static website files |

Example:

```bash
HYDRA_DB_CONN_STR="host=localhost port=5432 dbname=hydra_registry user=myuser password=secret" \
HYDRA_HTTP_PORT=3000 \
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

The test suite includes 88 tests:
- Hydra message parsing (golden JSON tests)
- JSON roundtrip tests for all API types
- `utxoToResponse` Blockfrost format conversion
- Address validation (unit + QuickCheck property tests)
- Rate limiter logic
- Database CRUD operations (integration)
- Full API endpoint tests via hspec-wai (integration)

## Production deployment

1. Build the frontend: `cd website && npm run build`
2. Build the backend: `cd api && cabal build`
3. Set up PostgreSQL with proper authentication
4. Set environment variables (especially `HYDRA_DB_CONN_STR`)
5. Run behind a reverse proxy (nginx/caddy) with TLS termination
6. The backend serves both the API and the website from a single binary
