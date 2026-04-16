# Hydra Registry API

REST API for querying Hydra L2 UTxO state across multiple Hydra heads. Provides a Blockfrost-compatible interface for wallets and dApps to discover and query UTxOs held in Hydra heads.

## Prerequisites

- [Nix](https://nixos.org/download/) with flakes enabled
- PostgreSQL (provided by the Nix dev shell, but you need a running instance)

## Quick start

### 1. Enter the development shell

```bash
nix develop
```

This provides GHC 9.10.3, cabal, PostgreSQL tools, HLS, and fourmolu.

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

### 3. Build and run

```bash
cabal build all
cabal run hydra-registry-api
```

The server starts on port 8080 by default. You should see structured JSON log output:

```json
{"timestamp":"...","level":"info","message":"Listening","port":8080}
```

### 4. Register a Hydra head

```bash
curl -X POST http://localhost:8080/heads/register \
  -H "Content-Type: application/json" \
  -d '{"host": "your-hydra-node.example.com", "port": 4001}'
```

The API connects to the Hydra node's WebSocket, validates the Greetings message, stores the head, and begins indexing UTxO snapshots.

## Configuration

All configuration is via environment variables with sensible defaults:

| Variable | Default | Description |
|---|---|---|
| `HYDRA_DB_CONN_STR` | `host=/tmp port=5432 dbname=hydra_registry` | PostgreSQL [libpq connection string](https://www.postgresql.org/docs/current/libpq-connect.html#LIBPQ-CONNSTRING) |
| `HYDRA_HTTP_PORT` | `8080` | HTTP server port |
| `HYDRA_RATE_LIMIT` | `100` | Max requests per IP per minute |
| `HYDRA_HEALTH_TIMEOUT` | `120` | Seconds before a silent head is marked unreachable |

Example:

```bash
HYDRA_DB_CONN_STR="host=localhost port=5432 dbname=hydra_registry user=myuser password=secret" \
HYDRA_HTTP_PORT=3000 \
cabal run hydra-registry-api
```

## API endpoints

| Method | Path | Description |
|---|---|---|
| `GET` | `/` | API version and description |
| `GET` | `/health` | Health check (includes DB connectivity) |
| `POST` | `/heads/register` | Register a Hydra head by host and port |
| `GET` | `/heads?count=N&page=P` | List registered heads (paginated) |
| `GET` | `/heads/{headId}` | Head details (status, UTxO count, timestamps) |
| `GET` | `/heads/{headId}/addresses` | Distinct addresses holding UTxOs in a head |
| `GET` | `/heads/{headId}/addresses/{address}/balance` | Aggregated lovelace + token balance |
| `GET` | `/heads/{headId}/addresses/{address}/utxos` | Blockfrost-compatible UTxO list |
| `GET` | `/addresses/{address}/utxos` | UTxOs for an address across all heads |
| `DELETE` | `/admin/heads/{headId}` | Deregister a head (deletes head and UTxOs) |
| `GET` | `/metrics` | Prometheus-format metrics |

### Address validation

All endpoints that accept an `{address}` parameter validate the format before querying. Accepted formats:
- **Bech32**: `addr1...`, `addr_test1...`, `stake1...`, `stake_test1...`
- **Hex**: 56-130 character hex strings
- **Base58**: Byron-era addresses

Invalid addresses return `400 Bad Request`.

### Pagination

`GET /heads` supports optional query parameters:
- `count` -- results per page (1-100, default 100)
- `page` -- page number (default 1)

## Running tests

### Unit tests only (no database required)

```bash
cabal test --test-option='--skip=DbIntegration' --test-option='--skip=API'
```

### Full test suite (requires PostgreSQL)

Create the test database first:

```bash
createdb -h /tmp hydra_registry_test
```

Then run:

```bash
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

## Project structure

```
api/
  app/Main.hs              -- Entry point: config, middleware, graceful shutdown
  src/
    Api.hs                 -- Servant API type and handlers
    Api/Types.hs           -- Request/response types (JSON)
    Api/Validation.hs      -- Cardano address format validation
    Cache.hs               -- TTL-based in-memory cache
    Config.hs              -- Environment variable configuration
    Db.hs                  -- Database queries (rel8 + hasql)
    Db/Schema.hs           -- Rel8 table schemas (heads, utxos)
    Hydra/Client.hs        -- WebSocket client, message parsing
    Indexer.hs             -- Event processing, head registration
    Logging.hs             -- Structured JSON logging
    Metrics.hs             -- Prometheus metrics collection
    Middleware/RateLimit.hs -- IP-based rate limiting
  test/
    Spec.hs                -- hspec-discover entry point
    TestUtils.hs           -- Shared test helpers and fixtures
    HydraClientSpec.hs     -- parseHydraMessage golden tests
    ApiTypesSpec.hs        -- JSON roundtrip + conversion tests
    ValidationSpec.hs      -- Address validation + property tests
    RateLimitSpec.hs       -- Rate limiter tests
    ConfigSpec.hs          -- Configuration tests
    DbIntegrationSpec.hs   -- Database integration tests
    ApiIntegrationSpec.hs  -- API endpoint integration tests
```

## How it works

1. **Registration**: A user POSTs a Hydra node's host and port. The API connects via WebSocket, reads the `Greetings` message to get the head ID and current UTxO snapshot, stores everything in PostgreSQL, and keeps the WebSocket open.

2. **Indexing**: The indexer thread processes events from all connected heads. `SnapshotConfirmed` events replace the stored UTxOs for that head. `HeadIsClosed` and `HeadIsFinalized` events update the head status. Lost connections trigger automatic reconnection with exponential backoff.

3. **Querying**: Clients query UTxOs by head and/or address. Responses use the Blockfrost UTxO format for wallet compatibility. Frequently queried addresses are cached with a 30-second TTL.

## License

Apache-2.0
