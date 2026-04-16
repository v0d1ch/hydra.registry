# Hydra Registry API — Design Plan

## Product

A standalone REST API service that indexes Hydra L2 UTxO state so Cardano wallets can query funds per address inside Hydra heads. Wallets connect only to hydra.registry — not to individual Hydra nodes.

## Architecture

- **New standalone Haskell project** (separate from the Obelisk frontend)
- **Servant** for the API layer
- **rel8** for type-safe database access (no Template Haskell)
- **PostgreSQL**, self-hosted
- **Plain binary** deployed on a VPS
- **Single canonical instance** — not designed for federation

## API Surface

### Head Registration

```
POST /heads/register
Body: { "host": "1.2.3.4", "port": 4001 }
Response: 200 { "headId": "...", "status": "connected" }
          400 (unreachable or not a valid Hydra node)
```

Open registration with strict validation:
- Connect via WebSocket
- Parse `Greetings` message
- Verify valid `headId`
- Confirm head status is parseable
- Confirm UTxO snapshot is parseable

Deregistration is admin-only for v1.

### List Registered Heads

```
GET /heads
Response: [{ "headId": "...", "status": "open|closed|unreachable|..." }]
```

### UTxOs for Address in Specific Head (Blockfrost-compatible)

```
GET /heads/{headId}/addresses/{address}/utxos
Response: [{ "tx_hash": "...", "output_index": 0, "amount": [...], ... }]
```

### UTxOs for Address Across All Heads

```
GET /addresses/{address}/utxos
Response: [{ "head_id": "...", "head_status": "open", "utxos": [...] }]
```

## Wallet Interface

- **Blockfrost-compatible response format** — minimizes integration effort for wallet developers
- **All standard Cardano address formats accepted**: bech32 (`addr1...`), hex-encoded, base58 (legacy Byron)
- **Head status included** in responses (`open`, `closed`, `unreachable`) so wallets decide how to present funds

## Hydra Integration

- **Passive observer only** — never a participant, no key management, no transaction submission
- **Persistent WebSocket connections** to each registered Hydra node
- **Indexes full UTxOs**: lovelace, native tokens, datums, reference scripts
- **State from events**: `Greetings` (on connect), `SnapshotConfirmed` (new snapshots), `HeadIsClosed`, `HeadIsFinalized`
- **Reconnection**: exponential backoff up to 5 minutes between retries, head marked `unreachable` while disconnected
- **Data preservation**: last known UTxOs preserved for closed/unreachable heads — never deleted

## Data Model (PostgreSQL)

### heads

| Column     | Type      | Notes                                      |
|------------|-----------|--------------------------------------------|
| head_id    | TEXT PK   | From Hydra Greetings message               |
| host       | TEXT      | Registered endpoint                        |
| port       | INT       | Registered endpoint                        |
| status     | TEXT      | open, closed, finalized, unreachable       |
| created_at | TIMESTAMP | Registration time                          |
| updated_at | TIMESTAMP | Last status change                         |

### utxos

| Column       | Type      | Notes                                    |
|--------------|-----------|------------------------------------------|
| tx_hash      | TEXT      | Transaction hash                         |
| output_index | INT       | Output index within transaction          |
| head_id      | TEXT FK   | Which head this UTxO belongs to          |
| address      | TEXT      | Owner address                            |
| lovelace     | BIGINT    | ADA amount in lovelace                   |
| assets       | JSONB     | Native tokens (policy ID, asset, qty)    |
| datum        | JSONB     | Inline datum or datum hash               |
| script_ref   | JSONB     | Reference script if present              |
| updated_at   | TIMESTAMP | Last snapshot that confirmed this UTxO   |

## Authentication & Rate Limiting

- **No authentication** for wallet-facing GET endpoints (v1)
- **IP-based rate limiting**: 100 requests/minute via WAI middleware
- API keys can be added later if abuse becomes a problem

## Tech Stack

| Layer              | Choice                        |
|--------------------|-------------------------------|
| Language           | Haskell                       |
| Web framework      | Servant + Warp                |
| Database           | PostgreSQL                    |
| DB library         | rel8 (no Template Haskell)    |
| WebSocket client   | websockets                    |
| JSON               | Aeson                         |
| Build              | Cabal + Nix flakes            |
| Deployment         | Plain binary on VPS           |

## Deployment

- Plain Haskell binary on a VPS
- Self-hosted PostgreSQL alongside the service
- Single canonical instance at hydra.registry
