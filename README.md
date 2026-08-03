# hydra.registry

Cross-head payment routing for Cardano Hydra.

**Live instance: [hydra-registry.app](https://hydra-registry.app/)**

hydra.registry is an open-source service that indexes [Hydra Head](https://hydra.family) UTxO state and exposes it through wallet-backend-format APIs. It aims to bridge the gap between Hydra's off-chain L2 state and Cardano wallets, so users can eventually see their Hydra funds directly in the wallets they already use. The wire-format endpoints exist and are tested, but **end-to-end wallet integration is still a TODO**.

It also provides a **payment relay** that routes cross-head payments via HTLC (Hashed TimeLock Contracts), enabling users to send funds between Hydra heads through bridge operators who earn fees for relaying.

## Trust model

The registry is deliberately powerless:

- **It never connects to your hydra-node.** Head state reaches the registry through `hydra-registry-agent`, a small one-way telemetry binary you run next to your node. Every connection is outbound from your machine; your node's (unauthenticated) API is never exposed.
- **The agent physically cannot write to your node.** Its node connection is a read-only WebSocket enforced at the type level, and there is no command channel back — the registry cannot queue, relay, or submit anything.
- **Your keys never leave your machine.** The registry builds unsigned transaction envelopes; you sign offline with your own tooling and submit to your **own** node's `POST /transaction`. There is no submission endpoint on the registry, in any mode.
- **The agent is auditable.** It is a standalone package (`agent/`) with plain Hackage dependencies — no Cardano toolchain required to read or build it. Agents authenticate with per-agent secrets (the registry stores only their hashes); each agent's self-reported binary hash is recorded for fleet visibility.

## What it does

- **Indexes** every confirmed UTxO snapshot in real time from agent-pushed node events
- **Discovers** Hydra heads on-chain via a hydra-explorer sidecar plus a registry-side L1 scan (participants + TVL from head UTxOs)
- **Routes payments** between Hydra heads via HTLC contracts, with Dijkstra pathfinding weighted by bridge operator fees
- **Builds transactions natively** (lock / claim / refund / publish-ref-script) as Conway envelopes for users to sign offline
- **Tracks payments** per hop in real time (HTLC watcher + server-sent events), including preimage broadcast to bridge operators
- **Serves** wallet-backend-compatible endpoints (Blockfrost-style plus a legacy UTxO query) that wallets can query as if it were an L1 provider
- **Provides** a web interface for exploring heads, running the agent, creating invoices, executing routes, and acting on your hops (lock / claim / refund) from a dashboard

## Architecture

```
 operator machine                                registry server
┌──────────────────────────────┐                ┌───────────────────────────────┐
│  hydra-node                  │                │  hydra-registry-api           │
│     ▲ read-only WS           │  events +      │   ├─ Indexer (agent events)   │
│  hydra-registry-agent ───────┼──── pparams ──►│   ├─ Explorer sidecar + L1    │
│                              │   (HTTP out)   │   │  scan + relay graph       │
│  you: sign offline, submit   │                │   ├─ Relay (HTLC, SSE)        │
│  to your OWN node            │                │   └─ Wallet APIs + SPA        │
└──────────────────────────────┘                │            │                  │
                                                │       PostgreSQL              │
        hydra-explorer ◄── chain observers      └───────────────────────────────┘
              ▲ polled by the sidecar

   All connections are outbound from the operator's machine. The registry
   never dials a hydra-node and has no way to submit transactions.
```

## API overview

Full endpoint reference: [ARCHITECTURE.md](ARCHITECTURE.md) and the live [/docs](https://hydra-registry.app/docs) page.

| Group | Examples | Notes |
|---|---|---|
| Wallet compat | `GET /addresses/{addr}/utxos`, `POST /api/txs/utxoForAddresses` | standard wallet-backend wire formats (integration TODO) |
| Heads & explorer | `GET /api/v1/heads`, `GET /api/v1/explorer/heads`, `GET /api/v1/addresses/{addr}/heads` | registered + on-chain discovered heads |
| Agent push | `POST /api/v1/agent/register`, `POST /api/v1/agent/events`, `PUT /api/v1/agent/heads/{id}/protocol-parameters` | one-way: information flows to the registry only |
| Relay | `POST /api/v1/relay/invoices`, `POST /api/v1/relay/routes`, `POST …/routes/{id}/execute`, `GET …/payments/{id}`, SSE `…/payments/{id}/events` | invoice → route → per-hop HTLC tracking |
| Tx building | `POST …/hops/{i}/{lock,claim,refund}-tx-cbor`, `POST /api/v1/heads/{id}/publish-ref-script-tx-cbor` | returns unsigned Conway envelopes; **you sign and submit to your own node** |
| Ownership | `POST /api/v1/heads/{id}/claim-ownership` | prove head access via a deposited UTxO |

## Wallet integration (TODO)

The registry serves standard wallet-backend wire formats — a Blockfrost-style
UTxO query and a legacy `utxoForAddresses` query — and both are covered by
tests. Pointing a real wallet at the registry end-to-end has **not** been done
yet and still takes real effort (custom backend URLs, network/era quirks,
per-wallet testing). Until then, treat wallet support as roadmap, not a
feature.

## Project structure

```
agent/      hydra-registry-agent — standalone one-way telemetry agent
            (Hackage deps only; builds with plain GHC + cabal)
api/        Haskell backend (Servant + rel8 + PostgreSQL); hydra libraries
            pinned to a hydra master rev via source-repository-package
website/    React SPA (Vite + TypeScript) — explorer, setup, dashboard,
            invoices, routes, payment tracker, docs
testnet/    cardano-node + hydra-node harness for end-to-end relay testing
tools/      standalone browser HTLC wallet helpers (dev)
```

Module-level detail lives in [ARCHITECTURE.md](ARCHITECTURE.md).

## Getting started

### Run the agent (head operators)

```bash
export HYDRA_NODE_WS_URL=ws://127.0.0.1:4001
export HYDRA_REGISTRY_URL=https://hydra-registry.app
export HYDRA_AGENT_STATE_FILE=$HOME/.hydra-agent-state.json

nix run github:v0d1ch/hydra.registry#hydra-registry-agent
```

Or download a release binary + sha256 from [GitHub Releases](https://github.com/v0d1ch/hydra.registry/releases), or `cd agent && cabal build` with plain GHC.

### Run the registry (development)

See [RUN.md](RUN.md) for detailed instructions. Quick version:

```bash
./dev.sh
```

This starts PostgreSQL, the Haskell backend, and the Vite frontend, auto-entering the Nix dev shell (GHC 9.6.7, Node, PostgreSQL) if needed. The test suite (281 examples, real Postgres) runs with `cd api && cabal test`.

## License

Apache-2.0
