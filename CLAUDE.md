# Claude Code Standing Rules

These rules apply in every session. They override default behaviour.

## Workflow

**TDD — tests first, always.**
Write the test file with expected behaviour, verify it compiles but fails, then write the implementation to make it pass. Never add tests as an afterthought.

**No git operations.**
Never run `git commit`, `git add`, or any other git write command. The user commits all code themselves. When asked for a commit message, output the message text only.

**No permission prompts.**
Never ask "should I continue?" or pause for confirmation. Execute directly.

## After Every Code Change

**Update the wiki** at `~/Documents/v0d1ch/hydra.registry/` for any change that affects:
- DB columns or tables → `database.md`
- API endpoints → `api.md`
- Modules, threads, or shared state → `architecture.md`
- HTLC datum/redeemer/script → `htlc.md`
- Relay flow, routing, watcher, SSE events → `relay.md`
- Env vars → `config.md`
- Testnet scripts or topology → `testnet.md`

**Update `ARCHITECTURE.md`** in the repo root for cross-cutting structural changes:
new/removed modules, tables, threads, top-level routes, external services, env vars.

## Security

**Never handle signing keys.**
The server and SPA must never see, store, ask for, suggest paths to, or reference the user's Cardano signing keys (`.sk` files). The server's job ends at producing an unsigned CBOR for the user to download and sign locally. Use the placeholder `<your-address>.sk` in any CLI commands shown in the UI — never a user-supplied path, never ask where the key lives.

## Hydra Node Questions

**Check the local source first.**
For any question about hydra-node behaviour, API shapes, event names, or CLI flags, grep `~/code/hydra` before guessing. The local clone is the canonical reference for the running version:
- CLI/config: `hydra-node --help` inside `nix develop`
- API shapes: `hydra-node/src/Hydra/API/HTTPServer.hs`
- Event names and usage patterns: `hydra-cluster/src/Hydra/Cluster/Scenarios.hs`
