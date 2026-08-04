# Build the one-way telemetry agent. Hackage dependencies only — no
# Cardano toolchain — so the stock haskell image suffices.
FROM haskell:9.6.7-slim AS build
WORKDIR /src

# Dependency layer: copy just the build plan inputs so code changes
# don't invalidate the compiled-dependencies cache. Target `all` (not
# the exe) so the package's own library counts as a target, not a
# dependency — its sources aren't in this layer yet.
COPY hydra-registry-agent.cabal cabal.project ./
RUN cabal update && cabal build --only-dependencies all

COPY src src
COPY app app
RUN cabal build exe:hydra-registry-agent \
  && cp "$(cabal list-bin exe:hydra-registry-agent)" /hydra-registry-agent \
  && strip /hydra-registry-agent

FROM debian:bookworm-slim
LABEL org.opencontainers.image.source="https://github.com/v0d1ch/hydra.registry" \
      org.opencontainers.image.description="One-way telemetry agent for the Hydra Registry — reads a local hydra-node over a read-only WebSocket and pushes events to a registry; cannot write to the node" \
      org.opencontainers.image.licenses="Apache-2.0"
RUN apt-get update \
  && apt-get install -y --no-install-recommends ca-certificates libgmp10 \
  && rm -rf /var/lib/apt/lists/* \
  && useradd --system --uid 1000 --create-home agent \
  && mkdir /data && chown agent:agent /data

COPY --from=build /hydra-registry-agent /usr/local/bin/hydra-registry-agent

# Per-agent credentials land in the state file — mount a volume at /data
# so the agent keeps its identity across container recreations.
ENV HYDRA_AGENT_STATE_FILE=/data/hydra-agent-state.json
VOLUME /data
USER agent

ENTRYPOINT ["hydra-registry-agent"]
