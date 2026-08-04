# Build the one-way telemetry agent. Hackage dependencies only — no
# Cardano toolchain — so the stock haskell image suffices.
FROM haskell:9.6.7-slim AS build
WORKDIR /src

# Dependency layer: copy just the build plan inputs so code changes
# don't invalidate the compiled-dependencies cache.
COPY hydra-registry-agent.cabal cabal.project ./
RUN cabal update && cabal build --only-dependencies exe:hydra-registry-agent

COPY src src
COPY app app
RUN cabal build exe:hydra-registry-agent \
  && cp "$(cabal list-bin exe:hydra-registry-agent)" /hydra-registry-agent \
  && strip /hydra-registry-agent

FROM debian:bookworm-slim
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
