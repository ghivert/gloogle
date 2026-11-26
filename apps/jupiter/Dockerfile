FROM ghcr.io/gleam-lang/gleam:v1.19.2-erlang-alpine AS builder

RUN apk add build-base ca-certificates

# Lets the BEAM JIT run under QEMU, when building linux/amd64 on an ARM host
# (without it, `gleam export` crashes in prim_tty). Builder stage only.
ENV ERL_FLAGS="+JMsingle true"

# Path dependencies resolve as `../../packages/*` from `apps/jupiter`, so keep
# the repository layout inside the image.
COPY packages /build/packages
COPY apps/jupiter/gleam.toml apps/jupiter/manifest.toml /build/apps/jupiter/
COPY apps/jupiter/src /build/apps/jupiter/src

RUN cd /build/apps/jupiter && gleam export erlang-shipment

# The runner keeps the Gleam toolchain: ingestion falls back to
# `gleam docs build` when HexDocs has no package-interface.json.
FROM ghcr.io/gleam-lang/gleam:v1.19.2-erlang-alpine AS runner
LABEL org.opencontainers.image.source=https://github.com/ghivert/gloogle

RUN apk add build-base ca-certificates inotify-tools

WORKDIR /app

COPY --from=builder /build/apps/jupiter/build/erlang-shipment .
ENTRYPOINT ["/app/entrypoint.sh"]
CMD ["run"]
