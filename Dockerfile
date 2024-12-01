FROM --platform=x86_64 ghcr.io/gleam-lang/gleam:v1.6.2-erlang-alpine AS builder

RUN apk add build-base ca-certificates
RUN mkdir -p /build/backend/src

COPY apps/backend/gleam.toml /build/backend
COPY apps/backend/manifest.toml /build/backend
COPY apps/backend/src /build/backend/src
COPY packages /packages

RUN cd /build/backend && gleam export erlang-shipment

FROM --platform=x86_64 ghcr.io/gleam-lang/gleam:v1.6.2-erlang-alpine AS runner
LABEL org.opencontainers.image.source=https://github.com/ghivert/gloogle

RUN apk add build-base ca-certificates inotify-tools

WORKDIR /app

COPY --from=builder /build/backend/build/erlang-shipment .
ENTRYPOINT ["/app/entrypoint.sh"]
CMD ["run"]
