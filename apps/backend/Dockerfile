FROM --platform=x86_64 ghcr.io/gleam-lang/gleam:v1.1.0-erlang-alpine AS builder

WORKDIR /build

COPY gleam.toml .
COPY manifest.toml .
COPY src src

RUN apk add ca-certificates
RUN gleam build
RUN gleam export erlang-shipment

FROM --platform=x86_64 ghcr.io/gleam-lang/gleam:v1.1.0-erlang-alpine as runner

WORKDIR /app

RUN apk add ca-certificates inotify-tools
COPY --from=builder /build/build/erlang-shipment .
ENTRYPOINT ["/app/entrypoint.sh"]
CMD ["run"]
