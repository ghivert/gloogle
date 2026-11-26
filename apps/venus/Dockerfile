# syntax=docker/dockerfile:1.10
#
# venus, the frontend: a static Vite build. Production is Netlify, which only
# hosts: CI extracts the `site` stage (the content of dist/, public/_headers
# and public/_redirects included) and deploys it with the Netlify CLI. Build
# from the repository root (workspace + path dependencies):
#
#   docker buildx build -f apps/venus/Dockerfile --target site \
#     --output type=local,dest=out .
#
# Sentry sourcemap upload is optional: pass the token as a BuildKit secret
# (never a build arg, which would land in the image history), plus the
# SENTRY_ORG / SENTRY_PROJECT build args.
#
#     --secret id=sentry_auth_token,env=SENTRY_AUTH_TOKEN \
#     --build-arg SENTRY_ORG=… --build-arg SENTRY_PROJECT=…

FROM node:24-alpine AS builder

RUN corepack enable
WORKDIR /build

# Dependencies first, for the layer cache. Every workspace manifest is needed
# for `--immutable` to accept the lockfile.
COPY package.json yarn.lock .yarnrc.yml ./
COPY plugins ./plugins
COPY apps/jupiter/package.json apps/jupiter/
COPY apps/venus/package.json apps/venus/
COPY packages/vitools/package.json packages/vitools/
RUN yarn install --immutable

COPY packages ./packages
COPY apps/venus ./apps/venus

ARG SENTRY_ORG
ARG SENTRY_PROJECT
RUN --mount=type=secret,id=sentry_auth_token,env=SENTRY_AUTH_TOKEN \
    SENTRY_ORG="$SENTRY_ORG" SENTRY_PROJECT="$SENTRY_PROJECT" \
    yarn workspace @jupiter/venus venus:build

FROM scratch AS site
COPY --from=builder /build/apps/venus/dist /
