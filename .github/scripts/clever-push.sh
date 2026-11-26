#!/usr/bin/env bash
# Deploy the current directory, as is, to a Clever Cloud application.
#
# Clever Cloud deploys by `git push`; this wraps the directory in a throwaway
# repository and force-pushes it, so the app runs exactly what CI built. For
# jupiter, the directory holds a one-line Dockerfile, `FROM <image>@<digest>`:
# the platform only pulls the image built here.
#
# Usage: clever-push.sh <app-name> [org]
#   app-name  as set by infra/clevercloud (e.g. gloogle-jupiter): no ID to copy
#             around, the stack fixes the name.
#   org       organisation ID or name, when the name alone is ambiguous.
#
# Credentials: CLEVER_TOKEN / CLEVER_SECRET in CI; locally, the `clever login`
# session is used when they are unset. GITHUB_SHA names the commit, if set.
set -euo pipefail

app="${1:?usage: clever-push.sh <app-name> [org]}"
org="${2:-}"

if [ -n "${CI:-}" ]; then
  : "${CLEVER_TOKEN:?missing}" "${CLEVER_SECRET:?missing}"
fi

clever() { npx --yes clever-tools@5.5.0 "$@"; }

git init --quiet --initial-branch=master
git add --all
git -c user.name="gloogle-ci" -c user.email="ci@gloogle.run" \
  commit --quiet --message "Deploy ${GITHUB_SHA:-local}"

# Already linked when the same directory is reused (locally): keep the link.
if ! grep -qs "\"alias\": *\"$app\"" .clever.json; then
  if [ -n "$org" ]; then
    clever link "$app" --org "$org"
  else
    clever link "$app"
  fi
fi
# --force: every deploy is an unrelated one-commit history. clever-tools
# follows the deployment and exits non-zero when it fails.
clever deploy --force
