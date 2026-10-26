#!/usr/bin/env bash
# Copy production data into the local stack, in two halves:
#   1. the Postgres database — the local production snapshot by default, or a
#      fresh pg_dump of production (Render) — then `dbmate up` for migrations
#      newer than the dump;
#   2. the Scaleway object-storage bucket (package archives), mirrored into the
#      local RustFS through a copy on disk (mirror_bucket.py).
#
# Nothing here can be regenerated from Hex: hex_read cursors, analytics,
# analytics_timeseries, search_analytics and GitHub popularity history only
# exist in production. The dump is kept on disk (infra/local/dumps, gitignored)
# so a reseed does not hit production twice.
#
# Credentials are read from the environment, never from this file:
#   PROD_DATABASE_URL      optional: production Postgres URL (Render external URL) for a
#                          fresh dump instead of the local snapshot
#   DUMP_FILE              optional: restore this dump instead
#   SCALEWAY_ACCESS_KEY    object-storage key pair
#   SCALEWAY_SECRET_KEY
#   BUCKET_URI             e.g. gloogle.s3.fr-par.scw.cloud (as jupiter reads it)
#
# Usage: infra/local/scripts/seed-local.sh [db|s3|all]   (default: all)
set -euo pipefail

here="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
root="$(cd "$here/../../.." && pwd)"
dumps="$root/infra/local/dumps"
mkdir -p "$dumps"

LOCAL_DATABASE_URL="${LOCAL_DATABASE_URL:-postgres://gloogle:gloogle@localhost:25432/gloogle?sslmode=disable}"
LOCAL_S3_ENDPOINT="${LOCAL_S3_ENDPOINT:-http://localhost:29000}"
LOCAL_S3_ACCESS_KEY="${LOCAL_S3_ACCESS_KEY:-gloogle}"
LOCAL_S3_SECRET_KEY="${LOCAL_S3_SECRET_KEY:-gloogle-local}"
LOCAL_BUCKET="${LOCAL_BUCKET:-gloogle}"

need() { command -v "$1" >/dev/null || { echo "missing tool: $1" >&2; exit 1; }; }
require_env() { [ -n "${!1:-}" ] || { echo "missing env var: $1" >&2; exit 1; }; }

# The database half restores from, in order of preference:
#   DUMP_FILE                 an explicit dump (.sql, .sql.gz, or pg_dump custom format);
#   PROD_DATABASE_URL         a fresh pg_dump of production, cached under infra/local/dumps;
#   apps/jupiter/db/gloogle_migrated.sql.gz
#                             the committed-alongside snapshot of production (gitignored,
#                             2026-07-02, pg_dump 18, migration level 20241016151324).
seed_db() {
  need psql; need dbmate
  local dump="${DUMP_FILE:-}"
  if [ -z "$dump" ] && [ -n "${PROD_DATABASE_URL:-}" ]; then
    need pg_dump
    dump="$dumps/gloogle-$(date +%Y%m%d).dump"
    if [ ! -s "$dump" ]; then
      echo "→ dumping production database to $dump"
      pg_dump --format=custom --no-owner --no-privileges \
        --dbname="$PROD_DATABASE_URL" --file="$dump"
    else
      echo "→ reusing existing dump $dump"
    fi
  fi
  if [ -z "$dump" ]; then
    dump="$root/apps/jupiter/db/gloogle_migrated.sql.gz"
    [ -s "$dump" ] || { echo "no dump: set DUMP_FILE or PROD_DATABASE_URL, or put the snapshot at $dump" >&2; exit 1; }
    echo "→ using the local production snapshot $dump"
  fi
  echo "→ resetting local database"
  psql "$LOCAL_DATABASE_URL" -v ON_ERROR_STOP=1 -q \
    -c "DROP SCHEMA public CASCADE; CREATE SCHEMA public;"
  echo "→ restoring $dump"
  case "$dump" in
    *.sql.gz) gzip -dc "$dump" | psql "$LOCAL_DATABASE_URL" -v ON_ERROR_STOP=1 -q ;;
    *.sql)    psql "$LOCAL_DATABASE_URL" -v ON_ERROR_STOP=1 -q -f "$dump" ;;
    *)        need pg_restore
              pg_restore --no-owner --no-privileges --exit-on-error \
                --dbname="$LOCAL_DATABASE_URL" "$dump" ;;
  esac
  echo "→ applying migrations newer than the dump"
  # `--url` rather than DATABASE_URL: a mise shim or a `.env` would override the
  # variable with the developer's own database, and migrate the wrong one.
  (cd "$root/apps/jupiter" && dbmate --url "$LOCAL_DATABASE_URL" --no-dump-schema up)
  psql "$LOCAL_DATABASE_URL" -Atc "
    select 'package', count(*) from package
    union all select 'package_release', count(*) from package_release
    union all select 'package_type_fun_signature', count(*) from package_type_fun_signature
    union all select 'hex_read', count(*) from hex_read
    union all select 'analytics', count(*) from analytics
    union all select 'analytics_timeseries', count(*) from analytics_timeseries
    union all select 'search_analytics', count(*) from search_analytics;"
}

seed_s3() {
  require_env SCALEWAY_ACCESS_KEY; require_env SCALEWAY_SECRET_KEY; require_env BUCKET_URI
  local venv="$root/infra/local/.venv"
  if [ ! -x "$venv/bin/python" ]; then
    echo "→ creating $venv (boto3)"
    python3 -m venv "$venv" && "$venv/bin/pip" install -q boto3
  fi
  LOCAL_S3_ENDPOINT="$LOCAL_S3_ENDPOINT" LOCAL_S3_ACCESS_KEY="$LOCAL_S3_ACCESS_KEY" \
  LOCAL_S3_SECRET_KEY="$LOCAL_S3_SECRET_KEY" LOCAL_BUCKET="$LOCAL_BUCKET" \
    "$venv/bin/python" "$here/mirror_bucket.py" mirror
}

case "${1:-all}" in
  db) seed_db ;;
  s3) seed_s3 ;;
  all) seed_db; seed_s3 ;;
  *) echo "usage: $0 [db|s3|all]" >&2; exit 2 ;;
esac
