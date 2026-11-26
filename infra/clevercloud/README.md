# Production hosting on Clever Cloud (OpenTofu)

Clever Cloud only **hosts** jupiter and its database. Everything is built in
Docker on GitHub Actions (`.github/workflows/deploy.yml`), then pushed to the
app this stack provisions. Nothing here builds or deploys code. venus, the
frontend, stays on Netlify (free CDN, which Clever Cloud does not offer), also
deployed by that workflow from a Docker build.

| Resource | What | Receives from CI |
| --- | --- | --- |
| `clevercloud_postgresql.db` | PostgreSQL **18** (the schema needs `uuidv7()`), plan `xs_sml`, daily backups | — |
| `clevercloud_docker.jupiter` | the backend, **one instance, no auto-scaling** | a one-line `Dockerfile`: `FROM ghcr.io/ghivert/gloogle@sha256:…` (the platform only pulls) |
| `clevercloud_cellar.storage` + `clevercloud_cellar_bucket.archives` | S3 object storage (Cellar), bucket `gloogle-hex-mirror`: the Hex archive cache. Its keys go straight into jupiter's environment | — |
| `clevercloud_networkgroup.private` | WireGuard network, jupiter ↔ Postgres | — |

Out of scope: the database and bucket contents (copied once, below), venus
(Netlify), and the DNS of `gloogle.run` (OVH).

jupiter's bucket variables keep their historical names: `BUCKET_URI` is
`https://<cellar host>/<bucket>` (path-style, as Cellar recommends; Cellar
ignores the SigV4 region), and `SCALEWAY_ACCESS_KEY` / `SCALEWAY_SECRET_KEY`
hold the Cellar keys.

Why one jupiter instance: the periodic workers (Hex sync every 10 s, ranking,
popularity, timeseries) run in the same BEAM as the HTTP API. Two instances
would run them twice.

## Prerequisites

- OpenTofu ≥ 1.10 (`brew install opentofu`), clever-tools
  (`npm i -g clever-tools`), `clever login`.
- **The state bucket** already exists: Cellar add-on `gloogle-tfstate`, bucket
  `gloogle-tfstate`, in the Chouquette organisation. It stays outside this
  stack (the stack needs its state before creating anything, and a
  `tofu destroy` must not take its own state with it); within Cellar's free
  100 MB. Its keys: `clever addon env <its cellar_… id> --format json`
  (`CELLAR_ADDON_KEY_ID` / `CELLAR_ADDON_KEY_SECRET`). To recreate it from
  scratch:

  ```sh
  clever addon create cellar-addon gloogle-tfstate --plan S --region par --org <org>
  curl --aws-sigv4 "aws:amz:us-east-1:s3" --user "$KEY_ID:$KEY_SECRET" \
    -X PUT https://cellar-c2.services.clever-cloud.com/gloogle-tfstate
  ```

- **The environment**, in `infra/clevercloud/.mise.local.toml` (gitignored,
  loaded by mise in this folder):

  ```toml
  [env]
  # Provider: the `clever login` session (~/.config/clever-cloud/clever-tools.json).
  CC_OAUTH_TOKEN = "…"
  CC_OAUTH_SECRET = "…"
  # State backend: the keys of the gloogle-tfstate Cellar add-on.
  AWS_ACCESS_KEY_ID = "…"
  AWS_SECRET_ACCESS_KEY = "…"
  # Stack variables (or terraform.tfvars, gitignored too).
  TF_VAR_state_passphrase = "…" # 16+ characters, keep it in a password manager
  TF_VAR_organisation = "orga_…"
  TF_VAR_hex_api_key = "…"
  TF_VAR_github_token = "…"
  ```

### The state

It lives in the `gloogle-tfstate` bucket (backend `s3` in `versions.tf`),
**encrypted client-side** with `state_passphrase` (`encryption` block,
`enforced`): the database password, the Cellar keys and the tokens it holds
never reach the bucket in clear. Lose the passphrase and the state is
unreadable — the resources survive, but they would have to be re-imported.

**No state lock.** OpenTofu's S3 lock (`use_lockfile`) relies on a
conditional write (`If-None-Match`), and Cellar ignores it: tested on
2026-11-26, a `tofu plan` ran over an existing lock file, overwrote it, then
failed to release it. `use_lockfile` is therefore off. Harmless while one
person runs `tofu` at a time; two concurrent `apply` would race on the state.

The bucket lives in the **Chouquette** organisation (add-on
`gloogle-tfstate`, `cellar_2221d888-93f9-45fb-935a-3f6aef0a9e80`), like the
rest of the stack.

## First deployment

1. **Storage first**, so it is filled before jupiter starts syncing:

   ```sh
   tofu init
   tofu apply -target=clevercloud_networkgroup.private -target=clevercloud_postgresql.db \
     -target=clevercloud_cellar_bucket.archives
   ```

2. **Restore the local stack's database** — fully migrated and backfilled
   (IR, `signature_canonical`, `package_dependency`), so no one-off task runs
   afterwards. The add-on's owner role is not a superuser, so the
   `COMMENT ON EXTENSION` entries are skipped:

   ```sh
   docker compose -f ../local/compose.yml up -d postgres
   pg_dump --format=custom --no-owner --no-privileges \
     --dbname="postgres://gloogle:gloogle@localhost:25432/gloogle" --file=gloogle.dump
   pg_restore -l gloogle.dump | grep -v 'COMMENT - EXTENSION' > restore.list
   pg_restore --no-owner --no-privileges --jobs=4 --exit-on-error \
     --use-list=restore.list --dbname="$(tofu output -raw postgresql_uri)" gloogle.dump
   (cd ../../apps/jupiter && dbmate --url "$(cd ../../infra/clevercloud && tofu output -raw postgresql_uri)" --no-dump-schema status)
   psql "$(tofu output -raw postgresql_uri)" -c 'ANALYZE'
   ```

   `dbmate status` must list nothing pending. A refused `CREATE EXTENSION`
   means a support ticket (the five used — `btree_gin`, `pg_trgm`,
   `fuzzystrmatch`, `moddatetime`, `uuid-ossp` — are in Clever Cloud's default
   list). Data stops at the local snapshot; the Hex sync catches up from the
   `hex_read` cursor.

3. **Copy the archive cache** from the Scaleway bucket to Cellar (8,000+
   archives, ~600 MB), with the local stack's mirror script: it downloads to
   `infra/local/dumps/bucket-hex-mirror/` (skipping files already there), then
   uploads to whatever `LOCAL_S3_*` points at. Idempotent, resumable:

   ```sh
   BUCKET_URI=hex-mirror.s3.fr-par.scw.cloud \
   SCALEWAY_ACCESS_KEY=… SCALEWAY_SECRET_KEY=… \
   LOCAL_S3_ENDPOINT="https://$(tofu output -raw cellar_endpoint)" \
   LOCAL_S3_ACCESS_KEY="$(tofu output -raw cellar_key_id)" \
   LOCAL_S3_SECRET_KEY="$(tofu output -raw cellar_key_secret)" \
   LOCAL_BUCKET=gloogle-hex-mirror \
   AWS_REQUEST_CHECKSUM_CALCULATION=when_required \
   AWS_RESPONSE_CHECKSUM_VALIDATION=when_required \
     ../local/.venv/bin/python ../local/scripts/mirror_bucket.py mirror
   ```

   The two `AWS_*_CHECKSUM_*` variables are needed: boto3 ≥ 1.36 streams
   uploads with a trailing checksum and no `Content-Length`, which Cellar (Ceph)
   rejects (`MissingContentLength`). The venv comes from
   `../local/scripts/seed-local.sh s3`, or `python3 -m venv ../local/.venv &&
   ../local/.venv/bin/pip install boto3`.

   An existing copy on disk elsewhere (e.g. `infra/infra-parked/…`) can be
   moved to `infra/local/dumps/bucket-hex-mirror/` first to skip the download.
   The cache is not vital: a missing archive is fetched again from
   `repo.hex.pm`.

4. **The app**, empty until CI deploys. It joins the network group and
   reaches Postgres there (see *Database route*):

   ```sh
   tofu apply
   ```

5. **Wire GitHub** (Settings → Environments → `production`):
   - secrets `CLEVER_TOKEN`, `CLEVER_SECRET` (a clever-tools login, ideally a
     dedicated account), `NETLIFY_AUTH_TOKEN` (Netlify personal access token),
     `NETLIFY_SITE_ID` (Netlify → Site configuration → Site ID; site
     `gloogle-app`), optional `SENTRY_AUTH_TOKEN`;
   - variables: optional `CLEVER_ORG` (only if the app name `gloogle-jupiter`
     is ambiguous for that account), optional `SENTRY_ORG`, `SENTRY_PROJECT`.
     No app ID: CI deploys by the name this stack gives the app;
   - on GHCR, give the repository write access to the `gloogle` package
     (package settings → Manage Actions access → `ghivert/gloogle`, role
     Write): the package predates the workflow, so its `GITHUB_TOKEN` is
     denied otherwise (`permission_denied: write_package`);
   - on Netlify, stop the site's own builds (Site configuration → Build &
     deploy → Stop builds): CI deploys the build output.

   Then run the `deploy` workflow by hand (it also runs after every green
   `test` on main). Images go to the public GHCR package `gloogle`, tagged by
   commit SHA (never `latest`, which still names the 2024 image); Clever Cloud
   pulls it anonymously. Should the package become private, set
   `registry_user` / `registry_password` (token with `read:packages`) and
   `tofu apply` again.

6. **Check** before touching the DNS:
   - `https://gloogle-jupiter.cleverapps.io/healthcheck` answers 200, a typed
     query (`/search?q=fn(List(a))%20->%20Int`) answers with `"narrowed": true`,
     `clever logs --app gloogle-jupiter` shows the Hex sync without
     `JSON error`;

7. **DNS at OVH**, by hand: `api.gloogle.run` CNAME
   `domain.par.clever-cloud.com.` (switched from `gloogle.onrender.com.` on
   2026-11-26; Clever Cloud issued the Let's Encrypt certificate within a
   minute). The front's records (Netlify) do not change. Then shut down
   Render, and the Scaleway bucket once nothing reads it.

## Database route

`database_route = "networkgroup"` (default): jupiter reaches Postgres inside
the WireGuard network, at the add-on's **member name**
`postgresql-<id>.m.ng-<id>.cc-ng.cloud` (output `networkgroup_database_host`)
on **port 8080** — where every dedicated add-on listens inside a group,
PostgreSQL with TLS — and `sslmode=require` in `DATABASE_URL`: encrypted, not
verified; WireGuard authenticates both ends. jupiter honours an explicit
`sslmode` (`apps/jupiter/src/jupiter/context/postgres.gleam`, `ssl`).

Two traps, checked on 2026-11-26:
- the `fqdn` the provider requires when linking a member
  (`gloogle-postgres.cc-ng.cloud`) is a label only: the group's DNS does not
  serve it (`Nxdomain`). Only `<memberID>.m.<ngID>.cc-ng.cloud` resolves, from
  inside Clever Cloud; hyphens rather than underscores, as Clever advises;
- `POSTGRESQL_ADDON_PORT` (7451) is the public proxy's port: on the group,
  every port but 8080 refuses connections.

`"public"` (the add-on URI through Clever Cloud's proxy, `SslVerified`) does
not work: the add-on's certificate is self-signed, per instance (CN = the
add-on host, valid until 2036). psql accepts it in `verify-full` once given as
root, but Erlang's `ssl` refuses a self-signed peer even when it sits in the
trust store (`selfsigned_peer`, checked 2026-11-26 on OTP 29), and pog does not
expose a `verify_fun` to pin it.

To poke at the group from a member: `clever ssh --app gloogle-jupiter`. The
instance has neither bash nor nc, but has `erl` (the image's Erlang):
`gen_tcp:connect("<host>", 8080, [], 3000)`.

## Admin routes

`POST /packages/update/<name>` re-ingests a package (all its releases, Hex
called with jupiter's key). It answers 404 unless the request carries the
token this stack generates (`random_password.admin_token`, passed as
`ADMIN_TOKEN`):

```sh
curl -X POST -H "authorization: Bearer $(tofu output -raw admin_token)" \
  https://api.gloogle.run/packages/update/lumenmail
```

Rotate it with `tofu apply -replace=random_password.admin_token`, then
restart jupiter (`clever restart --app gloogle-jupiter`).

## Day to day

- Code: merge to main → `test` → `deploy` builds both apps and pushes jupiter
  to Clever Cloud, venus to Netlify.
- Infrastructure: change this stack, `tofu plan`, `tofu apply`. It never
  redeploys code, and a `tofu apply` does not undo a CI deployment.
