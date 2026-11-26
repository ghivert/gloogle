# Production hosting of jupiter on Clever Cloud. This stack only provisions:
# nothing is built or deployed from here. GitHub Actions builds the image in
# Docker and pushes it to the app below (.github/workflows/deploy.yml).
#
#   clevercloud_postgresql.db         PostgreSQL 18
#   clevercloud_cellar.storage        S3-compatible object storage (Cellar)
#   clevercloud_cellar_bucket.archives  the Hex archive cache jupiter reads/writes
#   clevercloud_docker.jupiter        the backend: runs the image CI pushed to GHCR
#   clevercloud_networkgroup.private  private network, jupiter <-> Postgres
#
# Not managed here: the database and bucket contents (copied once, see
# README.md), venus (Netlify, deployed by CI) and the DNS of gloogle.run (OVH).

locals {
  # Member labels the provider requires at link time. They do NOT resolve: the
  # group's DNS only serves `<memberID>.m.<ngID>.cc-ng.cloud`.
  networkgroup_database_fqdn = "${var.name}-postgres.cc-ng.cloud"
  networkgroup_jupiter_fqdn  = "${var.name}-jupiter.cc-ng.cloud"

  # How the database is reached inside the group: its member name (hyphens,
  # as Clever Cloud advises, underscores trip some clients), on port 8080 —
  # a dedicated add-on listens there inside a group, PostgreSQL with TLS.
  networkgroup_database_host = format(
    "%s.m.%s.cc-ng.cloud",
    replace(clevercloud_postgresql.db.id, "_", "-"),
    replace(clevercloud_networkgroup.private.id, "_", "-"),
  )

  networkgroup_database_url = format(
    "postgres://%s:%s@%s:%d/%s?sslmode=%s",
    clevercloud_postgresql.db.user,
    urlencode(clevercloud_postgresql.db.password),
    local.networkgroup_database_host,
    var.networkgroup_database_port,
    clevercloud_postgresql.db.database,
    var.networkgroup_sslmode,
  )

  database_url = (
    var.database_route == "networkgroup"
    ? local.networkgroup_database_url
    : clevercloud_postgresql.db.uri
  )

  # jupiter's BUCKET_URI in its URL form: path-style (`/<bucket>/<key>`), what
  # Cellar recommends. Cellar ignores the SigV4 region, so the `fr-par` that
  # aws/s3.gleam signs with is fine.
  cellar_endpoint = trimprefix(trimprefix(clevercloud_cellar.storage.host, "https://"), "http://")
  bucket_uri      = "https://${local.cellar_endpoint}/${clevercloud_cellar_bucket.archives.id}"

  # Registry login (CC_DOCKER_LOGIN_*), only when the GHCR package is private.
  private_registry = var.registry_password != null
}

resource "clevercloud_networkgroup" "private" {
  name        = var.name
  description = "Private network between jupiter and its database."
}

resource "clevercloud_postgresql" "db" {
  name    = "${var.name}-postgres"
  plan    = var.postgresql_plan
  version = var.postgresql_version
  region  = var.region
  backup  = true

  networkgroups = [{
    networkgroup_id = clevercloud_networkgroup.private.id
    fqdn            = local.networkgroup_database_fqdn
  }]
}

resource "clevercloud_cellar" "storage" {
  name   = "${var.name}-cellar"
  region = var.region
}

# Bucket names are global on Cellar: change `bucket_name` if it is taken.
resource "clevercloud_cellar_bucket" "archives" {
  cellar_id = clevercloud_cellar.storage.id
  id        = var.bucket_name
}

# Guards jupiter's admin routes (`POST /packages/update/:name`): without
# `authorization: Bearer <token>` they answer 404. `tofu output -raw admin_token`.
resource "random_password" "admin_token" {
  length  = 48
  special = false
}

resource "clevercloud_docker" "jupiter" {
  name        = "${var.name}-jupiter"
  description = "jupiter: search API, Hex ingestion and periodic workers."
  region      = var.region

  # Exactly one instance, no auto-scaling: the periodic workers (Hex sync every
  # 10 s, ranking, popularity, timeseries) run in the same BEAM as the API, so
  # a second instance would run them twice.
  min_instance_count = 1
  max_instance_count = 1
  smallest_flavor    = var.jupiter_flavor
  biggest_flavor     = var.jupiter_flavor

  # CI deploys a one-line Dockerfile, `FROM ghcr.io/…@sha256:…`: the platform
  # only pulls. Clever Cloud injects PORT=8080, which jupiter reads; GLEAM_ENV
  # stays unset (production: SSL on, CORS on gloogle.run).
  container_port = 8080
  redirect_https = true

  registry_url      = local.private_registry ? "ghcr.io" : null
  registry_user     = local.private_registry ? var.registry_user : null
  registry_password = var.registry_password

  dependencies = [clevercloud_postgresql.db.id]

  networkgroups = [{
    networkgroup_id = clevercloud_networkgroup.private.id
    fqdn            = local.networkgroup_jupiter_fqdn
  }]

  vhosts = [for domain in var.jupiter_domains : { fqdn = domain }]

  environment = {
    DATABASE_URL = local.database_url
    ADMIN_TOKEN  = random_password.admin_token.result
    LOG_LEVEL    = var.log_level
    HEX_API_KEY  = var.hex_api_key
    GITHUB_TOKEN = var.github_token
    # The keys of the Cellar add-on. The variable names are jupiter's
    # (context/environment/variables.gleam), from when the bucket was on
    # Scaleway.
    BUCKET_URI          = local.bucket_uri
    SCALEWAY_ACCESS_KEY = clevercloud_cellar.storage.key_id
    SCALEWAY_SECRET_KEY = clevercloud_cellar.storage.key_secret
  }
}
