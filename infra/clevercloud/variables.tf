variable "state_passphrase" {
  description = "Passphrase encrypting the state (at least 16 characters). Lose it and the state is unreadable: keep it in a password manager."
  type        = string
  sensitive   = true
}

variable "organisation" {
  description = "Clever Cloud organisation (orga_xxx) or personal space (user_xxx)."
  type        = string
}

variable "name" {
  description = "Prefix of every resource name."
  type        = string
  default     = "gloogle"
}

variable "region" {
  description = "Clever Cloud region of the app and the database."
  type        = string
  default     = "par"
}

# --- Database -----------------------------------------------------------------

variable "postgresql_plan" {
  description = <<-EOT
    PostgreSQL add-on plan (`clever addon providers show postgresql-addon`).
    The database is ~1.5 GB with its indexes: xs_sml = 1 vCPU, 1 GB RAM, 5 GB,
    75 connections. Not `dev`: no extensions, 5 connections, 256 MB.
  EOT
  type        = string
  default     = "xs_sml"
}

variable "postgresql_version" {
  description = "PostgreSQL major version. The schema needs 18 (`uuidv7()`)."
  type        = string
  default     = "18"
}

variable "database_route" {
  description = <<-EOT
    How jupiter reaches Postgres:
    - "networkgroup" (default): the private WireGuard network, with
      `sslmode=networkgroup_sslmode` in DATABASE_URL, which jupiter honours.
      WireGuard authenticates both ends.
    - "public": the add-on URI, through Clever Cloud's proxy, no sslmode, so
      jupiter uses `SslVerified`. Does not work on Clever Cloud today: the
      add-on's certificate is self-signed per instance, and Erlang's `ssl`
      refuses a self-signed peer even when it is in the trust store
      (`selfsigned_peer`, checked 2026-11-26 on OTP 29).
  EOT
  type        = string
  default     = "networkgroup"

  validation {
    condition     = contains(["public", "networkgroup"], var.database_route)
    error_message = "database_route must be \"public\" or \"networkgroup\"."
  }
}

variable "networkgroup_database_port" {
  description = "Postgres port inside the network group: 8080 for every dedicated add-on (TLS for PostgreSQL), not POSTGRESQL_ADDON_PORT."
  type        = number
  default     = 8080
}

variable "networkgroup_sslmode" {
  description = "sslmode over the network group. Postgres speaks TLS on 8080 there (checked 2026-11-26): `require`, encrypted and unverified; WireGuard authenticates the peers."
  type        = string
  default     = "require"
}

# --- Object storage -----------------------------------------------------------

variable "bucket_name" {
  description = "Cellar bucket of the Hex archive cache. Names are global on Cellar."
  type        = string
  default     = "gloogle-hex-mirror"
}

# --- Application --------------------------------------------------------------

variable "jupiter_flavor" {
  description = <<-EOT
    jupiter's Docker instance flavor (nano, XS, S, M…). At least XS (1.1 GB):
    ingestion falls back to `gleam docs build` when HexDocs has no package
    interface.
  EOT
  type        = string
  default     = "XS"
}

variable "jupiter_domains" {
  description = "Virtual hosts of the API. Keep a cleverapps.io one to test before switching the DNS."
  type        = list(string)
  default     = ["gloogle-jupiter.cleverapps.io", "api.gloogle.run"]
}

variable "registry_user" {
  description = "GHCR user, only if the gloogle package is private (it is public)."
  type        = string
  default     = null
}

variable "registry_password" {
  description = "GHCR token with read:packages, only if the gloogle package is private (it is public)."
  type        = string
  default     = null
  sensitive   = true
}

variable "log_level" {
  description = "jupiter LOG_LEVEL."
  type        = string
  default     = "INFO"
}

# --- Secrets (terraform.tfvars, gitignored) ----------------------------------

variable "hex_api_key" {
  description = "Hex API key, used to list new releases."
  type        = string
  sensitive   = true
}

variable "github_token" {
  description = "GitHub token, used to crawl stargazers."
  type        = string
  sensitive   = true
}
