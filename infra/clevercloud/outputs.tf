output "jupiter_app_id" {
  description = "Clever Cloud ID of jupiter (app_xxx). CI does not need it: it deploys by app name."
  value       = clevercloud_docker.jupiter.id
}

output "jupiter_domains" {
  description = "Virtual hosts of the API. DNS: api.gloogle.run CNAME domain.par.clever-cloud.com."
  value       = var.jupiter_domains
}

output "postgresql_id" {
  description = "Clever Cloud ID of the PostgreSQL add-on (postgresql_xxx)."
  value       = clevercloud_postgresql.db.id
}

output "postgresql_host" {
  description = "Public host:port of the add-on, through the Clever Cloud proxy."
  value       = "${clevercloud_postgresql.db.host}:${clevercloud_postgresql.db.port}"
}

output "postgresql_uri" {
  description = "Public connection string, for the one-off restore (`tofu output -raw postgresql_uri`)."
  value       = clevercloud_postgresql.db.uri
  sensitive   = true
}

output "networkgroup_id" {
  description = "Network group ID, for `clever ng get-config` (join it from a laptop)."
  value       = clevercloud_networkgroup.private.id
}

output "networkgroup_database_host" {
  description = "Hostname of the database inside the network group (port 8080)."
  value       = local.networkgroup_database_host
}

output "bucket_uri" {
  description = "jupiter's BUCKET_URI: the Cellar bucket, path-style."
  value       = local.bucket_uri
}

output "cellar_endpoint" {
  description = "Cellar S3 endpoint, for the one-off copy of the archives (LOCAL_S3_ENDPOINT=https://…)."
  value       = local.cellar_endpoint
}

output "cellar_key_id" {
  description = "Cellar access key (SCALEWAY_ACCESS_KEY in jupiter)."
  value       = clevercloud_cellar.storage.key_id
}

output "cellar_key_secret" {
  description = "Cellar secret key (SCALEWAY_SECRET_KEY in jupiter)."
  value       = clevercloud_cellar.storage.key_secret
  sensitive   = true
}

output "admin_token" {
  description = "Bearer token of jupiter's admin routes: curl -X POST -H \"authorization: Bearer $(tofu output -raw admin_token)\" https://api.gloogle.run/packages/update/<name>"
  value       = random_password.admin_token.result
  sensitive   = true
}
