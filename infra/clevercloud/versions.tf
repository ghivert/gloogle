terraform {
  required_version = ">= 1.10"

  required_providers {
    clevercloud = {
      source  = "CleverCloud/clevercloud"
      version = "~> 3.1"
    }
    random = {
      source  = "hashicorp/random"
      version = "~> 3.6"
    }
  }

  # State on a dedicated Cellar add-on (gloogle-tfstate, see README.md), and
  # kept outside this stack (a `tofu destroy` must not take its own state with
  # it). Credentials come from the environment:
  #   AWS_ACCESS_KEY_ID / AWS_SECRET_ACCESS_KEY   that add-on's keys
  # Cellar is Ceph behind an S3 API: no AWS region, account or checksum
  # checks, path-style addressing.
  backend "s3" {
    bucket    = "gloogle-tfstate"
    key       = "clevercloud/terraform.tfstate"
    region    = "us-east-1" # unused by Cellar, required by the backend
    endpoints = { s3 = "https://cellar-c2.services.clever-cloud.com" }

    use_path_style              = true
    skip_credentials_validation = true
    skip_region_validation      = true
    skip_requesting_account_id  = true
    skip_metadata_api_check     = true
    skip_s3_checksum            = true

    # No state lock. `use_lockfile` relies on a conditional write
    # (If-None-Match), which Cellar ignores: tested on 2026-11-26, a plan ran
    # over an existing lock, overwrote it, then failed to release it. Fine
    # while a single person runs `tofu`; two concurrent applies would race.
    use_lockfile = false
  }

  # The state holds the database password, the Cellar keys and the tokens
  # passed to jupiter: encrypted client-side, so the bucket only ever sees
  # ciphertext. `enforced` refuses to write it in clear.
  encryption {
    key_provider "pbkdf2" "passphrase" {
      passphrase = var.state_passphrase
    }

    method "aes_gcm" "state" {
      keys = key_provider.pbkdf2.passphrase
    }

    state {
      method   = method.aes_gcm.state
      enforced = true
    }

    plan {
      method   = method.aes_gcm.state
      enforced = true
    }
  }
}

# Credentials come from the environment, never from this file:
#   CC_OAUTH_TOKEN / CC_OAUTH_SECRET   from `clever login` (see README.md)
provider "clevercloud" {
  organisation = var.organisation
}
