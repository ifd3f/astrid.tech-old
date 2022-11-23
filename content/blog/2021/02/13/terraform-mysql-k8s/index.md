---
title:
  Declaratively Provision Databases and Submit Credentials to Kubernetes using
  Terraform
date: 2021-02-13 12:10:50-08:00
description: DATABASES! I didn't say it, I DECLARED it!
tags:
  - /projects/infrastructure/
  - terraform
  - mysql
  - proxmox
  - kubernetes
  - devops
---

Firefly III is a budget management app that I'm trying to self-host. Being a
budget management app, it would hold every single monetary transaction I make,
which is obviously somewhat sensitive. As such, I'm running it on my local
cluster instead of on a public cloud instance that also hosts my website
backend.

## Provisioning the server manually

Firefly uses MySQL as its database backend, so I've spun up an LXC container
built from TurnKeyLinux's excellent MySQL container image through Proxmox.

![The downloadable TurnKeyLinux MySQL LXC image](https://s3.us-west-000.backblazeb2.com/nyaabucket/c006194c13cd7b9df4913e00a3c5ebe10ff01137666db05afede38e84a3932a8/turnkey-mysql.png)

![The LXC container running.](https://s3.us-west-000.backblazeb2.com/nyaabucket/5652db4a4475cd266ad8e9eeac08d3ec47e4ade188e59f8093a7eadcb1dbfa09/proxmox-container.png)

Unfortunately, I didn't automate this step; I just manually provisioned it
through the web UI like a pleb. However, I will try to automate it using
Terraform for my Postgres database, possibly in a later blog post.

## Provisioning the database automatically

However, once I had that out of the way, I wrote a short Terraform configuration
that:

1. Provisions a MySQL database
2. Provisions a user with an automatically-generated password
3. Securely creates a secret in Kubernetes to be used in Firefly

### Declaring plugins and providers

First, we declare the plugins we use.

```hcl
terraform {
  required_version = ">= 0.13.0"

  required_providers {
    random = {
      source  = "hashicorp/random"
      version = ">= 2.2.0"
    }
    mysql = {
      source  = "terraform-providers/mysql"
      version = ">= 1.5"
    }
  }
}
```

Next, we declare some variables for our MySQL admin parameters.

```hcl
variable "mysql_host" {
  description = "MySQL server host."
  type        = string
}

variable "mysql_port" {
  description = "MySQL server port."
  type        = string
}

variable "mysql_admin_user" {
  description = "MySQL server administrator's username."
  type        = string
}

variable "mysql_admin_password" {
  description = "MySQL server administrator's password."
  type        = string
  sensitive   = true
}
```

We can import them in an `.env` file that looks like this.

```bash
export TF_VAR_mysql_host=...
export TF_VAR_mysql_port=...
export TF_VAR_mysql_admin_user=...
export TF_VAR_mysql_admin_password=...
```

Combining all of those variables together, we declare the following MySQL
provider:

```hcl
provider "mysql" {
  endpoint = "${var.mysql_host}:${var.mysql_port}"
  username = var.mysql_admin_user
  password = var.mysql_admin_password
  tls = "skip-verify"
}
```

Additionally, we declare our Kubernetes provider to allow us to directly insert
our passwords into the cluster as a Secret.

```hcl
provider "kubernetes" {
  config_path    = "~/.kube/config"
}
```

### Declaring a Database and User

Declaring a database is as simple as

```hcl
resource "mysql_database" "firefly" {
  name = "fireflyiii"
}
```

To create our user, however, we need to first generate a password. This creates
a 16-char password.

```hcl
resource "random_password" "firefly_password" {
  length = 16
  special = true
  override_special = "_%@"
}
```

With our password, we can create the user.

```hcl
resource "mysql_user" "firefly" {
  user = "fireflyop"
  plaintext_password  = random_password.firefly_password.result
  host = "192.168.1.%"
}
```

The variable `random_password.firefly_password.result` is in
`<provider>.<resource name>.<field name>` form.

In order to let our user actually do things, we need to grant them all the
privileges on the `firefly` database.

**Warning:** To be honest, I don't know how to do this the right way. I thought
it could do a `GRANT ALL ON ...` with the following segment of code:

```hcl
resource "mysql_grant" "firefly" {
  user = mysql_user.firefly.user
  host = mysql_user.firefly.host
  database = mysql_database.firefly.name
  privileges = ["ALL"]
}
```

but this doesn't seem to work. I ended up running the Terraform script to
provision almost all of the resources, then granting the privileges manually.

### Giving Kubernetes the Passwords

There is one final step that is specific to Firefly III that we must do. Firefly
III encrypts its database with a 32-character key, so we need to generate that.
I could have done this outside of Terraform, but I thought it would be more
convenient to do it inside.

```hcl
resource "random_password" "firefly_key" {
  length = 32
  special = true
  override_special = "_%@{}~`[]()"
}
```

Finally, we can submit all of our secrets to Kubernetes! We do this by taking
all of our variables from above and combining it together.

```hcl
resource "kubernetes_secret" "firefly" {
  type = "Opaque"

  metadata {
    name = "firefly-db"
    namespace = "firefly-iii"
  }

  data = {
    "APP_KEY" = random_password.firefly_key.result
    "DB_USERNAME" = mysql_user.firefly.user
    "DB_PASSWORD" = random_password.firefly_password.result
    "DB_HOST" = var.mysql_host
    "DB_PORT" = 3306
    "DB_DATABASE" = mysql_database.firefly.name
    "DB_CONNECTION" = "mysql"
    "MYSQL_USE_SSL" = "true"
    "MYSQL_SSL_VERIFY_SERVER_CERT" = "false"
    "MYSQL_SSL_CAPATH" = "/etc/ssl/certs/"
  }
}
```

This creates `secret/firefly-db` under namespace `firefly-iii` with all of the
parameters that we want.

### Putting it all together

We have a full configuration file now!

```hcl
terraform {
  required_version = ">= 0.13.0"

  required_providers {
    random = {
      source  = "hashicorp/random"
      version = ">= 2.2.0"
    }
    mysql = {
      source  = "terraform-providers/mysql"
      version = ">= 1.5"
    }
  }
}

variable "mysql_host" {
  description = "MySQL server host."
  type        = string
}

variable "mysql_port" {
  description = "MySQL server port."
  type        = string
}

variable "mysql_admin_user" {
  description = "MySQL server administrator's username."
  type        = string
}

variable "mysql_admin_password" {
  description = "MySQL server administrator's password."
  type        = string
  sensitive   = true
}

provider "mysql" {
  endpoint = "${var.mysql_host}:${var.mysql_port}"
  username = var.mysql_admin_user
  password = var.mysql_admin_password
  tls = "skip-verify"
}

resource "mysql_database" "firefly" {
  name = "fireflyiii"
}

resource "random_password" "firefly_password" {
  length = 16
  special = true
  override_special = "_%@"
}

resource "mysql_user" "firefly" {
  user = "fireflyop"
  plaintext_password  = random_password.firefly_password.result
  host = "192.168.1.%"
}

resource "mysql_grant" "firefly" {
  user = mysql_user.firefly.user
  host = mysql_user.firefly.host
  database = mysql_database.firefly.name
  privileges = ["ALL"]
}

provider "kubernetes" {
  config_path    = "~/.kube/config"
}

resource "random_password" "firefly_key" {
  length = 32
  special = true
  override_special = "_%@{}~`[]()"
}

resource "kubernetes_secret" "firefly" {
  type = "Opaque"

  metadata {
    name = "firefly-db"
    namespace = "firefly-iii"
  }

  data = {
    "APP_KEY" = random_password.firefly_key.result
    "DB_USERNAME" = mysql_user.firefly.user
    "DB_PASSWORD" = random_password.firefly_password.result
    "DB_HOST" = var.mysql_host
    "DB_PORT" = 3306
    "DB_DATABASE" = mysql_database.firefly.name
    "DB_CONNECTION" = "mysql"
    "MYSQL_USE_SSL" = "true"
    "MYSQL_SSL_VERIFY_SERVER_CERT" = "false"
    "MYSQL_SSL_CAPATH" = "/etc/ssl/certs/"
  }
}
```

To deploy, it's as simple as

```bash
. ./.env
terraform apply
```

## The Kubernetes app manifest

I can declare the Firefly III app as a StatefulSet (stateful because it needs to
store some upload data). Inside a single container's spec, we can apply all of
our secrets into its environment variables with a `envFrom[].secretRef` like so:

```yaml
- name: app
  image: registry.hub.docker.com/jc5x/firefly-iii:latest
  env:
    - name: TZ
      value: America/Los_Angeles
  envFrom:
    - secretRef:
        name: firefly-db
  ...
```

After applying this manifest and its associated service and ingress, everything
seems to work!

![It't aliiiiiive!](https://s3.us-west-000.backblazeb2.com/nyaabucket/2f0bfcac87a2ece05f4a66c8297d6de82bc52ee6150e6a61b035168a37fb7391/firefly-works.png)

## What next?

While this solution is much nicer and more automated than creating the database
manually in a GUI or SQL script, this isn't the most secure way to provision a
user. I heard that Hashicorp Vault can actually generate new passwords
specifically for one app on the fly, and I might explore that later.
