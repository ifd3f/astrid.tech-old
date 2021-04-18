---
title: How to set up Dynamic DNS on FreeIPA for your Kubernetes Cluster
date: 2021-04-18T12:35:45-08:00
tags:
  - kubernetes
  - freeipa
  - dns
  - devops
---

I have a [FreeIPA](https://www.freeipa.org/page/Main_Page) server that serves DNS on my home network. I wanted to automatically configure it with my Kubernetes ingresses using [ExternalDNS](https://github.com/kubernetes-sigs/external-dns). There doesn't seem to be much documentation for this specific setup, so I thought to put together this guide!

## Requirements

This guide will assume that you have the following set up already:

- A Kubernetes cluster running on your network
- A FreeIPA server (let's say `ipa0.p.astrid.tech`) serving DNS for a certain zone you want as the domain suffixes (call it `s.astrid.tech`)
- An app (or apps) on the Kubernetes cluster exposed on an Ingress (we'll assume it's `firefly.s.astrid.tech`)

In addition, I used the following guides to assemble this guide:

- [FreeIPA - Howto/ISC DHCPd and Dynamic DNS update](https://www.freeipa.org/page/Howto/ISC_DHCPd_and_Dynamic_DNS_update)
- [FreeIPA - Howto/DNS updates and zone transfers with TSIG](https://www.freeipa.org/page/Howto/DNS_updates_and_zone_transfers_with_TSIG)
- [Flylib.com - Allowing Dynamic Updates](https://flylib.com/books/en/2.684.1/allowing_dynamic_updates.html)
- [ExternalDNS - Configuring RFC2136 provider](https://github.com/kubernetes-sigs/external-dns/blob/master/docs/tutorials/rfc2136.md)
- [bitnami/external-dns Helm chart](https://github.com/bitnami/charts/tree/master/bitnami/external-dns)

## 1. Generate a TSIG key and register it

We will need to generate a TSIG key first.

Choose a name for your key. I called mine `k8s` but we'll call it `keyname`. Then on your FreeIPA server, execute

```bash
$ dnssec-keygen -a HMAC-SHA512 -b 512 -n HOST keyname
```

and you will get 2 files in your working directory that are probably called something like `Kkeyname.+165+44840.key` and `Kkeyname.+165+44840.private`. Open up the `.private` one:

```
$ cat Kkeyname.+165+44840.private
Private-key-format: v1.3
Algorithm: 165 (HMAC_SHA512)
Key: zeOLcYcv/95yZX1KSLDreZyMtAsy5Ci5xwC9gW7XAgtOnOTIJpyr03CNDA8sUxfrkhb6Hjs90d3zRGm2g0XDaQ==
Bits: AAA=
Created: 20210418040622
Publish: 20210418040622
Activate: 20210418040622
```

and copy the part after `Key:`.

Finally, add the following to your `/etc/named.conf`, but substitute the key for your key:

```conf
key "keyname" {
       algorithm hmac-sha512;
       secret "zeOLcYcv/95yZX1KSLDreZyMtAsy5Ci5xwC9gW7XAgtOnOTIJpyr03CNDA8sUxfrkhb6Hjs90d3zRGm2g0XDaQ==";
};
```

Repeat this for every FreeIPA server you have, and we can move onto the next step.

## 2. Enable DDNS on your FreeIPA server

This step can be done via UI or CLI, but I did it via UI.

First, navigate to your DNS zone's settings page.

![How the top of your UI should look](./dns-settings.png)

Scroll down to where it says "Dynamic update" and set that to True. Additionally, add the following line[^guide-dev-1] to "BIND update policy," replacing `keyname` with your key and `s.astrid.tech` with your zone:

```
grant keyname subdomain s.astrid.tech ANY;
```

[^guide-dev-1]: Note that this is a deviation from [this guide](https://www.freeipa.org/page/Howto/DNS_updates_and_zone_transfers_with_TSIG), where it says to write `grant keyname name s.astrid.tech ANY;`. `name` only allows you to change `s.astrid.tech`, while `subdomain` allows you to change every domain like `*.s.astrid.tech`, as described [here](https://flylib.com/books/en/2.684.1/allowing_dynamic_updates.html).

Now, your UI should look something like this:

![How your UI should look after making these changes](./ddns-and-bind-update-policy.png)

Save your changes, and anyone with that secret key can add anything to that subdomain.

## 3. Install ExternalDNS on your Kubernetes cluster

[ExternalDNS](https://github.com/kubernetes-sigs/external-dns) is an addon for Kubernetes that has functionality to provide DNS updates over RFC2136. I installed the [bitnami/external-dns Helm chart](https://github.com/bitnami/charts/tree/master/bitnami/external-dns) using the following [Helmfile](https://github.com/roboll/helmfile):

```yaml
repositories:
  - name: bitnami
    url: https://charts.bitnami.com/bitnami
releases:
  - name: freeipa-dns-sync
    namespace: external-dns
    chart: bitnami/external-dns
    installed: true
    values:
      - provider: rfc2136
        logFormat: json
        domainFilters:
          - s.astrid.tech # only handle DDNS for *.s.astrid.tech domains
        rfc2136:
          host: ipa0.p.astrid.tech # replace with your host
          zone: s.astrid.tech # replace with your zone
          tsigKeyname: keyname # replace with your keyname
          tsigSecretAlg: hmac-sha512
          secretName: freeipa-rfc2136
```

After deploying this with `helmfile apply`, I then installed the following secret:

```yaml
apiVersion: v1
kind: Secret
metadata:
  name: freeipa-rfc2136
  namespace: external-dns
type: Opaque
stringData:
  # replace with your secret
  rfc2136_tsig_secret: zeOLcYcv/95yZX1KSLDreZyMtAsy5Ci5xwC9gW7XAgtOnOTIJpyr03CNDA8sUxfrkhb6Hjs90d3zRGm2g0XDaQ==
```

And that's it! You should soon see DNS records show up in FreeIPA automatically.

![FreeIPA DNS, but with automatically updated DNS settings](./dns-complete.png)

For debugging, you may want to check external DNS's logs using `kubectl logs [podname]`.
