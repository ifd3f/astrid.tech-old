---
title: A Kubernetes cluster but it's only made of used computers
date: 2021-01-20 22:14:00-08:00
description: 16-year-old me would be proud
tags:
  - kubernetes
  - docker
  - server
  - devops
  - elk
  - proxmox
---

In [my last post](/2021/01/18/selfhost/) had 7 things I wanted to do. Well, here's a summary of everything I did so far. The [IaC repo is here](https://github.com/plenglin/astrid.tech-deployment) and the last commit before I published this article was [d18736](https://github.com/Plenglin/astrid.tech-deployment/tree/d18736ae4cf926c177488a07565521328ba09aee).

## Monday 1/18: Setting up k8s and an Elastic stack

I woke up awfully early that morning at 5 AM, way earlier than I usually wake up. It was MLK day, so I didn't have classes, and so I decided to spend the day setting up Kubernetes. I started out with 2 machines, both running Debian Buster Stable:

- The HP Pavilion x360 laptop with a cracked screen that I used to use before the screen cracked (named `cracktop`)
- An old Dell Optiplex workstation (that STILL RUNS DDR2 mind you) I got off of eBay a few years ago for doing something similar (named `deskrap`)

I basically spent from 5 AM until 9 PM trying to set up k8s[^1] the Proper Way :tm: on those two machines and it was painful and it didn't work for some reason.

But then, I learned about this cool thing called k3s, which is like a much more lightweight version of k8s. That's perfect, because my machines are all very cheap and have very few resources. Additionally, someone made a [very simple setup utility](https://github.com/alexellis/k3sup) that sets everything up on a remote for you, so I got k3s up around 10 PM. So, I could check tasks #1 and #6 off.

The whole reason I was doing this in the first place was to get an ELK[^2] stack set up. I only got the E and K before I set up MetricBeat to record system metrics like memory and CPU usage. I got everything set up around 3 AM, then I felt really tired and went to bed. I guess that's part of #2 (proper centralized logging) checked off.

---

[^1]: Kubernetes is usually abbreviated to k8s because there's 8 characters between k and s.
[^2]: Elasticsearch, Logstash, and Kibana. Logstash gathers and parses logs, then it sends to Elasticsearch to index logs, and Kibana looks at the logs and makes cool squiggly graphs.

## Tuesday 1/19: Setting up Firefly III and FileBeat

The next morning, I woke up at 8 AM in pain because I did have class that day. Before class started at 9 AM, I set up Kibana to make cool graphs from the MetricBeat data collected overnight.

![Cool and squiggly graphs that might have meanings](./kibana.png)

After class, I went on the the second order of business, which was setting up those private apps like Firefly III on Kubernetes. I successfully migrated the persistent MySQL container data off of the VPS and onto `deskrap`, and after much finagling with configuration and secrets, it surprisingly worked without any data corruption.

After bombing an interview with Facebook, I got ahold of my mom's old Acer Aspire E1-510 that she didn't need anymore. It was running terribly slow because it was bloatware-infested Windows 10 on a HDD. Even after I installed Debian Buster Stable and k3s and named it `crappertop`, it was still kinda slow, but it's fine; there were 3 nodes on my cluster and quantity > quality here.

At the very end, I got FileBeat running on a DaemonSet pod to forward all container logs into Elasticsearch. I felt satisfied, then went to bed at around 2 AM.

## Wednesday 1/20: Expanding the cluster further, then realizing I made a boo-boo

Today, I added another machine to the cluster: a Thinkpad T420 that I got used off of eBay for a different experiment than `deskrap`. Once again, I installed Debian Buster Stable, named the Thinkpad `thonkpad`, and hooked it up to the rest of the k3s cluster. I also learned how to deploy containers with Helm charts, and declaratively deploy Helm charts with Helmfiles.

Sometime in the afternoon, I was trying to set up a VPN system so that I could access my containers behind authentication. I learned about Wireguard and how it's much better than OpenVPN, but when I tried running `apt install -t bullseye-backports wireguard` on my machines like the instructions told me to, it couldn't find Wireguard. It turns out that Debian 10 uses Linux kernel 4.19, which is too old for Wireguard. So, I had a small existential crisis before accepting my fate of having to go to all four machines and reinstall them with a different distro.

So that I don't have to physically pull the machines onto my desk and use a USB stick as I have been doing, I decided to install Proxmox instead. Proxmox is a hypervisor, so I have to spin up a VM inside of it. However, I'd be slightly cheating; these machines are all so resource starved, I'm just gonna install a single fat VM on each of them and hopefully not experience too much overhead.

At first, I tried setting up OpenSUSE Kubic, which is a distro with k8s preinstalled for you. Then, I learned about K3OS, and installed that instead because, once again, I'm rather resource starved. While I was doing that, I was trying to set up Proxmox on `deskrap`, but the USB installer kept crashing. Maybe that machine is just too old/shitty for a modern hypervisor.

Now, as I'm writing this blog post, I'm trying to figure out how to move the elasticsearch data off of `cracktop`, which is the last node I have not attempted to install Proxmox on yet. I will say one thing, though: DevOps is _frustrating_. So much YAML-staring and hoping and praying things will work. I can only hope and pray at this point that the end result will be something I can be happy about.
