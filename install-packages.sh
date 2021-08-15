#!/bin/bash

# Bash "strict mode". See
# http://redsymbol.net/articles/unofficial-bash-strict-mode/
set -euo pipefail

export DEBIAN_FRONTEND=noninteractive
apt-get update
apt-get -y upgrade
apt-get -y install --no-install-recommends $PACKAGES
apt-get -y autoremove
apt-get clean
rm -rf /var/lib/apt/lists/*
