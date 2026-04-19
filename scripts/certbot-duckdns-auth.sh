#!/bin/bash
# Certbot manual auth hook — sets _acme-challenge TXT record via DuckDNS API
PROJECT_DIR=$(cat /etc/letsencrypt/nhlpool-project-dir)
source "${PROJECT_DIR}/.env"
curl -s "https://www.duckdns.org/update?domains=${DUCKDNS_DOMAIN}&token=${DUCKDNS_TOKEN}&txt=${CERTBOT_VALIDATION}&verbose=true"
sleep 30
