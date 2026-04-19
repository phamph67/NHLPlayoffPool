#!/bin/bash
# Certbot manual cleanup hook — removes _acme-challenge TXT record after verification
PROJECT_DIR=$(cat /etc/letsencrypt/nhlpool-project-dir)
source "${PROJECT_DIR}/.env"
curl -s "https://www.duckdns.org/update?domains=${DUCKDNS_DOMAIN}&token=${DUCKDNS_TOKEN}&txt=cleared&clear=true"
