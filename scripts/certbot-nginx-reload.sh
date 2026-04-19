#!/bin/bash
# Certbot deploy hook — reloads nginx after a successful certificate renewal
PROJECT_DIR=$(cat /etc/letsencrypt/nhlpool-project-dir)
cd "${PROJECT_DIR}"
docker compose exec nginx nginx -s reload
