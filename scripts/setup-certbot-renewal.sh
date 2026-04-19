#!/bin/bash
# Run once to wire up automated certbot renewal with DuckDNS hooks.
# Must be run from the project root with sudo:
#   sudo bash scripts/setup-certbot-renewal.sh

set -e

SCRIPTS_DIR="$(cd "$(dirname "$0")" && pwd)"
PROJECT_DIR="$(dirname "$SCRIPTS_DIR")"

# Load config from .env
source "${PROJECT_DIR}/.env"

echo "==> Writing project path pointer for hook scripts..."
echo "${PROJECT_DIR}" > /etc/letsencrypt/nhlpool-project-dir
chmod 644 /etc/letsencrypt/nhlpool-project-dir

echo "==> Installing hook scripts..."
cp "$SCRIPTS_DIR/certbot-duckdns-auth.sh"    /usr/local/bin/certbot-duckdns-auth.sh
cp "$SCRIPTS_DIR/certbot-duckdns-cleanup.sh" /usr/local/bin/certbot-duckdns-cleanup.sh
chmod +x /usr/local/bin/certbot-duckdns-auth.sh
chmod +x /usr/local/bin/certbot-duckdns-cleanup.sh

echo "==> Installing deploy hook (nginx reload after renewal)..."
cp "$SCRIPTS_DIR/certbot-nginx-reload.sh" /etc/letsencrypt/renewal-hooks/deploy/reload-nginx.sh
chmod +x /etc/letsencrypt/renewal-hooks/deploy/reload-nginx.sh

echo "==> Re-issuing certificate with hooks wired in..."
certbot certonly --manual --preferred-challenges dns \
  --manual-auth-hook /usr/local/bin/certbot-duckdns-auth.sh \
  --manual-cleanup-hook /usr/local/bin/certbot-duckdns-cleanup.sh \
  --force-renewal \
  -d "${DUCKDNS_DOMAIN}.duckdns.org"

echo ""
echo "==> Testing automated renewal (dry run)..."
certbot renew --dry-run

echo ""
echo "Done. Certbot will renew automatically via systemd certbot.timer."
echo "Verify: systemctl status certbot.timer"
