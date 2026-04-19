# Network Setup Guide

How external users reach `https://phillphamserver.duckdns.org/pool1/` and `/pool2/`.

---

## Architecture Overview

```
Internet
  │
  ▼
Router (home network, public IP)
  │  port 80 + 443 forwarded to 10.0.0.244
  ▼
Windows 11 server (10.0.0.244)
  │  WSL2 mirrored networking — ports bound in WSL appear directly on this IP
  ▼
WSL2 (Ubuntu) — same IP as Windows host in mirrored mode
  │
  ├── Docker: nginx container (ports 80 + 443)
  │     ├── /pool1/ → shiny_pool1:3838
  │     └── /pool2/ → shiny_pool2:3838
  │
  └── PostgreSQL (native WSL, port 5432)
        └── reachable from containers via host.docker.internal
```

**DuckDNS** (`phillphamserver.duckdns.org`) is a free dynamic DNS service that maps your domain name to your home router's public IP. You update it whenever your ISP changes your IP.

---

## Component Glossary

| Term | What it means |
|---|---|
| WSL2 mirrored networking | Windows feature where WSL and Windows share the same network interface — no separate WSL IP, no port proxy rules needed |
| nginx | A reverse proxy running in Docker. Handles HTTPS termination and routes `/pool1/` and `/pool2/` to the right Shiny container |
| Let's Encrypt | Free certificate authority. Issues SSL certificates so the browser shows the padlock (HTTPS) |
| certbot | Tool that requests and renews Let's Encrypt certificates |
| DNS-01 challenge | Proof-of-domain-ownership method: Let's Encrypt asks you to place a specific value in a DNS TXT record |
| DuckDNS API | HTTP endpoint that lets you update your DNS records programmatically — used here to automate the DNS-01 challenge |
| `.htpasswd` | Apache-style file holding hashed usernames/passwords for HTTP Basic Auth |

---

## One-Time Setup Steps

### 1. WSL2 Mirrored Networking

File: `C:\Users\Phillip\.wslconfig`

```ini
[wsl2]
networkingMode=mirrored
```

This must be set before WSL starts. After changing it, run `wsl --shutdown` in PowerShell and restart WSL.

**Why mirrored mode?** The old default (NAT mode) gave WSL a private IP (e.g. `172.x.x.x`) that wasn't the same as the Windows host. Ports bound in WSL weren't automatically reachable on the Windows IP — you'd need `netsh interface portproxy` rules. Mirrored mode eliminates all of that: WSL and Windows share `10.0.0.244`, so any port nginx binds in WSL is immediately accessible on the server's LAN IP.

**Status:** Done.

---

### 2. Router Port Forwarding

In your router admin panel, forward:

| Protocol | External port | Internal IP | Internal port |
|---|---|---|---|
| TCP | 80 | 10.0.0.244 | 80 |
| TCP | 443 | 10.0.0.244 | 443 |

Port 80 is needed so HTTP redirects to HTTPS work, and also so Let's Encrypt can reach the server during certificate renewal (HTTP-01 challenge fallback — though we use DNS-01 here, it's good to have open).

**Status:** Done.

---

### 3. DuckDNS

DuckDNS maps `phillphamserver.duckdns.org` → your router's public IP.

- Dashboard: https://www.duckdns.org (log in to manage)
- Your token is shown on the dashboard — treat it like a password

**Keeping the IP current:** If your ISP ever changes your public IP, DuckDNS will be stale. To auto-update, add this cron job (runs every 5 minutes):

```bash
crontab -e
```
Add:
```
*/5 * * * * curl -s "https://www.duckdns.org/update?domains=phillphamserver&token=YOUR_TOKEN_HERE&ip=" > /tmp/duckdns.log
```

Or use the DuckDNS Linux install script from their website — it sets up the cron automatically.

---

### 4. SSL Certificate (Let's Encrypt via certbot)

Let's Encrypt certificates are valid for **90 days**. Certbot handles renewal automatically — see Section 5.

#### Initial issuance

The cert was obtained using the DNS-01 challenge with manual verification:

```bash
sudo certbot certonly --manual --preferred-challenges dns \
  -d phillphamserver.duckdns.org
```

Certbot prompted for a DNS TXT record value. That was set on the DuckDNS dashboard, Let's Encrypt verified it, and the certificate was issued to `/etc/letsencrypt/live/phillphamserver.duckdns.org/`.

#### Why DNS-01 instead of HTTP-01?

HTTP-01 requires Let's Encrypt to reach your server on port 80. This works but ties cert renewal to the server being up and port 80 being accessible. DNS-01 only requires control of the DNS record — more flexible and works even if the server is temporarily offline.

---

### 5. Automatic Certificate Renewal

Certbot's systemd timer (`certbot.timer`) is already installed and enabled — it runs `certbot renew` twice daily. However, the original cert was issued with `authenticator = manual`, which requires a human to update the DNS record. To make renewal fully automated, we use **DuckDNS API hooks**.

#### How it works

When certbot renews, it runs:
1. `certbot-duckdns-auth.sh` — calls the DuckDNS API to set the `_acme-challenge` TXT record
2. Waits for DNS propagation (configurable)
3. Let's Encrypt verifies the TXT record
4. `certbot-duckdns-cleanup.sh` — removes the TXT record
5. `certbot-nginx-reload.sh` — reloads nginx so it picks up the new cert

#### Setup

All hook scripts live in `scripts/` and are committed to git. Machine-specific values (`PROJECT_DIR`, `DUCKDNS_TOKEN`, `DUCKDNS_DOMAIN`) are stored in `.env` (gitignored). The hooks find `.env` via a one-line pointer file written to `/etc/letsencrypt/nhlpool-project-dir` during setup.

**Step 5a — Verify `.env` has the required network variables:**

```
PROJECT_DIR=/home/<your-user>/projects/NHLPlayoffPool_dev
DUCKDNS_DOMAIN=phillphamserver
DUCKDNS_TOKEN=<from duckdns.org dashboard>
```

See `.env.example` for the full template.

**Step 5b — Run the setup script (once):**

```bash
cd /home/<your-user>/projects/NHLPlayoffPool_dev
sudo bash scripts/setup-certbot-renewal.sh
```

This script:
1. Reads `PROJECT_DIR`, `DUCKDNS_DOMAIN`, and `DUCKDNS_TOKEN` from `.env`
2. Writes `/etc/letsencrypt/nhlpool-project-dir` (path pointer for hooks)
3. Copies and makes executable: `certbot-duckdns-auth.sh`, `certbot-duckdns-cleanup.sh` → `/usr/local/bin/`
4. Copies `certbot-nginx-reload.sh` → `/etc/letsencrypt/renewal-hooks/deploy/`
5. Re-issues the certificate with hooks wired into the renewal config
6. Runs `certbot renew --dry-run` to confirm automation works

Should complete without any prompts. After this, `certbot.timer` handles renewals forever.

---

### 6. nginx Configuration

File: `nginx/nginx.conf`

nginx sits in front of the two Shiny containers and handles:
- HTTP → HTTPS redirect (port 80 → 443)
- SSL termination (reads certs from `/etc/letsencrypt/...`)
- Routing: `/pool1/` → `shiny_pool1:3838`, `/pool2/` → `shiny_pool2:3838`
- HTTP Basic Auth (prompts for username/password)
- Rate limiting (10 req/s per IP, burst of 20)

The containers communicate over Docker's internal network using service names (`shiny_pool1`, `shiny_pool2`) — no port exposure needed for the Shiny containers themselves.

**WebSocket headers** (`Upgrade` + `Connection`) are required because Shiny uses WebSockets for interactive sessions.

---

### 7. HTTP Basic Auth (.htpasswd)

The pool pages are password-protected. The credentials file is at `nginx/.htpasswd` (gitignored).

To create or add a user:

```bash
sudo apt install apache2-utils   # one-time
htpasswd -c nginx/.htpasswd USERNAME   # -c creates new file (use once)
htpasswd nginx/.htpasswd USERNAME      # add additional users (no -c)
```

Users enter these credentials when first visiting the site. The browser caches them for the session.

---

## Ongoing Maintenance

| Task | How |
|---|---|
| Check cert expiry | `sudo certbot certificates` |
| Force renew early | `sudo certbot renew --force-renewal` |
| Test renewal automation | `sudo certbot renew --dry-run` |
| Restart all services | `cd ~/projects/NHLPlayoffPool_dev && docker compose restart` |
| View nginx logs | `docker compose logs -f nginx` |
| Add/change pool password | `htpasswd nginx/.htpasswd USERNAME` then `docker compose restart nginx` |
| DuckDNS IP update fails | Check `cat /tmp/duckdns.log`; verify token in crontab |

---

## Troubleshooting

**Browser shows "Connection refused"**
- Are containers running? `docker compose ps`
- Is nginx bound to 80/443? `ss -tlnp | grep -E ':80|:443'`

**Browser shows SSL error / cert warning**
- Is the cert expired? `sudo certbot certificates`
- Did nginx reload after renewal? `docker compose exec nginx nginx -s reload`

**Can reach on LAN but not from internet**
- Check router port forwarding (Step 2)
- Check DuckDNS is pointing to the right public IP: `curl https://ipinfo.io/ip` vs what duckdns.org shows

**certbot renew fails**
- Check the DuckDNS token: `cat /etc/letsencrypt/duckdns-token`
- Run manually with verbose: `sudo certbot renew --dry-run -v`
- Check DuckDNS API response: run the auth hook script manually

**Shiny app loads but is blank / spins forever**
- Check Shiny container logs: `docker compose logs -f shiny_pool1`
- Check DB connectivity from container: `docker compose run --rm shiny_pool1 Rscript -e "DBI::dbConnect(...)"`
