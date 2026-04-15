# Infrastructure

## Overview

| Layer | Technology | Where it runs |
|-------|-----------|---------------|
| Database | PostgreSQL 16 | WSL (Ubuntu 24.04), directly on host |
| Scraper | Python 3.12 + cron | Docker container (WSL) |
| Dashboards | R Shiny (×2) | Docker containers (WSL) |
| Host machine | Windows 11 | Always-on home server |
| External access | DuckDNS (planned) | Dynamic DNS → home IP |

---

## PostgreSQL on WSL

Postgres runs **directly on WSL**, not in a container. This simplifies persistence (no volume management) and makes it accessible from both the WSL terminal and the Windows host.

**Config files:**
```
/etc/postgresql/16/main/postgresql.conf   # listen_addresses, port
/etc/postgresql/16/main/pg_hba.conf       # authentication rules
```

**Key config changes made:**

`postgresql.conf` — set to listen on all interfaces so Docker containers can reach it:
```
listen_addresses = '*'
```

`pg_hba.conf` — allow Docker bridge subnet to authenticate:
```
host    all    all    172.17.0.0/16    scram-sha-256
```

After any config change: `sudo systemctl reload postgresql`

---

## Docker and WSL networking

Containers cannot use `localhost` to reach the WSL host — inside a container, `localhost` refers to the container itself. Instead, `host.docker.internal` is used, which Docker resolves to the host machine's Docker bridge gateway IP (typically `172.17.0.1`).

On Linux/WSL2 this hostname is not set automatically (unlike Docker Desktop on Mac/Windows), so it is injected explicitly in `docker-compose.yml`:

```yaml
extra_hosts:
  - "host.docker.internal:host-gateway"
```

This is why `postgresql.conf` must listen on `*` (not just `localhost`) and `pg_hba.conf` must allow the `172.17.0.0/16` subnet.

---

## Docker Compose

`docker-compose.yml` defines three services. There is no `db` service — the existing WSL Postgres instance is used directly.

| Service | Image | Port | Key env vars |
|---------|-------|------|-------------|
| `scraper` | built from `scraper/` | none | `DATABASE_URL` |
| `shiny_pool1` | built from `shiny/` | 3838 | `DATABASE_URL`, `POOL_ID=1` |
| `shiny_pool2` | built from `shiny/` | 3839 | `DATABASE_URL`, `POOL_ID=2` |

Credentials are stored in `.env` (gitignored) and interpolated by Compose:

```
# .env
DB_USER=YOUR-USERNAME
DB_PASSWORD=YOUR-PASSWORD
DB_HOST=host.docker.internal
DB_PORT=5432
DB_NAME=nhl
```

---

## Common operations

```bash
# Start dashboards
docker compose up shiny_pool1 shiny_pool2

# Start everything (dashboards + scraper cron)
docker compose up

# Rebuild after code changes
docker compose up --build

# Tail scraper cron logs
docker compose logs -f scraper

# Run scraper immediately (one-off, outside cron schedule)
docker compose run --rm scraper python scraper.py

# Connect to DB
PGPASSWORD=YOUR-PASSWORD psql -h localhost -p 5432 -U YOUR-USERNAME -d nhl
```

---

## Adding a new season

1. Update the three hockey-reference URLs in `scraper/scraper.py`
2. Truncate or drop and recreate the stats tables (skaters, goalies, eliminations)
3. Re-seed the `players` table from the new `db/hockey_player_key.csv`
4. Load new `picks` from the Google Form export
5. Rebuild containers: `docker compose up --build`
