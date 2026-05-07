# 🏒 NHLPlayoffPool

A data pipeline and visualization project for running a friends-league NHL playoff pool. Participants submit picks via Google Form before the playoffs begin. Stats are scraped nightly and stored in a local PostgreSQL database, and cumulative standings are served through an R Shiny dashboard hosted at home.

---

## Quick Reference

| I want to… | Go here |
|---|---|
| Understand the system end-to-end | [How It Works](#how-it-works) · [Architecture](#architecture) · [docs/architecture.md](docs/architecture.md) |
| Set up the project for the first time | [Setup](#setup) |
| **Load picks for the new season** | [Ingesting Picks](#ingesting-picks-per-season--production-workflow) |
| Switch from dev (`nhl_dev`) to prod (`nhl`) | [Switching from Dev to Production](#switching-from-dev-to-production) |
| Start / rebuild Docker containers | [Starting the stack](#starting-the-stack) |
| Verify the nightly scraper is running | [Verifying the cron schedule](#verifying-the-cron-schedule) |
| Check scraper logs or run it manually | [Nightly Stats Scrape](#nightly-stats-scrape) |
| Troubleshoot the site being unreachable | [docs/network-setup.md — Troubleshooting](docs/network-setup.md#troubleshooting) |
| Renew or check the SSL certificate | [docs/network-setup.md — Step 5](docs/network-setup.md#5-automatic-certificate-renewal) |
| Add or change the pool password | [docs/network-setup.md — .htpasswd](docs/network-setup.md#7-http-basic-auth-htpasswd) |
| Understand the DB schema | [docs/database.md](docs/database.md) |
| Debug Docker containers | [docs/docker-guide.md](docs/docker-guide.md) |
| Understand scraper quirks | [docs/scraper.md](docs/scraper.md) |

---

## How It Works

### 1. Picks Collection

Before the playoffs begin, participants submit their roster via a **Google Form**. Each entry includes:

- Participant name
- Selected skaters (forwards & defensemen) — one per category slot
- Selected goalie(s)

Form responses are exported and loaded into the database once, prior to the first game.

### 2. Nightly Stats Scrape

A scraper runs daily at **21:00 EST** and pulls current playoff statistics from:

- [hockey-reference.com/playoffs](https://www.hockey-reference.com/playoffs) (web scrape), or
- The unofficial NHL API — see [this reference](https://github.com/Zmalski/NHL-API-Reference?tab=readme-ov-file#players)

Each run inserts a dated snapshot into PostgreSQL. The database is a **mirror of published NHL stats** — no derived values are stored, only raw game data.

### 3. Score Calculation

Pool standings are calculated at query time by joining participant picks against the stats mirror and applying the scoring rules below.

### 4. Dashboard

An **R Shiny** application reads from the local PostgreSQL instance and renders standings, player stat trends, and head-to-head comparisons. The dashboard is containerized and exposed externally via **DuckDNS** from a home server (Windows 11 + WSL).

---

## Scoring Rules

| Event   | Skaters (F/D) | Goalies |
|---------|:-------------:|:-------:|
| Goal    | +1 pt         | +1 pt   |
| Assist  | +1 pt         | +1 pt   |
| Win     | —             | +2 pts  |
| Shutout | —             | +3 pts  |

### Elimination Rule

- Points are **cumulative and never deducted** — a player's score never drops.
- Once a player's team is eliminated, their stats on hockey-reference freeze at their final total. Subsequent scrapes continue to show that frozen total, so their contribution to the pool also stays frozen — it just stops growing.
- No special elimination logic is needed in scoring: the scraper still records eliminations, but the Shiny app ignores them entirely and scores directly from the cumulative stats snapshot.

> **Example:** If a participant picks a player whose team exits in Round 1 with 8 points, those 8 points are kept permanently — the line on the graph simply goes flat from that date onward.

---

## Architecture

```
Google Form (picks)
       │
       ▼
  picks table (PostgreSQL)  ◄──────────────────────────┐
                                                        │
  NHL Site / API  ──►  nightly scraper (21:00 EST)  ──►  stats table (dated snapshots)
                                                        │
                                                   eliminations table        (recorded for reference;
                                                   (team → round_eliminated)  not used in scoring)
                                                        │
                                                        ▼
                                               R Shiny Dashboard
                                               (score calc at query time — cumulative stats only)
                                                        │
                                                        ▼
                                            DuckDNS endpoint (home server)
```

### Database Tables (overview)

| Table          | Key Columns                                              | Notes                          |
|----------------|----------------------------------------------------------|--------------------------------|
| `picks`        | participant, player_name, nhl_team, position             | Loaded once before playoffs    |
| `stats`        | scrape_date, player_name, nhl_team, goals, assists, wins, shutouts | One row per player per scrape date |
| `eliminations` | nhl_team, eliminated_date, round                         | Updated manually or via scraper |

---

## Infrastructure

| Component        | Details                              |
|------------------|--------------------------------------|
| Host             | Windows 11 home server (always-on)   |
| Runtime          | WSL (Ubuntu) for scraper + containers |
| Database         | PostgreSQL (local)                   |
| Dashboard        | R Shiny (Dockerized)                 |
| External access  | DuckDNS dynamic DNS → home IP        |

---

## Setup

### Prerequisites

1. PostgreSQL running locally (WSL). Apply the schema once:
   ```bash
   PGPASSWORD=<DB_PASSWORD> psql -h localhost -U <DB_USER> -d <DB_NAME> -f db/init.sql
   ```

2. Python venv at project root with dependencies:
   ```bash
   python3 -m venv venv
   venv/bin/pip install psycopg2-binary python-dotenv
   ```

3. `.env` file in the project root (gitignored — copy from `.env.example`):
   ```
   DB_USER=...
   DB_PASSWORD=...
   DB_HOST=host.docker.internal
   DB_PORT=5432
   DB_NAME=nhl

   PROJECT_DIR=/home/<you>/projects/NHLPlayoffPool_dev
   DUCKDNS_DOMAIN=yourname          # subdomain only, not .duckdns.org
   DUCKDNS_TOKEN=...                # from duckdns.org dashboard
   ```
   PostgreSQL always runs on WSL localhost. Direct `psql` commands (run from WSL) use `-h localhost`. Docker containers cannot use `localhost` (it resolves to the container itself), so `DB_HOST` must be `host.docker.internal` — this is what the `.env` value is for.

---

### Ingesting Picks (per season — production workflow)

#### Step 1 — Prepare the CSVs

Export each pool's Google Form responses as CSV. Before running the ingest, make two edits to each file:

1. **Rename** the `"Your name (this is the name that will show up on the graph):"` column to exactly `name`. This is what each participant typed as their display name on the form.
2. **Add** a `pool_id` column. Set every row to `1` for the pool 1 file, and `2` for pool 2.

Everything else can be left as-is. The script only reads:
- `name` — participant display name
- `pool_id` — pool identifier
- Columns matching `"Pick a forward/defenseman/goalie (group N)..."` — exactly 24 expected (12 forwards, 8 defensemen, 4 goalies)

All other columns (`Timestamp`, `Email Address`, etc.) are ignored.

#### Step 2 — Apply schema to the production database

The `nhl` production database needs the schema created before first use:

```bash
PGPASSWORD=<DB_PASSWORD> psql -h localhost -U <DB_USER> -d nhl -f db/init.sql
```

This is safe to re-run (all `CREATE TABLE IF NOT EXISTS`).

#### Step 3 — Ingest

```bash
DB_NAME=nhl venv/bin/python db/ingest_picks.py path/to/pool1_picks.csv
DB_NAME=nhl venv/bin/python db/ingest_picks.py path/to/pool2_picks.csv
```

Player name resolution uses `db/hockey_player_key.csv` (form names ↔ hockey-reference names). Any pick value not found in that file is printed as a warning and skipped — check the output after each run and update the key file if needed.

> Picks are upserted, so re-running with a corrected file is safe.

---

### Ingesting Historical Data (one-time, dev/testing only)

The `legacy/data/` directory contains dated CSV snapshots of skaters, goalies, and playoff results from the 2025 season. Use this to populate a test database for validating the pipeline end-to-end:

```bash
DB_NAME=nhl_dev venv/bin/python db/ingest_legacy.py
```

This upserts all `skaters_*.csv`, `goalies_*.csv`, and `playoff_results_*.csv` files found in `legacy/data/`. Safe to re-run.

---

### Nightly Stats Scrape

Stats ingestion in production is handled automatically by the scraper container — no manual steps required. The scraper runs at **21:00 EST (02:00 UTC)** via cron and upserts skaters, goalies, and eliminations directly into PostgreSQL.

#### Verifying the cron schedule

```bash
# 1. Confirm cron is PID 1 (the container's main process)
docker compose exec scraper cat /proc/1/cmdline | tr '\0' ' '
# Expected output: cron -f

# 2. Confirm the schedule is installed
docker compose exec scraper cat /etc/cron.d/scraper
# Should show the "0 2 * * *" line

# 3. Check the scrape log — empty means it hasn't fired yet; errors show here too
docker compose exec scraper cat /var/log/cron.log

# 4. Watch the log live (useful when waiting for the 02:00 UTC run)
docker compose exec scraper tail -f /var/log/cron.log
```

Every run appends to `/var/log/cron.log` inside the container. A successful run ends with `Scrape complete`. Errors are logged there too, so this is the first place to look if data stops updating.

#### Running the scraper manually

```bash
docker compose run --rm scraper python scraper.py
```

---

### Switching from Dev to Production

When you're ready to go live with real picks data:

1. **Update `.env`** — change `DB_NAME` from `nhl_dev` to `nhl`:
   ```
   DB_NAME=nhl
   ```

2. **Restart containers** — they pick up the new `.env` on startup:
   ```bash
   docker compose down
   docker compose up --build -d
   ```

That's it. The network configuration (DuckDNS, nginx, SSL, firewall rules) is completely independent of which database the containers connect to — nothing there needs to change.

---

### Running the Dashboard (local dev)

```bash
# Start everything
docker compose up --build
```

In dev, temporarily remove the `nginx` service or comment out the `depends_on` block and
re-add `ports:` to the shiny services to hit them directly at `http://localhost:3838` / `:3839`.

To test against the dev database, set `DB_NAME=nhl_dev` in `.env` before running.

---

### Production Deployment (DuckDNS + HTTPS)

> Full step-by-step instructions, diagrams, and troubleshooting: **[docs/network-setup.md](docs/network-setup.md)**

The stack is exposed externally via `https://phillphamserver.duckdns.org`. The path from internet to app:

```
Internet → Router (ports 80/443 forwarded) → Windows 11 host (10.0.0.244)
  → WSL2 (mirrored networking, same IP) → nginx container → shiny_pool1/2
```

#### One-time setup summary

**1. WSL2 mirrored networking** — add to `C:\Users\<you>\.wslconfig` on Windows, then run `wsl --shutdown` in PowerShell:
```ini
[wsl2]
networkingMode=mirrored
```
This makes WSL share the Windows host IP so Docker ports are immediately reachable on the LAN — no port proxy rules needed.

**2. Router** — forward TCP **80** and **443** to the server's LAN IP.

**3. DuckDNS** — register at [duckdns.org](https://www.duckdns.org), claim a subdomain, and set your token in `.env`. Optionally add a cron job to keep the IP current if your ISP changes it:
```
*/5 * * * * curl -s "https://www.duckdns.org/update?domains=${DUCKDNS_DOMAIN}&token=${DUCKDNS_TOKEN}&ip=" > /tmp/duckdns.log
```

**4. SSL certificate + automated renewal** — install certbot, fill `.env` with `DUCKDNS_TOKEN`, then run once:
```bash
sudo apt install certbot
sudo bash scripts/setup-certbot-renewal.sh
```
This issues the cert via DNS-01 challenge (no port dependency), wires in DuckDNS API hooks so renewal is fully automated, and registers a deploy hook to reload nginx after each renewal. Certbot's built-in systemd timer (`certbot.timer`) handles renewals twice daily — no cron job needed.

**5. Basic auth** — create `nginx/.htpasswd` (gitignored):
```bash
sudo apt install apache2-utils
htpasswd -c nginx/.htpasswd <username>
```

#### Starting the stack

```bash
docker compose up --build -d
```

Apps are accessible at:
- `https://phillphamserver.duckdns.org/pool1/`
- `https://phillphamserver.duckdns.org/pool2/`

---

## Further Reading

### Internal docs
| Doc | What it covers |
|-----|---------------|
| [docs/network-setup.md](docs/network-setup.md) | Full networking walkthrough: WSL2 mirrored mode, DuckDNS, certbot, nginx config, troubleshooting |
| [docs/architecture.md](docs/architecture.md) | System design decisions |
| [docs/database.md](docs/database.md) | Schema reference and diagnostic queries |
| [docs/docker-guide.md](docs/docker-guide.md) | Docker usage, one-off commands, container troubleshooting |
| [docs/scraper.md](docs/scraper.md) | Scraper quirks: duplicate columns, sub-headers, elimination logic |

### External references
| Topic | Link |
|-------|------|
| WSL2 mirrored networking | [Microsoft: WSL networking modes](https://learn.microsoft.com/en-us/windows/wsl/networking) |
| Let's Encrypt / certbot | [certbot.eff.org](https://certbot.eff.org) |
| DuckDNS | [duckdns.org](https://www.duckdns.org) — includes Linux auto-update script |
| nginx reverse proxy | [nginx docs: proxy_pass](https://nginx.org/en/docs/http/ngx_http_proxy_module.html#proxy_pass) |
| Shiny Server behind a proxy | [RStudio: Shiny Server admin guide](https://docs.posit.co/shiny-server/) — WebSocket headers section |
| NHL API reference | [Zmalski/NHL-API-Reference](https://github.com/Zmalski/NHL-API-Reference?tab=readme-ov-file#players) |
