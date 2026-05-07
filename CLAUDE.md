# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## What This Project Is

An NHL playoff pool tracker. Participants submit picks via Google Form before the playoffs. A nightly Python scraper pulls stats from hockey-reference into PostgreSQL, and two R Shiny containers serve a cumulative standings graph — one per workplace pool group.

Hosted on a Windows 11 home server running WSL2. The PostgreSQL instance runs directly on WSL (not in a container). Docker containers reach it via `host.docker.internal`.

Full architecture and design decisions: `docs/architecture.md`

## Credentials

Stored in `.env` (gitignored). Check `.env` for the actual values before running any DB commands. The format is:

```
DB_USER=...
DB_PASSWORD=...
DB_HOST=host.docker.internal
DB_PORT=5432
DB_NAME=nhl
```

## Commands

### Tests
```bash
# Python — inside the scraper container
docker compose run --rm scraper python -m pytest tests/ -v

# R — inside the shiny container
docker compose run --rm shiny_pool1 Rscript -e "testthat::test_file('/srv/shiny-server/tests/testthat/test_scoring.R')"
```

### Docker
```bash
docker compose up --build          # build and start all services
docker compose up --build scraper  # rebuild one service after code changes
docker compose logs -f shiny_pool1 # tail logs for a service
```

See `docs/docker-guide.md` for full usage including one-off commands and troubleshooting.

### Database
```bash
# Credentials from .env
PGPASSWORD=<DB_PASSWORD> psql -h localhost -p 5432 -U <DB_USER> -d <DB_NAME>

# Re-run schema creation (safe — all CREATE TABLE IF NOT EXISTS)
PGPASSWORD=<DB_PASSWORD> psql -h localhost -p 5432 -U <DB_USER> -d <DB_NAME> -f db/init.sql
```

See `docs/database.md` for schema reference and useful diagnostic queries.

## Architecture

### Pipeline
```
Google Form → picks table (seeded once)
hockey-reference → scraper.py (21:00 EST cron) → skaters / goalies / eliminations tables
PostgreSQL → shiny/app.R (at render time) → cumulative line graph
```

### Key points
- Two Shiny containers, same image, differ only by `POOL_ID` env var (1 or 2) → ports 3838 / 3839
- Scoring lives in `shiny/scoring.R` (`compute_scores()`), applied at render time, never stored
- Scoring: skaters = `goals + assists`; goalies = `goals + assists + wins×2 + shutouts×3`
- Eliminated players' stats freeze on hockey-reference at their final total — scoring ignores the eliminations table entirely and scores from cumulative stats only (scores never drop, they plateau)
- `db/hockey_player_key.csv` is the name-mapping seed file (form names ↔ hockey-reference names)

Scraper quirks (duplicate columns, sub-headers, encoding): `docs/scraper.md`

## Legacy Code

`legacy/` contains the original R-only implementation and historical CSV data. Not part of the active pipeline, kept for reference only.
