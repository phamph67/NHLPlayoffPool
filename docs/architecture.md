# Architecture Overview

## What the system does

Participants submit NHL playoff picks once, before the playoffs start. A nightly scraper pulls cumulative stats from hockey-reference and stores dated snapshots in PostgreSQL. Two R Shiny dashboards (one per workplace pool group) read from that database at render time and display a cumulative standings graph.

## Components

```
┌─────────────────────────────────────────────────────────────────┐
│  Data Sources                                                   │
│                                                                 │
│  Google Form ──► picks (seeded once, manually)                  │
│  hockey-reference.com ──► scraper (nightly, automated)          │
└───────────────────────────┬─────────────────────────────────────┘
                            │
                            ▼
┌─────────────────────────────────────────────────────────────────┐
│  PostgreSQL (WSL, localhost:5432, database: nhl)                │
│                                                                 │
│  players      name mapping: form name ↔ hockey-reference name   │
│  picks        who picked whom, per pool                         │
│  skaters      daily cumulative stats snapshot                   │
│  goalies      daily cumulative stats snapshot                   │
│  eliminations team → date eliminated → round                    │
└───────────────────────────┬─────────────────────────────────────┘
                            │
                            ▼
┌─────────────────────────────────────────────────────────────────┐
│  Docker (WSL)                                                   │
│                                                                 │
│  scraper      Python container, cron 21:00 EST → writes DB      │
│  shiny_pool1  R Shiny on :3838, POOL_ID=1 → reads DB            │
│  shiny_pool2  R Shiny on :3839, POOL_ID=2 → reads DB            │
└─────────────────────────────────────────────────────────────────┘
```

## Key design decisions

### Stats are stored as dated snapshots, not deltas

Each nightly scrape writes a full row for every player (`scrape_date`, stats). Hockey-reference publishes **cumulative** season totals, so each row already represents totals-to-date. The dashboard plots these directly as the y-axis — no accumulation needed in the app.

This means if the scraper misses a day, that date simply has no row. The graph will have a gap rather than a wrong value.

### Scoring happens at render time, not at storage time

No derived scores are ever written to the database. The Shiny app applies the scoring formula (`scoring.R`) when it renders. This keeps the DB as a clean mirror of published stats, and means the scoring formula can be changed without any data migration.

### Two pools, one codebase

Both Shiny containers are built from the same image. The only difference is the `POOL_ID` environment variable (1 or 2), which filters the `picks` table at query time. Adding a third pool in future requires only a new `docker-compose.yml` service entry.

### Name mapping layer (`players` table)

Hockey-reference uses full proper names (e.g. `Aaron Ekblad`). The Google Form uses a shorthand format (e.g. `AARON EKBLAD FLA`). The `players` table bridges these two namespaces. The scraper writes `name_ref` (hockey-reference names). The picks table stores `player_key` (form names). The Shiny app joins through `players` to match them.

## Data flow on a render request

1. Browser hits `localhost:3838`
2. Shiny server starts `app.R` for `POOL_ID=1`
3. App connects to PostgreSQL via `host.docker.internal:5432`
4. Three queries: `picks` (filtered to pool 1), `skaters`, `goalies`
5. `compute_scores()` in `scoring.R` joins picks against cumulative stats across all dates
6. `ggplot2` renders the cumulative line graph
7. Graph is returned to the browser as a PNG

## Data flow on a nightly scrape

1. Cron fires at 02:00 UTC (21:00 EST) inside the scraper container
2. `scraper.py` fetches three URLs from hockey-reference
3. Skater and goalie rows are upserted by `(scrape_date, name_ref)` — safe to re-run
4. Playoff bracket is parsed to identify newly eliminated teams
5. Eliminations are inserted with `ON CONFLICT DO NOTHING` — the first recorded date is kept permanently
