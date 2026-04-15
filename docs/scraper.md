# Scraper

**Location:** `scraper/scraper.py`
**Language:** Python 3.12
**Schedule:** Cron, 02:00 UTC (21:00 EST) daily inside the container

---

## What it scrapes

| URL | Data extracted |
|-----|----------------|
| `hockey-reference.com/playoffs/NHL_2025_skaters.html` | Goals, assists per skater |
| `hockey-reference.com/playoffs/NHL_2025_goalies.html` | Wins, shutouts, goals, assists per goalie |
| `hockey-reference.com/playoffs/NHL_2025.html` | Playoff bracket — used to derive eliminations |

All stats on hockey-reference are **cumulative season totals**, not per-game. Each nightly scrape captures a full snapshot of where every player stands to date.

---

## How it runs

The scraper container runs a standard cron daemon. On container start, `docker-entrypoint.sh` writes the Docker environment variables (including `DATABASE_URL`) to `/app/env.sh`, which the cron job sources before executing:

```
0 2 * * * root . /app/env.sh && python /app/scraper.py >> /var/log/cron.log 2>&1
```

This is necessary because cron does not inherit the container's environment variables by default.

---

## Parsing hockey-reference tables

Hockey-reference stats pages use an HTML `<table id="stats">` structure with a few quirks:

**Sub-header rows:** The `<tbody>` contains repeated header rows (with CSS class `thead`) interspersed throughout the data. These are skipped by checking `tr.get("class", [])`.

**Duplicate column names:** The skaters table has two sets of columns named `EV`, `PP`, and `SH` (even-strength and power-play splits). `parse_stats_table()` deduplicates these by appending `_1` to the second occurrence (e.g. `EV` and `EV_1`). Only `G` (goals) and `A` (assists) are used, so this doesn't affect the data written to the DB — but it prevents dict key collisions during parsing.

**Empty-name rows:** Some rows have a blank `Player` cell (separator rows). `parse_stats_table()` returns these as-is; `scrape_skaters()` and `scrape_goalies()` filter them out with `if not name or not team: continue`.

---

## Deriving eliminations

The playoff bracket page (`NHL_2025.html`) has a summary table where each row looks like:

```
Round name  |  Score  |  Series description
Final       |  4-2    |  Florida Panthers over Edmonton Oilers EDM
```

The scraper identifies completed series (where one side has exactly 4 wins) and determines the loser:
- If `team1_wins == 4` → team2 is eliminated
- If `team2_wins == 4` → team1 is eliminated

Team full names are resolved to 3-letter abbreviations via `TEAM_NAME_MAP` in `scraper.py`. This map covers both city/region names (e.g. `Florida` → `FLA`) and nickname variants (e.g. `Panthers` → `FLA`), since hockey-reference uses both formats in the same sentence.

In-progress series (neither team at 4 wins) are silently skipped.

---

## Database writes

All three upserts are designed to be **idempotent** — running the scraper multiple times on the same day produces the same result:

| Table | Conflict key | On conflict |
|-------|-------------|-------------|
| `skaters` | `(scrape_date, name_ref)` | `DO UPDATE` — overwrites with latest values |
| `goalies` | `(scrape_date, name_ref)` | `DO UPDATE` — overwrites with latest values |
| `eliminations` | `team` | `DO NOTHING` — first recorded date is preserved |

---

## Running manually

```bash
# Against the live DB (from WSL, outside Docker)
DATABASE_URL=postgres://YOUR-USERNAME:YOUR-PASSWORD@localhost:5432/nhl python scraper/scraper.py

# Inside the container
docker compose run --rm scraper python scraper.py

# Run tests
docker compose run --rm scraper python -m pytest tests/ -v
```

---

## Adding a new season

At the start of each NHL playoff season, update the three URLs in `scraper.py` to point to the new year's pages:

```python
SKATERS_URL  = "https://www.hockey-reference.com/playoffs/NHL_2026_skaters.html"
GOALIES_URL  = "https://www.hockey-reference.com/playoffs/NHL_2026_goalies.html"
PLAYOFFS_URL = "https://www.hockey-reference.com/playoffs/NHL_2026.html"
```

Also update `TEAM_NAME_MAP` if any franchises have relocated or been added.
