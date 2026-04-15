# Dashboard

**Location:** `shiny/app.R`, `shiny/scoring.R`
**Language:** R 4.4, Shiny
**Endpoints:** Pool 1 on `:3838`, Pool 2 on `:3839`

---

## Two-pool design

Two identical Shiny containers are run from the same Docker image. The only difference between them is the `POOL_ID` environment variable:

```yaml
shiny_pool1:  POOL_ID=1  →  localhost:3838
shiny_pool2:  POOL_ID=2  →  localhost:3839
```

On startup, `app.R` reads `POOL_ID` and passes it as a parameter to every DB query that touches `picks`. This filters each dashboard to show only the participants from that group.

---

## Scoring rules

Scoring is applied in `scoring.R` (`compute_scores()`), never stored in the database.

| Event | Skaters (F/D) | Goalies |
|-------|:---:|:---:|
| Goal | +1 | +1 |
| Assist | +1 | +1 |
| Win | — | +2 |
| Shutout | — | +3 |

**Elimination rule:** Once a player's team appears in the `eliminations` table with an `eliminated_date` ≤ the current scrape date, that player contributes **0 points** for that date onward. Points accumulated before elimination are kept permanently — they are never deducted.

---

## How `compute_scores()` works

The function in `scoring.R` takes four data frames (picks, skaters, goalies, eliminations) and returns one row per `(participant, date)` with their total score.

For each scrape date `d`:

1. Score each skater row: `goals + assists`
2. Score each goalie row: `goals + assists + wins×2 + shutouts×3`
3. Join picks → day's stats (left join, so missing players score 0)
4. Join → eliminations on `team`
5. Apply `effective_score`: if `eliminated_date <= d`, score becomes 0
6. Sum `effective_score` per participant → one total for that day

The result across all dates forms the time series plotted in the graph.

---

## The graph

The dashboard renders a single `ggplot2` cumulative line graph per pool:

- **X axis:** scrape date
- **Y axis:** cumulative pool score
- **One line per participant**, coloured with `colorspace::qualitative_hcl()`
- **End labels:** participant name and current score, connected to their line by a short leader segment, spread vertically to avoid overlap
- **Legend:** hidden (labels on the lines serve this purpose)

The y-positions of the end labels are spread evenly between `min(score)` and `max(score)` to prevent overlapping text, ordered by descending final score.

---

## Database connection

`app.R` calls `parse_db_url()` from `scoring.R` to parse the `DATABASE_URL` environment variable into individual connection parameters, then uses `RPostgres::Postgres()` to connect. A new connection is opened on each render request and closed via `on.exit(dbDisconnect(conn))`.

The DB is reached at `host.docker.internal:5432` — a hostname Docker resolves to the WSL host machine via the `extra_hosts: host.docker.internal:host-gateway` entry in `docker-compose.yml`.

---

## No-data state

If `picks` is empty or both `skaters` and `goalies` are empty, the app renders a plain text placeholder ("No data yet — check back after the first scrape") instead of crashing.

---

## Running locally for development

The Shiny app can be run outside Docker by setting the environment variables and calling `shiny::runApp()`:

```r
Sys.setenv(
  DATABASE_URL = "postgres://YOUR-USERNAME:YOUR-PASSWORD@localhost:5432/nhl",
  POOL_ID      = "1"
)
shiny::runApp("shiny/")
```

The scoring logic can be tested independently (no Shiny or DB required):

```bash
docker compose run --rm shiny_pool1 Rscript -e \
  "testthat::test_file('/srv/shiny-server/tests/testthat/test_scoring.R')"
```
