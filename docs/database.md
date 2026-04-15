# Database

**Instance:** PostgreSQL 16, running on WSL at `localhost:5432`
**Database name:** `nhl`
**Schema:** defined in `db/init.sql`, applied once manually (or on first container boot if using the db service)

---

## Tables

### `players`

The name mapping table. Bridges the two naming conventions used across the system.

| Column | Type | Description |
|--------|------|-------------|
| `name_key` | TEXT (PK) | Name as submitted in the Google Form, e.g. `AARON EKBLAD FLA` |
| `name_ref` | TEXT | Name as it appears on hockey-reference, e.g. `Aaron Ekblad` |
| `position_type` | TEXT | `skater` or `goalie` |

**Seeding:** Load from `db/hockey_player_key.csv` before the season. This file maps every player who could appear on a pick sheet to their hockey-reference equivalent.

---

### `picks`

One row per participant × position slot × pool. Loaded once from the Google Form export before the first game. Not updated during the season.

| Column | Type | Description |
|--------|------|-------------|
| `participant` | TEXT | Display name of the pool participant |
| `pool_id` | INT | 1 or 2 — which workplace group this pick belongs to |
| `player_key` | TEXT (FK → players.name_key) | The form-format player name |
| `position_slot` | TEXT | e.g. `center_1`, `winger_3`, `goalie_2` |

**Unique constraint:** `(participant, pool_id, position_slot)` — one player per slot per participant.

**Position slots available:**
- Skaters: `center_1` through `center_8`, `winger_1` through `winger_10`, `defenseman_1` through `defenseman_6`
- Goalies: `goalie_1` through `goalie_3`

---

### `skaters`

Daily cumulative stats snapshot for all playoff skaters. One row per player per scrape date.

| Column | Type | Description |
|--------|------|-------------|
| `scrape_date` | DATE (PK) | Date the row was scraped |
| `name_ref` | TEXT (PK) | Hockey-reference player name |
| `team` | TEXT | 3-letter team abbreviation, e.g. `EDM` |
| `goals` | INT | Cumulative goals in the playoffs to date |
| `assists` | INT | Cumulative assists in the playoffs to date |

**Primary key:** `(scrape_date, name_ref)` — upserted each night, safe to re-run.

---

### `goalies`

Daily cumulative stats snapshot for all playoff goalies. Same pattern as `skaters`.

| Column | Type | Description |
|--------|------|-------------|
| `scrape_date` | DATE (PK) | Date the row was scraped |
| `name_ref` | TEXT (PK) | Hockey-reference player name |
| `team` | TEXT | 3-letter team abbreviation |
| `wins` | INT | Cumulative playoff wins to date |
| `shutouts` | INT | Cumulative playoff shutouts to date |
| `goals` | INT | Cumulative goals (rare but tracked) |
| `assists` | INT | Cumulative assists |

---

### `eliminations`

One row per eliminated team. Written when the scraper first detects a completed series. Never updated — the first recorded date is permanent.

| Column | Type | Description |
|--------|------|-------------|
| `team` | TEXT (PK) | 3-letter team abbreviation |
| `eliminated_date` | DATE | Date the scraper first recorded the elimination |
| `round` | TEXT | Round slug, e.g. `first_round`, `conference_finals`, `final` |

**Insert policy:** `ON CONFLICT (team) DO NOTHING` — re-running the scraper after a team is already recorded is safe.

---

## Relationships

```
players.name_key ◄── picks.player_key
players.name_ref ◄── skaters.name_ref  (joined at render time in app.R)
players.name_ref ◄── goalies.name_ref  (joined at render time in app.R)
skaters.team     ──► eliminations.team (joined at render time to apply zero-out rule)
goalies.team     ──► eliminations.team (same)
```

There are no foreign keys enforced between `skaters`/`goalies` and `players` — hockey-reference publishes stats for all players, not just those who were picked. The app filters to picked players via the join through `picks → players → skaters/goalies`.

---

## Connecting

```bash
PGPASSWORD=YOUR-PASSWORD psql -h localhost -p 5432 -U YOUR-USERNAME -d nhl
```

Useful queries:

```sql
-- Check what's been scraped
SELECT scrape_date, COUNT(*) FROM skaters GROUP BY scrape_date ORDER BY scrape_date DESC;

-- Check eliminations recorded so far
SELECT * FROM eliminations ORDER BY eliminated_date;

-- Check picks loaded for pool 1
SELECT participant, COUNT(*) as picks FROM picks WHERE pool_id = 1 GROUP BY participant;
```
