# 🏒 NHLPlayoffPool

A data pipeline and visualization project for running a friends-league NHL playoff pool. Participants submit picks via Google Form before the playoffs begin. Stats are scraped nightly and stored in a local PostgreSQL database, and cumulative standings are served through an R Shiny dashboard hosted at home.

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

- Points are **cumulative and never deducted** — a player's historical points are always retained.
- Once a player's NHL team is **eliminated from the playoffs**, that player stops producing points on the next nightly update.
- Players on active teams continue to accumulate until their team is eliminated or the playoffs end.

> **Example:** If a participant picks a player whose team exits in Round 1, those Round 1 points are kept — but no further points are added from Round 2 onward.

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
                                                   eliminations table
                                                   (team → round_eliminated)
                                                        │
                                                        ▼
                                               R Shiny Dashboard
                                               (score calc at query time)
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

*Detailed setup instructions to follow once the refactor is complete.*
