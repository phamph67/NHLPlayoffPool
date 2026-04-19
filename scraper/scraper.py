#!/usr/bin/env python3
"""
NHL Playoff Pool scraper.
Scrapes skaters, goalies, and playoff results from hockey-reference.
Upserts to PostgreSQL.
Cron: 21:00 EST (02:00 UTC) daily.
"""

import os
import re
import logging
from datetime import date

import requests
from bs4 import BeautifulSoup
import psycopg2
from psycopg2.extras import execute_values

logging.basicConfig(
    level=logging.INFO,
    format="%(asctime)s %(levelname)s %(message)s",
)
log = logging.getLogger(__name__)

HEADERS = {
    "User-Agent": (
        "Mozilla/5.0 (Windows NT 10.0; Win64; x64) "
        "AppleWebKit/537.36 (KHTML, like Gecko) "
        "Chrome/123.0.0.0 Safari/537.36"
    )
}

_YEAR        = os.environ.get("SCRAPE_YEAR", "2025")
SKATERS_URL  = f"https://www.hockey-reference.com/playoffs/NHL_{_YEAR}_skaters.html"
GOALIES_URL  = f"https://www.hockey-reference.com/playoffs/NHL_{_YEAR}_goalies.html"
PLAYOFFS_URL = f"https://www.hockey-reference.com/playoffs/NHL_{_YEAR}.html"

# Maps partial team name fragment → 3-letter abbreviation.
# Must cover both full city/region names (used in team1) and
# nickname variants (used in team2 after " over / lead ").
TEAM_NAME_MAP = {
    "Edmonton":      "EDM",
    "Los Angeles":   "LAK",
    "Vegas":         "VGK",
    "Minnesota":     "MIN",
    "Dallas":        "DAL",
    "Colorado":      "COL",
    "Winnipeg":      "WPG",
    "St. Louis":     "STL",
    "Carolina":      "CAR",
    "New Jersey":    "NJD",
    "Washington":    "WSH",
    "Montreal":      "MTL",
    "Florida":       "FLA",
    "Tampa Bay":     "TBL",
    "Toronto":       "TOR",
    "Ottawa":        "OTT",
    # Nickname variants
    "Kings":         "LAK",
    "Oilers":        "EDM",
    "Golden Knights":"VGK",
    "Wild":          "MIN",
    "Stars":         "DAL",
    "Avalanche":     "COL",
    "Jets":          "WPG",
    "Blues":         "STL",
    "Hurricanes":    "CAR",
    "Devils":        "NJD",
    "Capitals":      "WSH",
    "Canadiens":     "MTL",
    "Panthers":      "FLA",
    "Lightning":     "TBL",
    "Maple Leafs":   "TOR",
    "Senators":      "OTT",
}


def resolve_team(name: str) -> str | None:
    """Return 3-letter abbreviation for a full or partial team name."""
    for fragment, abbr in TEAM_NAME_MAP.items():
        if fragment in name:
            return abbr
    return None


def get_db():
    """Return a psycopg2 connection using the DATABASE_URL env var."""
    return psycopg2.connect(os.environ["DATABASE_URL"])


def fetch_soup(url: str) -> BeautifulSoup:
    resp = requests.get(url, headers=HEADERS, timeout=30)
    resp.raise_for_status()
    return BeautifulSoup(resp.text, "lxml")


def parse_stats_table(soup: BeautifulSoup) -> list[dict]:
    """
    Parse a hockey-reference #stats table into a list of dicts.

    Hockey-reference quirks handled:
    - Repeated sub-header rows (tr.thead class) are skipped.
    - Rows where the first cell equals 'Rk' are sub-headers and are skipped.
    - Duplicate column names are suffixed with _1, _2, ... to avoid key collisions
      (e.g. the skaters table has two 'EV', 'PP', 'SH' columns).
    """
    table = soup.find("table", id="stats")
    if table is None:
        raise ValueError("Could not find table#stats on page")

    thead = table.find("thead")
    raw_headers = [th.get_text(strip=True) for th in thead.find_all("th")]

    # Deduplicate column names
    seen: dict[str, int] = {}
    headers: list[str] = []
    for h in raw_headers:
        if h in seen:
            seen[h] += 1
            headers.append(f"{h}_{seen[h]}")
        else:
            seen[h] = 0
            headers.append(h)

    rows = []
    for tr in table.find("tbody").find_all("tr"):
        if "thead" in tr.get("class", []):
            continue
        cells = [td.get_text(strip=True) for td in tr.find_all(["td", "th"])]
        if not cells or cells[0] == "Rk":
            continue
        if len(cells) < len(headers):
            cells += [""] * (len(headers) - len(cells))
        rows.append(dict(zip(headers, cells)))

    return rows


def scrape_skaters(today: date) -> list[tuple]:
    """Returns list of (scrape_date, name_ref, team, goals, assists)."""
    log.info("Scraping skaters")
    soup = fetch_soup(SKATERS_URL)
    rows = parse_stats_table(soup)

    records = []
    for row in rows:
        name = row.get("Player", "").strip()
        team = row.get("Tm", "").strip()
        if not name or not team:
            continue
        try:
            goals   = int(row.get("G",  0) or 0)
            assists = int(row.get("A",  0) or 0)
        except ValueError:
            continue
        records.append((today, name, team, goals, assists))

    log.info("Scraped %d skater rows", len(records))
    return records


def scrape_goalies(today: date) -> list[tuple]:
    """Returns list of (scrape_date, name_ref, team, wins, shutouts, goals, assists)."""
    log.info("Scraping goalies")
    soup = fetch_soup(GOALIES_URL)
    rows = parse_stats_table(soup)

    records = []
    for row in rows:
        name = row.get("Player", "").strip()
        team = row.get("Tm", "").strip()
        if not name or not team:
            continue
        try:
            wins     = int(row.get("W",  0) or 0)
            shutouts = int(row.get("SO", 0) or 0)
            goals    = int(row.get("G",  0) or 0)
            assists  = int(row.get("A",  0) or 0)
        except ValueError:
            continue
        records.append((today, name, team, wins, shutouts, goals, assists))

    log.info("Scraped %d goalie rows", len(records))
    return records


def scrape_eliminations(today: date) -> list[tuple]:
    """
    Parses the playoff bracket page to derive eliminated teams.

    The bracket table rows look like:
      X1 (round)  | X2 (score) | X3 (series description)
      Final       | 4-2        | Florida Panthers over Edmonton Oilers EDM

    A series is complete when one side has exactly 4 wins.
    The loser is written to the eliminations table.

    Returns list of (team_abbr, eliminated_date, round_slug).
    """
    log.info("Scraping playoff eliminations")
    soup = fetch_soup(PLAYOFFS_URL)

    first_table = soup.find("table")
    if first_table is None:
        raise ValueError("Could not find bracket table on playoffs page")

    records = []
    for tr in first_table.find_all("tr"):
        cells = [td.get_text(strip=True) for td in tr.find_all(["td", "th"])]
        if len(cells) < 3:
            continue

        round_name, score_str, series_str = cells[0], cells[1], cells[2]

        if not re.search(r"Round|Final", round_name, re.IGNORECASE):
            continue

        # Score must be present and one side at 4 wins
        score_match = re.match(r"(\d+)-(\d+)", score_str)
        if not score_match:
            continue
        team1_wins = int(score_match.group(1))
        team2_wins = int(score_match.group(2))
        if team1_wins != 4 and team2_wins != 4:
            continue

        # Parse "{team1_full} over|lead {team2_full} XXX"
        parts = re.split(r"\s+(?:over|lead)\s+", series_str, maxsplit=1)
        if len(parts) != 2:
            log.warning("Could not split series string: %r", series_str)
            continue
        team1_full, remainder = parts
        # Strip trailing 3-letter abbreviation appended by hockey-reference
        team2_full = re.sub(r"\s+[A-Z]{3}$", "", remainder).strip()

        team1_abbr = resolve_team(team1_full)
        team2_abbr = resolve_team(team2_full)
        if not team1_abbr or not team2_abbr:
            log.warning(
                "Could not resolve team abbreviations: %r / %r",
                team1_full,
                team2_full,
            )
            continue

        eliminated = team2_abbr if team1_wins == 4 else team1_abbr
        round_slug = re.sub(r"\s+", "_", round_name.strip().lower())
        records.append((eliminated, today, round_slug))

    log.info("Derived %d elimination records", len(records))
    return records


# ---------------------------------------------------------------------------
# DB upserts
# ---------------------------------------------------------------------------

def upsert_skaters(conn, records: list[tuple]) -> None:
    with conn.cursor() as cur:
        execute_values(
            cur,
            """
            INSERT INTO skaters (scrape_date, name_ref, team, goals, assists)
            VALUES %s
            ON CONFLICT (scrape_date, name_ref) DO UPDATE
              SET team    = EXCLUDED.team,
                  goals   = EXCLUDED.goals,
                  assists = EXCLUDED.assists
            """,
            records,
        )
    conn.commit()
    log.info("Upserted %d skater rows", len(records))


def upsert_goalies(conn, records: list[tuple]) -> None:
    with conn.cursor() as cur:
        execute_values(
            cur,
            """
            INSERT INTO goalies (scrape_date, name_ref, team, wins, shutouts, goals, assists)
            VALUES %s
            ON CONFLICT (scrape_date, name_ref) DO UPDATE
              SET team     = EXCLUDED.team,
                  wins     = EXCLUDED.wins,
                  shutouts = EXCLUDED.shutouts,
                  goals    = EXCLUDED.goals,
                  assists  = EXCLUDED.assists
            """,
            records,
        )
    conn.commit()
    log.info("Upserted %d goalie rows", len(records))


def upsert_eliminations(conn, records: list[tuple]) -> None:
    if not records:
        return
    with conn.cursor() as cur:
        execute_values(
            cur,
            """
            INSERT INTO eliminations (team, eliminated_date, round)
            VALUES %s
            ON CONFLICT (team) DO NOTHING
            """,
            records,
        )
    conn.commit()
    log.info("Upserted %d elimination records", len(records))


def main():
    today = date.today()
    log.info("Starting scrape for %s", today)

    skater_records     = scrape_skaters(today)
    goalie_records     = scrape_goalies(today)
    elimination_records = scrape_eliminations(today)

    conn = get_db()
    try:
        upsert_skaters(conn, skater_records)
        upsert_goalies(conn, goalie_records)
        upsert_eliminations(conn, elimination_records)
    finally:
        conn.close()

    log.info("Scrape complete")


if __name__ == "__main__":
    main()
