#!/usr/bin/env python3
"""
Standalone utility: ingest all legacy/data CSV files into PostgreSQL.

Reads every skaters_*.csv, goalies_*.csv, and playoff_results_*.csv from
legacy/data/ and upserts into the skaters, goalies, and eliminations tables.

Connection is configured via environment variables (or a .env file in the
project root). Override DB_NAME to target a test database, e.g.:

    DB_NAME=nhl_dev python db/ingest_legacy.py

.env format:
    DB_USER=...
    DB_PASSWORD=...
    DB_HOST=localhost
    DB_PORT=5432
    DB_NAME=nhl_dev
"""

import csv
import glob
import os
import sys
from pathlib import Path

import psycopg2
from psycopg2.extras import execute_values
from dotenv import load_dotenv

# Load .env from project root (two levels up from this file)
load_dotenv(Path(__file__).parent.parent / ".env")

LEGACY_DIR = Path(__file__).parent.parent / "legacy" / "data"


def get_conn():
    return psycopg2.connect(
        host=os.environ.get("DB_HOST", "localhost"),
        port=int(os.environ.get("DB_PORT", 5432)),
        dbname=os.environ["DB_NAME"],
        user=os.environ["DB_USER"],
        password=os.environ["DB_PASSWORD"],
    )


def ingest_skaters(conn) -> int:
    files = sorted(LEGACY_DIR.glob("skaters_*.csv"))
    records = []
    for f in files:
        with open(f, newline="", encoding="utf-8") as fh:
            for row in csv.DictReader(fh):
                name = row["Player"].strip()
                team = row["team"].strip()
                date_val = row["date_retrieved"].strip()
                if not name or not team or not date_val:
                    continue
                try:
                    goals   = int(row["goals"]   or 0)
                    assists = int(row["assists"]  or 0)
                except ValueError:
                    continue
                records.append((date_val, name, team, goals, assists))

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
    return len(records)


def ingest_goalies(conn) -> int:
    files = sorted(LEGACY_DIR.glob("goalies_*.csv"))
    records = []
    for f in files:
        with open(f, newline="", encoding="utf-8") as fh:
            for row in csv.DictReader(fh):
                name = row["Player"].strip()
                team = row["team"].strip()
                date_val = row["date_retrieved"].strip()
                if not name or not team or not date_val:
                    continue
                try:
                    wins     = int(row["wins"]     or 0)
                    shutouts = int(row["shutouts"]  or 0)
                    goals    = int(row["goals"]     or 0)
                    assists  = int(row["assists"]   or 0)
                except ValueError:
                    continue
                records.append((date_val, name, team, wins, shutouts, goals, assists))

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
    return len(records)


def ingest_eliminations(conn) -> int:
    # Collect the final (latest-date) state for each eliminated team.
    # Each file is a full snapshot; later files supersede earlier ones.
    files = sorted(LEGACY_DIR.glob("playoff_results_*.csv"))
    # team → (eliminated_date, round_slug): last write wins
    by_team: dict[str, tuple[str, str]] = {}
    for f in files:
        with open(f, newline="", encoding="utf-8") as fh:
            for row in csv.DictReader(fh):
                try:
                    t1_wins = int(row["team1_wins"])
                    t2_wins = int(row["team2_wins"])
                except ValueError:
                    continue
                if t1_wins != 4 and t2_wins != 4:
                    continue  # series not yet complete

                round_slug = row["round"].strip()
                date_val   = row["date"].strip()
                t1 = row["team1_short"].strip()
                t2 = row["team2_short"].strip()
                eliminated = t2 if t1_wins == 4 else t1
                by_team[eliminated] = (date_val, round_slug)

    records = [(team, date_val, round_slug) for team, (date_val, round_slug) in by_team.items()]

    with conn.cursor() as cur:
        execute_values(
            cur,
            """
            INSERT INTO eliminations (team, eliminated_date, round)
            VALUES %s
            ON CONFLICT (team) DO UPDATE
              SET eliminated_date = EXCLUDED.eliminated_date,
                  round           = EXCLUDED.round
            """,
            records,
        )
    conn.commit()
    return len(records)


def main():
    print(f"Connecting to {os.environ.get('DB_NAME')} on {os.environ.get('DB_HOST', 'localhost')}...")
    try:
        conn = get_conn()
    except Exception as e:
        print(f"ERROR: could not connect — {e}", file=sys.stderr)
        sys.exit(1)

    try:
        n = ingest_skaters(conn)
        print(f"  skaters:      {n:,} rows upserted")
        n = ingest_goalies(conn)
        print(f"  goalies:      {n:,} rows upserted")
        n = ingest_eliminations(conn)
        print(f"  eliminations: {n} rows upserted")
    finally:
        conn.close()

    print("Done.")


if __name__ == "__main__":
    main()
