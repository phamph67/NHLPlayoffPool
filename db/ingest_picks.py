#!/usr/bin/env python3
"""
Standalone utility: ingest a prepared picks CSV into PostgreSQL players + picks tables.

Expected CSV columns:
  - name      participant display name
  - pool_id   integer pool identifier (1 or 2)
  - pick columns matching "Pick a (center|winger|defenseman|goalie)..." in order:
      8 centers, 10 wingers, 6 defensemen, 3 goalies

Usage:
    DB_NAME=nhl_dev python db/ingest_picks.py legacy/data/pool1_picks.csv
    DB_NAME=nhl_dev python db/ingest_picks.py legacy/data/pool2_picks.csv
"""

import csv
import os
import re
import sys
from pathlib import Path

import psycopg2
from psycopg2.extras import execute_values
from dotenv import load_dotenv

load_dotenv(Path(__file__).parent.parent / ".env")

PLAYER_KEY_CSV = Path(__file__).parent / "hockey_player_key.csv"

SLOTS = (
    [f"center_{i}"     for i in range(1, 9)]   +  # 8 centers
    [f"winger_{i}"     for i in range(1, 11)]  +  # 10 wingers
    [f"defenseman_{i}" for i in range(1, 7)]   +  # 6 defensemen
    [f"goalie_{i}"     for i in range(1, 4)]      # 3 goalies
)


def normalize(s: str) -> str:
    """Collapse internal whitespace and strip."""
    return re.sub(r"\s+", " ", s).strip()


def load_player_key() -> dict[str, tuple[str, str]]:
    """Return {name_key: (name_ref, position_type)} from hockey_player_key.csv."""
    lookup: dict[str, tuple[str, str]] = {}
    with open(PLAYER_KEY_CSV, newline="", encoding="utf-8-sig") as fh:
        for row in csv.DictReader(fh):
            key = normalize(row["Form in Google Forms"])
            ref = row["Form in hockey reference"].strip()
            pos = "goalie" if "goalies" in row["HTML"] else "skater"
            lookup[key] = (ref, pos)
    return lookup


def get_conn():
    return psycopg2.connect(
        host=os.environ.get("DB_HOST", "localhost"),
        port=int(os.environ.get("DB_PORT", 5432)),
        dbname=os.environ["DB_NAME"],
        user=os.environ["DB_USER"],
        password=os.environ["DB_PASSWORD"],
    )


def ingest_picks(conn, picks_csv: Path, player_lookup: dict) -> tuple[int, int]:
    """Returns (players_upserted, picks_upserted)."""
    player_records: list[tuple] = []
    pick_records:   list[tuple] = []
    unknown: set[str] = set()

    with open(picks_csv, newline="", encoding="utf-8-sig") as fh:
        reader = csv.DictReader(fh)

        pick_cols = [
            c for c in reader.fieldnames
            if re.search(r"Pick a (center|winger|defenseman|goalie)", c, re.IGNORECASE)
        ]
        if len(pick_cols) != len(SLOTS):
            raise ValueError(
                f"Expected {len(SLOTS)} pick columns, found {len(pick_cols)}"
            )

        for row in reader:
            participant = normalize(row["name"])
            pool_id = int(row["pool_id"])
            if not participant:
                continue

            for col, slot in zip(pick_cols, SLOTS):
                raw = normalize(row[col])
                if not raw or raw.upper() == "NA":
                    continue
                if raw not in player_lookup:
                    unknown.add(raw)
                    continue

                name_ref, pos_type = player_lookup[raw]
                player_records.append((raw, name_ref, pos_type))
                pick_records.append((participant, pool_id, raw, slot))

    if unknown:
        print(f"  WARNING: {len(unknown)} pick value(s) not found in player key — skipped:")
        for k in sorted(unknown):
            print(f"    {k!r}")

    with conn.cursor() as cur:
        execute_values(
            cur,
            """
            INSERT INTO players (name_key, name_ref, position_type)
            VALUES %s
            ON CONFLICT (name_key) DO UPDATE
              SET name_ref      = EXCLUDED.name_ref,
                  position_type = EXCLUDED.position_type
            """,
            list({r[0]: r for r in player_records}.values()),
        )
        n_players = cur.rowcount

        execute_values(
            cur,
            """
            INSERT INTO picks (participant, pool_id, player_key, position_slot)
            VALUES %s
            ON CONFLICT (participant, pool_id, position_slot) DO UPDATE
              SET player_key = EXCLUDED.player_key
            """,
            pick_records,
        )
        n_picks = cur.rowcount

    conn.commit()
    return n_players, n_picks


def main():
    if len(sys.argv) != 2:
        print("Usage: python db/ingest_picks.py <picks_csv>", file=sys.stderr)
        sys.exit(1)

    picks_csv = Path(sys.argv[1])
    if not picks_csv.exists():
        print(f"ERROR: file not found: {picks_csv}", file=sys.stderr)
        sys.exit(1)

    print(f"Connecting to {os.environ.get('DB_NAME')} on {os.environ.get('DB_HOST', 'localhost')}...")
    try:
        conn = get_conn()
    except Exception as e:
        print(f"ERROR: could not connect — {e}", file=sys.stderr)
        sys.exit(1)

    try:
        player_lookup = load_player_key()
        print(f"  Loaded {len(player_lookup)} entries from hockey_player_key.csv")
        n_players, n_picks = ingest_picks(conn, picks_csv, player_lookup)
        print(f"  players: {n_players} rows upserted")
        print(f"  picks:   {n_picks} rows upserted")
    finally:
        conn.close()

    print("Done.")


if __name__ == "__main__":
    main()
