#!/usr/bin/env python3
"""
Container startup checks. Run once before cron starts.
Exits non-zero if critical checks fail so the container surfaces as unhealthy.

Checks:
  1. DB connection — hard fail (no point running without a working DB)
  2. hockey-reference URL reachability — hard fail on network error,
     warn on HTTP 404 (playoffs pages may not exist yet early in the season)
"""

import os
import sys
import time

import psycopg2
import requests

HEADERS = {
    "User-Agent": (
        "Mozilla/5.0 (Windows NT 10.0; Win64; x64) "
        "AppleWebKit/537.36 (KHTML, like Gecko) "
        "Chrome/123.0.0.0 Safari/537.36"
    )
}

RETRIES    = 3
RETRY_WAIT = 5   # seconds between DB retries


def check_db() -> bool:
    url = os.environ.get("DATABASE_URL", "")
    if not url:
        print("STARTUP [DB]  FAIL — DATABASE_URL not set")
        return False

    for attempt in range(1, RETRIES + 1):
        try:
            conn = psycopg2.connect(url)
            conn.cursor().execute("SELECT 1")
            conn.close()
            print("STARTUP [DB]  OK")
            return True
        except Exception as e:
            print(f"STARTUP [DB]  attempt {attempt}/{RETRIES} failed — {e}")
            if attempt < RETRIES:
                time.sleep(RETRY_WAIT)

    print("STARTUP [DB]  FAIL — could not connect after all retries")
    return False


def check_urls() -> bool:
    year = os.environ.get("SCRAPE_YEAR", "2025")
    urls = [
        f"https://www.hockey-reference.com/playoffs/NHL_{year}_skaters.html",
        f"https://www.hockey-reference.com/playoffs/NHL_{year}_goalies.html",
        f"https://www.hockey-reference.com/playoffs/NHL_{year}.html",
    ]

    all_ok = True
    for url in urls:
        try:
            resp = requests.get(url, headers=HEADERS, timeout=15)
            if resp.status_code == 200:
                print(f"STARTUP [URL] OK   {url}")
            elif resp.status_code == 404:
                # Pages don't exist until games are played — warn, don't fail
                print(f"STARTUP [URL] WARN {url} — HTTP 404 (no data yet for {year}?)")
            else:
                print(f"STARTUP [URL] FAIL {url} — HTTP {resp.status_code}")
                all_ok = False
        except Exception as e:
            print(f"STARTUP [URL] FAIL {url} — {e}")
            all_ok = False

    return all_ok


if __name__ == "__main__":
    db_ok  = check_db()
    url_ok = check_urls()

    if not db_ok:
        print("STARTUP FAILED — database unreachable")
        sys.exit(1)

    if not url_ok:
        print("STARTUP FAILED — hockey-reference unreachable (network issue?)")
        sys.exit(1)

    print("STARTUP checks passed — starting cron")
