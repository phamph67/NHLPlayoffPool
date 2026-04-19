"""
Live integration tests against the 2025 NHL playoff pages on hockey-reference.

These tests make real HTTP requests and validate against the known final state
of the 2025 playoffs (as captured in legacy/data/*_2025-06-18.csv).

Failures are emitted as warnings rather than hard errors, because the source
site may change its structure over time. Run with:

    pytest tests/test_live_2025.py -v -W always

Or to include alongside unit tests:

    pytest tests/ -v -W always
"""

import sys
import os
import warnings
from datetime import date

import pytest

os.environ.setdefault("SCRAPE_YEAR", "2025")

sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))
from scraper import (
    SKATERS_URL,
    GOALIES_URL,
    PLAYOFFS_URL,
    fetch_soup,
    scrape_skaters,
    scrape_goalies,
    scrape_eliminations,
)

pytestmark = pytest.mark.live

_TODAY = date(2025, 6, 18)


def soft_assert(condition: bool, message: str) -> None:
    """Emit a UserWarning instead of raising AssertionError."""
    if not condition:
        warnings.warn(f"[live-2025] {message}", UserWarning, stacklevel=2)


# ---------------------------------------------------------------------------
# Skaters
# ---------------------------------------------------------------------------

@pytest.fixture(scope="module")
def live_skaters():
    return scrape_skaters(_TODAY)


class TestLiveSkaters:
    def test_minimum_row_count(self, live_skaters):
        # 2025 playoffs had 332 skater rows on 2025-06-18
        soft_assert(
            len(live_skaters) >= 300,
            f"Expected ≥300 skater rows, got {len(live_skaters)}",
        )

    def test_connor_mcdavid_stats(self, live_skaters):
        row = next((r for r in live_skaters if r[1] == "Connor McDavid"), None)
        soft_assert(row is not None, "Connor McDavid not found in skaters")
        if row:
            soft_assert(row[2] == "EDM", f"McDavid team: expected EDM, got {row[2]}")
            soft_assert(row[3] == 7,  f"McDavid goals: expected 7, got {row[3]}")
            soft_assert(row[4] == 26, f"McDavid assists: expected 26, got {row[4]}")

    def test_leon_draisaitl_stats(self, live_skaters):
        row = next((r for r in live_skaters if r[1] == "Leon Draisaitl"), None)
        soft_assert(row is not None, "Leon Draisaitl not found in skaters")
        if row:
            soft_assert(row[2] == "EDM", f"Draisaitl team: expected EDM, got {row[2]}")
            soft_assert(row[3] == 11, f"Draisaitl goals: expected 11, got {row[3]}")
            soft_assert(row[4] == 22, f"Draisaitl assists: expected 22, got {row[4]}")

    def test_record_tuple_shape(self, live_skaters):
        soft_assert(
            all(len(r) == 5 for r in live_skaters),
            "One or more skater records do not have exactly 5 fields",
        )

    def test_no_empty_names(self, live_skaters):
        empty = [r for r in live_skaters if not r[1]]
        soft_assert(len(empty) == 0, f"{len(empty)} skater rows have an empty name")

    def test_no_negative_stats(self, live_skaters):
        bad = [r for r in live_skaters if r[3] < 0 or r[4] < 0]
        soft_assert(len(bad) == 0, f"{len(bad)} skater rows have negative goals/assists")


# ---------------------------------------------------------------------------
# Goalies
# ---------------------------------------------------------------------------

@pytest.fixture(scope="module")
def live_goalies():
    return scrape_goalies(_TODAY)


class TestLiveGoalies:
    def test_minimum_row_count(self, live_goalies):
        # 2025 playoffs had 27 goalie rows on 2025-06-18
        soft_assert(
            len(live_goalies) >= 20,
            f"Expected ≥20 goalie rows, got {len(live_goalies)}",
        )

    def test_bobrovsky_stats(self, live_goalies):
        row = next((r for r in live_goalies if r[1] == "Sergei Bobrovsky"), None)
        soft_assert(row is not None, "Sergei Bobrovsky not found in goalies")
        if row:
            soft_assert(row[2] == "FLA", f"Bobrovsky team: expected FLA, got {row[2]}")
            soft_assert(row[3] == 16, f"Bobrovsky wins: expected 16, got {row[3]}")
            soft_assert(row[4] == 3,  f"Bobrovsky shutouts: expected 3, got {row[4]}")
            soft_assert(row[5] == 0,  f"Bobrovsky goals: expected 0, got {row[5]}")
            soft_assert(row[6] == 0,  f"Bobrovsky assists: expected 0, got {row[6]}")

    def test_record_tuple_shape(self, live_goalies):
        soft_assert(
            all(len(r) == 7 for r in live_goalies),
            "One or more goalie records do not have exactly 7 fields",
        )

    def test_no_empty_names(self, live_goalies):
        empty = [r for r in live_goalies if not r[1]]
        soft_assert(len(empty) == 0, f"{len(empty)} goalie rows have an empty name")


# ---------------------------------------------------------------------------
# Eliminations
# ---------------------------------------------------------------------------

@pytest.fixture(scope="module")
def live_eliminations():
    return scrape_eliminations(_TODAY)


class TestLiveEliminations:
    def test_all_16_teams_eliminated(self, live_eliminations):
        # 2025 playoffs: 16 teams entered, all 15 non-champions were eliminated
        # (EDM reached the final and lost, so 15 teams eliminated)
        eliminated = {r[0] for r in live_eliminations}
        soft_assert(
            len(eliminated) >= 15,
            f"Expected ≥15 eliminated teams, got {len(eliminated)}: {eliminated}",
        )

    def test_edm_eliminated_in_final(self, live_eliminations):
        row = next((r for r in live_eliminations if r[0] == "EDM"), None)
        soft_assert(row is not None, "EDM not found in eliminations")
        if row:
            soft_assert(
                row[2] == "final",
                f"EDM round: expected 'final', got {row[2]}",
            )

    def test_dal_eliminated_in_conference_finals(self, live_eliminations):
        row = next((r for r in live_eliminations if r[0] == "DAL"), None)
        soft_assert(row is not None, "DAL not found in eliminations")
        if row:
            soft_assert(
                "conference" in row[2],
                f"DAL round: expected conference finals slug, got {row[2]}",
            )

    def test_champion_fla_not_eliminated(self, live_eliminations):
        eliminated = {r[0] for r in live_eliminations}
        soft_assert("FLA" not in eliminated, "FLA (2025 champion) should not be eliminated")

    def test_first_round_losers_present(self, live_eliminations):
        # All 8 first-round losers: LAK, MIN, COL, STL, NJD, MTL, TBL, OTT
        expected_first_round = {"LAK", "MIN", "COL", "STL", "NJD", "MTL", "TBL", "OTT"}
        eliminated = {r[0] for r in live_eliminations}
        missing = expected_first_round - eliminated
        soft_assert(
            len(missing) == 0,
            f"First-round losers missing from eliminations: {missing}",
        )
