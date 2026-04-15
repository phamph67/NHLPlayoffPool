"""
Unit tests for scraper.py.
All tests are offline — no network calls are made.
"""

import sys
import os
from datetime import date

import pytest
from bs4 import BeautifulSoup

# Make scraper importable without a DATABASE_URL in the environment
sys.path.insert(0, os.path.join(os.path.dirname(__file__), ".."))
from scraper import resolve_team, parse_stats_table, scrape_skaters, scrape_goalies, scrape_eliminations

# ---------------------------------------------------------------------------
# Fixtures: minimal but structurally accurate HTML for each table type
# ---------------------------------------------------------------------------

SKATERS_HTML = """
<html><body>
<table id="stats">
  <thead>
    <tr>
      <th>Rk</th><th>Player</th><th>Age</th><th>Tm</th><th>Pos</th>
      <th>GP</th><th>G</th><th>A</th><th>PTS</th><th>+/-</th><th>PIM</th>
      <th>EV</th><th>PP</th><th>SH</th><th>GW</th>
      <th>EV</th><th>PP</th><th>SH</th>
      <th>S</th><th>S%</th><th>TOI</th><th>ATOI</th>
      <th>BLK</th><th>HIT</th><th>FOW</th><th>FOL</th><th>FO%</th>
    </tr>
  </thead>
  <tbody>
    <!-- Sub-header row that should be skipped -->
    <tr class="thead">
      <th>Rk</th><th>Player</th><th>Age</th><th>Tm</th><th>Pos</th>
      <th>GP</th><th>G</th><th>A</th><th>PTS</th><th>+/-</th><th>PIM</th>
      <th>EV</th><th>PP</th><th>SH</th><th>GW</th>
      <th>EV</th><th>PP</th><th>SH</th>
      <th>S</th><th>S%</th><th>TOI</th><th>ATOI</th>
      <th>BLK</th><th>HIT</th><th>FOW</th><th>FOL</th><th>FO%</th>
    </tr>
    <tr>
      <td>1</td><td>Connor McDavid</td><td>27</td><td>EDM</td><td>C</td>
      <td>15</td><td>10</td><td>20</td><td>30</td><td>5</td><td>2</td>
      <td>6</td><td>4</td><td>0</td><td>2</td>
      <td>12</td><td>8</td><td>0</td>
      <td>60</td><td>16.7</td><td>345</td><td>23:00</td>
      <td>5</td><td>15</td><td>200</td><td>100</td><td>66.7</td>
    </tr>
    <tr>
      <td>2</td><td>Leon Draisaitl</td><td>28</td><td>EDM</td><td>C</td>
      <td>15</td><td>8</td><td>15</td><td>23</td><td>3</td><td>4</td>
      <td>5</td><td>3</td><td>0</td><td>1</td>
      <td>10</td><td>5</td><td>0</td>
      <td>55</td><td>14.5</td><td>330</td><td>22:00</td>
      <td>3</td><td>10</td><td>150</td><td>80</td><td>65.2</td>
    </tr>
    <!-- Row with empty name — should be skipped -->
    <tr>
      <td>3</td><td></td><td>25</td><td>FLA</td><td>C</td>
      <td>5</td><td>2</td><td>3</td><td>5</td><td>1</td><td>0</td>
      <td>1</td><td>1</td><td>0</td><td>0</td>
      <td>2</td><td>1</td><td>0</td>
      <td>10</td><td>20.0</td><td>100</td><td>20:00</td>
      <td>1</td><td>2</td><td>10</td><td>5</td><td>66.7</td>
    </tr>
  </tbody>
</table>
</body></html>
"""

GOALIES_HTML = """
<html><body>
<table id="stats">
  <thead>
    <tr>
      <th>Rk</th><th>Player</th><th>Age</th><th>Tm</th>
      <th>GP</th><th>GS</th><th>W</th><th>L</th>
      <th>GA</th><th>SA</th><th>SV</th><th>SV%</th><th>GAA</th><th>SO</th>
      <th>GPS</th><th>MIN</th><th>QS</th><th>QS%</th><th>RBS</th>
      <th>GA%-</th><th>GSAA</th><th>G</th><th>A</th><th>PTS</th><th>PIM</th>
    </tr>
  </thead>
  <tbody>
    <tr>
      <td>1</td><td>Sergei Bobrovsky</td><td>36</td><td>FLA</td>
      <td>23</td><td>23</td><td>16</td><td>7</td>
      <td>53</td><td>618</td><td>565</td><td>.914</td><td>2.20</td><td>3</td>
      <td></td><td>1443</td><td>15</td><td>.652</td><td>3</td>
      <td>83</td><td>10.79</td><td>0</td><td>0</td><td>0</td><td>0</td>
    </tr>
    <tr>
      <td>2</td><td>Connor Hellebuyck</td><td>31</td><td>WPG</td>
      <td>6</td><td>6</td><td>2</td><td>4</td>
      <td>20</td><td>180</td><td>160</td><td>.889</td><td>3.10</td><td>1</td>
      <td></td><td>380</td><td>4</td><td>.667</td><td>2</td>
      <td>95</td><td>-1.20</td><td>0</td><td>1</td><td>1</td><td>0</td>
    </tr>
  </tbody>
</table>
</body></html>
"""

# Playoff bracket HTML — two complete series and one in progress
PLAYOFFS_HTML = """
<html><body>
<table>
  <tbody>
    <tr><td>Final</td><td>4-2</td><td>Florida Panthers over Edmonton Oilers EDM</td></tr>
    <tr><td>Eastern Conference Final</td><td>4-1</td><td>Florida Panthers over Carolina Hurricanes CAR</td></tr>
    <tr><td>Second Round</td><td>3-2</td><td>Edmonton Oilers lead Dallas Stars DAL</td></tr>
    <tr><td>First Round</td><td>4-0</td><td>Carolina Hurricanes over New Jersey Devils NJD</td></tr>
    <!-- Non-bracket row — should be skipped -->
    <tr><td>League</td><td></td><td>Some other info</td></tr>
  </tbody>
</table>
</body></html>
"""


def make_soup(html: str) -> BeautifulSoup:
    return BeautifulSoup(html, "lxml")


# ---------------------------------------------------------------------------
# resolve_team
# ---------------------------------------------------------------------------

class TestResolveTeam:
    def test_city_name(self):
        assert resolve_team("Florida Panthers") == "FLA"

    def test_nickname(self):
        assert resolve_team("Edmonton Oilers") == "EDM"

    def test_multi_word_city(self):
        assert resolve_team("Los Angeles Kings") == "LAK"

    def test_partial_match(self):
        # Should match on fragment anywhere in the string
        assert resolve_team("Tampa Bay Lightning") == "TBL"

    def test_unknown_team_returns_none(self):
        assert resolve_team("Springfield Atoms") is None


# ---------------------------------------------------------------------------
# parse_stats_table
# ---------------------------------------------------------------------------

class TestParseStatsTable:
    def test_returns_correct_row_count(self):
        # parse_stats_table drops sub-header rows (class="thead") and "Rk" rows,
        # but does NOT filter empty player names — that is the caller's responsibility
        # (see scrape_skaters / scrape_goalies). 3 rows expected: 2 named + 1 empty-name.
        rows = parse_stats_table(make_soup(SKATERS_HTML))
        assert len(rows) == 3

    def test_correct_player_names(self):
        rows = parse_stats_table(make_soup(SKATERS_HTML))
        names = [r["Player"] for r in rows]
        assert names == ["Connor McDavid", "Leon Draisaitl", ""]

    def test_skips_sub_header_rows(self):
        # No row should have "Rk" as its Player value
        rows = parse_stats_table(make_soup(SKATERS_HTML))
        assert all(r.get("Player") != "Rk" for r in rows)

    def test_duplicate_columns_are_deduplicated(self):
        # Skaters table has two EV, PP, SH columns — should become EV/EV_1, etc.
        rows = parse_stats_table(make_soup(SKATERS_HTML))
        assert "EV" in rows[0]
        assert "EV_1" in rows[0]

    def test_raises_on_missing_table(self):
        soup = make_soup("<html><body><p>No table here</p></body></html>")
        with pytest.raises(ValueError, match="table#stats"):
            parse_stats_table(soup)


# ---------------------------------------------------------------------------
# scrape_skaters (offline — monkeypatches fetch_soup)
# ---------------------------------------------------------------------------

class TestScrapeSkaters:
    def test_returns_expected_columns(self, monkeypatch):
        monkeypatch.setattr("scraper.fetch_soup", lambda url: make_soup(SKATERS_HTML))
        records = scrape_skaters(date(2025, 6, 18))
        # Each record: (scrape_date, name_ref, team, goals, assists)
        assert len(records) == 2
        assert records[0] == (date(2025, 6, 18), "Connor McDavid", "EDM", 10, 20)
        assert records[1] == (date(2025, 6, 18), "Leon Draisaitl", "EDM", 8, 15)

    def test_skips_rows_with_empty_name(self, monkeypatch):
        monkeypatch.setattr("scraper.fetch_soup", lambda url: make_soup(SKATERS_HTML))
        records = scrape_skaters(date(2025, 6, 18))
        assert all(r[1] != "" for r in records)


# ---------------------------------------------------------------------------
# scrape_goalies (offline)
# ---------------------------------------------------------------------------

class TestScrapeGoalies:
    def test_returns_expected_columns(self, monkeypatch):
        monkeypatch.setattr("scraper.fetch_soup", lambda url: make_soup(GOALIES_HTML))
        records = scrape_goalies(date(2025, 6, 18))
        # Each record: (scrape_date, name_ref, team, wins, shutouts, goals, assists)
        assert len(records) == 2
        assert records[0] == (date(2025, 6, 18), "Sergei Bobrovsky", "FLA", 16, 3, 0, 0)
        assert records[1] == (date(2025, 6, 18), "Connor Hellebuyck", "WPG", 2, 1, 0, 1)


# ---------------------------------------------------------------------------
# scrape_eliminations (offline)
# ---------------------------------------------------------------------------

class TestScrapeEliminations:
    def setup_method(self):
        self.today = date(2025, 6, 18)

    def _run(self, monkeypatch, html=PLAYOFFS_HTML):
        monkeypatch.setattr("scraper.fetch_soup", lambda url: make_soup(html))
        return scrape_eliminations(self.today)

    def test_completed_series_loser_is_eliminated(self, monkeypatch):
        records = self._run(monkeypatch)
        eliminated_teams = {r[0] for r in records}
        # Final: FLA won 4-2, so EDM is eliminated
        assert "EDM" in eliminated_teams

    def test_winner_is_not_eliminated(self, monkeypatch):
        records = self._run(monkeypatch)
        eliminated_teams = {r[0] for r in records}
        assert "FLA" not in eliminated_teams

    def test_in_progress_series_skipped(self, monkeypatch):
        # "3-2" series (EDM lead DAL) has no 4-win side yet
        records = self._run(monkeypatch)
        eliminated_teams = {r[0] for r in records}
        assert "DAL" not in eliminated_teams

    def test_sweep_is_handled(self, monkeypatch):
        # CAR beat NJD 4-0, so NJD should be eliminated
        records = self._run(monkeypatch)
        eliminated_teams = {r[0] for r in records}
        assert "NJD" in eliminated_teams

    def test_eliminated_date_is_today(self, monkeypatch):
        records = self._run(monkeypatch)
        assert all(r[1] == self.today for r in records)

    def test_non_bracket_rows_skipped(self, monkeypatch):
        # "League" row should produce no elimination record
        records = self._run(monkeypatch)
        # Only 3 completed series in the fixture (Final, E Conf Final, First Round)
        assert len(records) == 3

    def test_round_slug_is_lowercased_underscored(self, monkeypatch):
        records = self._run(monkeypatch)
        slugs = {r[2] for r in records}
        assert "final" in slugs
        assert "first_round" in slugs
