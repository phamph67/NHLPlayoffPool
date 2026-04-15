library(testthat)
library(dplyr)

source(file.path(dirname(dirname(getwd())), "scoring.R"))

# ---------------------------------------------------------------------------
# Fixtures
# ---------------------------------------------------------------------------

make_picks <- function() {
  data.frame(
    participant   = c("Alice", "Alice", "Bob", "Bob"),
    position_slot = c("center_1", "goalie_1", "center_1", "goalie_1"),
    name_ref      = c("Connor McDavid", "Sergei Bobrovsky", "Leon Draisaitl", "Connor Hellebuyck"),
    position_type = c("skater", "goalie", "skater", "goalie"),
    stringsAsFactors = FALSE
  )
}

make_skaters <- function() {
  data.frame(
    scrape_date = as.Date(c("2025-05-01", "2025-05-02")),
    name_ref    = c("Connor McDavid", "Connor McDavid"),
    team        = c("EDM", "EDM"),
    goals       = c(3L, 4L),
    assists     = c(5L, 7L),
    stringsAsFactors = FALSE
  ) |>
    bind_rows(data.frame(
      scrape_date = as.Date(c("2025-05-01", "2025-05-02")),
      name_ref    = c("Leon Draisaitl", "Leon Draisaitl"),
      team        = c("EDM", "EDM"),
      goals       = c(2L, 3L),
      assists     = c(4L, 5L),
      stringsAsFactors = FALSE
    ))
}

make_goalies <- function() {
  data.frame(
    scrape_date = as.Date(c("2025-05-01", "2025-05-02")),
    name_ref    = c("Sergei Bobrovsky", "Sergei Bobrovsky"),
    team        = c("FLA", "FLA"),
    wins        = c(8L, 10L),
    shutouts    = c(1L, 2L),
    goals       = c(0L, 0L),
    assists     = c(0L, 0L),
    stringsAsFactors = FALSE
  ) |>
    bind_rows(data.frame(
      scrape_date = as.Date(c("2025-05-01", "2025-05-02")),
      name_ref    = c("Connor Hellebuyck", "Connor Hellebuyck"),
      team        = c("WPG", "WPG"),
      wins        = c(2L, 2L),
      shutouts    = c(1L, 1L),
      goals       = c(0L, 0L),
      assists     = c(1L, 1L),
      stringsAsFactors = FALSE
    ))
}

no_eliminations <- data.frame(
  team           = character(0),
  eliminated_date = as.Date(character(0)),
  stringsAsFactors = FALSE
)

# ---------------------------------------------------------------------------
# parse_db_url
# ---------------------------------------------------------------------------

test_that("parse_db_url extracts all fields correctly", {
  p <- parse_db_url("postgres://ppham:abc123@localhost:5432/nhl")
  expect_equal(p$user,     "ppham")
  expect_equal(p$password, "abc123")
  expect_equal(p$host,     "localhost")
  expect_equal(p$port,     5432L)
  expect_equal(p$dbname,   "nhl")
})

test_that("parse_db_url errors on malformed URL", {
  expect_error(parse_db_url("not-a-url"), "Could not parse")
})

# ---------------------------------------------------------------------------
# Skater scoring: goals + assists
# ---------------------------------------------------------------------------

test_that("skater score is goals + assists", {
  results <- compute_scores(
    picks        = make_picks() |> filter(participant == "Alice", position_slot == "center_1"),
    skaters_db   = make_skaters() |> filter(scrape_date == as.Date("2025-05-01"), name_ref == "Connor McDavid"),
    goalies_db   = make_goalies()[0, ],   # empty
    eliminations = no_eliminations
  )
  alice_day1 <- results |> filter(participant == "Alice", date == as.Date("2025-05-01"))
  # goals=3, assists=5 → score=8
  expect_equal(alice_day1$score, 8L)
})

# ---------------------------------------------------------------------------
# Goalie scoring: goals + assists + wins*2 + shutouts*3
# ---------------------------------------------------------------------------

test_that("goalie score is goals + assists + wins*2 + shutouts*3", {
  results <- compute_scores(
    picks        = make_picks() |> filter(participant == "Alice", position_slot == "goalie_1"),
    skaters_db   = make_skaters()[0, ],   # empty
    goalies_db   = make_goalies() |> filter(scrape_date == as.Date("2025-05-01"), name_ref == "Sergei Bobrovsky"),
    eliminations = no_eliminations
  )
  alice_day1 <- results |> filter(participant == "Alice", date == as.Date("2025-05-01"))
  # wins=8*2=16, shutouts=1*3=3, goals=0, assists=0 → score=19
  expect_equal(alice_day1$score, 19L)
})

# ---------------------------------------------------------------------------
# Elimination logic
# ---------------------------------------------------------------------------

test_that("player on eliminated team scores 0 from elimination date onward", {
  eliminations <- data.frame(
    team            = "EDM",
    eliminated_date = as.Date("2025-05-02"),
    stringsAsFactors = FALSE
  )
  results <- compute_scores(
    picks        = make_picks() |> filter(participant == "Alice", position_slot == "center_1"),
    skaters_db   = make_skaters() |> filter(name_ref == "Connor McDavid"),
    goalies_db   = make_goalies()[0, ],
    eliminations = eliminations
  )
  # May 1: EDM not yet eliminated → full score (3+5=8)
  day1 <- results |> filter(date == as.Date("2025-05-01"))
  expect_equal(day1$score, 8L)

  # May 2: EDM eliminated on May 2 → score is 0
  day2 <- results |> filter(date == as.Date("2025-05-02"))
  expect_equal(day2$score, 0L)
})

test_that("player on active team is unaffected by another team's elimination", {
  eliminations <- data.frame(
    team            = "WPG",
    eliminated_date = as.Date("2025-05-01"),
    stringsAsFactors = FALSE
  )
  results <- compute_scores(
    picks        = make_picks() |> filter(participant == "Alice", position_slot == "center_1"),
    skaters_db   = make_skaters() |> filter(name_ref == "Connor McDavid"),
    goalies_db   = make_goalies()[0, ],
    eliminations = eliminations
  )
  # EDM is not eliminated; Alice's skater should still score normally
  day2 <- results |> filter(date == as.Date("2025-05-02"))
  expect_equal(day2$score, 4L + 7L)  # goals=4, assists=7
})

test_that("player with no stats entry scores 0 not NA", {
  results <- compute_scores(
    picks        = make_picks() |> filter(participant == "Alice", position_slot == "center_1"),
    skaters_db   = make_skaters()[0, ],   # empty — player has no stats
    goalies_db   = make_goalies()[0, ],
    eliminations = no_eliminations
  )
  expect_equal(nrow(results), 0L)  # no dates in stats → no results rows
})

# ---------------------------------------------------------------------------
# Multi-participant aggregation
# ---------------------------------------------------------------------------

test_that("scores are summed per participant across all their picks", {
  results <- compute_scores(
    picks        = make_picks(),
    skaters_db   = make_skaters() |> filter(scrape_date == as.Date("2025-05-01")),
    goalies_db   = make_goalies() |> filter(scrape_date == as.Date("2025-05-01")),
    eliminations = no_eliminations
  )
  # Alice: McDavid (3+5=8) + Bobrovsky (0+0+8*2+1*3=19) = 27
  alice <- results |> filter(participant == "Alice")
  expect_equal(alice$score, 27L)

  # Bob: Draisaitl (2+4=6) + Hellebuyck (0+1+2*2+1*3=8) = 14
  bob <- results |> filter(participant == "Bob")
  expect_equal(bob$score, 14L)
})

test_that("compute_scores returns one row per participant per date", {
  results <- compute_scores(make_picks(), make_skaters(), make_goalies(), no_eliminations)
  expect_equal(nrow(results), 4L)  # 2 participants × 2 dates
})
