# Scoring logic for the NHL Playoff Pool.
# Sourced by app.R and by tests — no Shiny or DB dependencies here.

#' Compute per-participant cumulative scores across all scrape dates.
#'
#' @param picks        data.frame: participant, position_slot, name_ref, position_type
#' @param skaters_db   data.frame: scrape_date, name_ref, team, goals, assists
#' @param goalies_db   data.frame: scrape_date, name_ref, team, wins, shutouts, goals, assists
#' @param eliminations data.frame: team, eliminated_date
#'
#' @return data.frame: participant, date, score
compute_scores <- function(picks, skaters_db, goalies_db, eliminations) {
  # Skaters: goals + assists
  skaters_scored <- skaters_db |>
    dplyr::mutate(score = goals + assists) |>
    dplyr::select(scrape_date, name_ref, team, score)

  # Goalies: goals + assists + wins*2 + shutouts*3
  goalies_scored <- goalies_db |>
    dplyr::mutate(score = goals + assists + wins * 2L + shutouts * 3L) |>
    dplyr::select(scrape_date, name_ref, team, score)

  stats <- dplyr::bind_rows(skaters_scored, goalies_scored)
  all_dates <- sort(unique(stats$scrape_date))

  results <- lapply(all_dates, function(d) {
    day_stats <- stats[stats$scrape_date == d, ]

    picks |>
      dplyr::left_join(
        day_stats[, c("name_ref", "team", "score")],
        by = "name_ref"
      ) |>
      dplyr::left_join(eliminations, by = "team") |>
      dplyr::mutate(
        effective_score = dplyr::case_when(
          is.na(score)                                    ~ 0L,
          !is.na(eliminated_date) & eliminated_date <= d ~ 0L,
          TRUE                                            ~ score
        )
      ) |>
      dplyr::group_by(participant) |>
      dplyr::summarise(score = sum(effective_score), .groups = "drop") |>
      dplyr::mutate(date = d)
  })

  dplyr::bind_rows(results)
}

#' Parse a postgres:// URL into a named list of connection parameters.
#'
#' @param url Character: postgres://user:password\@host:port/dbname
#' @return Named list: user, password, host, port, dbname
parse_db_url <- function(url) {
  m <- regmatches(
    url,
    regexec("^postgres://([^:]+):([^@]+)@([^:]+):(\\d+)/(.+)$", url)
  )[[1]]
  if (length(m) < 6) stop("Could not parse DATABASE_URL: ", url)
  list(
    user     = m[2],
    password = m[3],
    host     = m[4],
    port     = as.integer(m[5]),
    dbname   = m[6]
  )
}
