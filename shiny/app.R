library(shiny)
library(DBI)
library(RPostgres)
library(dplyr)
library(tidyr)
library(ggplot2)
library(ggthemes)
library(colorspace)
library(glue)

source("scoring.R")

pool_id <- as.integer(Sys.getenv("POOL_ID", unset = "1"))

# ---------------------------------------------------------------------------
# Database connection
# ---------------------------------------------------------------------------

get_conn <- function() {
  p <- parse_db_url(Sys.getenv("DATABASE_URL"))
  dbConnect(
    RPostgres::Postgres(),
    host     = p$host,
    port     = p$port,
    dbname   = p$dbname,
    user     = p$user,
    password = p$password
  )
}

# ---------------------------------------------------------------------------
# UI
# ---------------------------------------------------------------------------

ui <- fluidPage(
  titlePanel(glue("NHL Playoff Pool \u2014 Pool {pool_id}")),
  plotOutput("standings_plot", height = "700px")
)

# ---------------------------------------------------------------------------
# Server
# ---------------------------------------------------------------------------

server <- function(input, output, session) {
  output$standings_plot <- renderPlot({
    conn <- get_conn()
    on.exit(dbDisconnect(conn))

    # --- Fetch data from DB ------------------------------------------------

    picks <- dbGetQuery(
      conn,
      "SELECT p.participant, p.position_slot, pl.name_ref, pl.position_type
       FROM picks p
       JOIN players pl ON pl.name_key = p.player_key
       WHERE p.pool_id = $1",
      params = list(pool_id)
    )

    skaters_db <- dbGetQuery(
      conn,
      "SELECT scrape_date, name_ref, team, goals, assists FROM skaters"
    )

    goalies_db <- dbGetQuery(
      conn,
      "SELECT scrape_date, name_ref, team, wins, shutouts, goals, assists FROM goalies"
    )

    # Guard: nothing to plot yet
    if (nrow(picks) == 0 || (nrow(skaters_db) == 0 && nrow(goalies_db) == 0)) {
      return(
        ggplot() +
          annotate("text", x = 0.5, y = 0.5, label = "No data yet \u2014 check back after the first scrape.") +
          theme_void()
      )
    }

    results <- compute_scores(picks, skaters_db, goalies_db)

    # --- Plot --------------------------------------------------------------

    results_latest <- results %>%
      filter(date == max(date)) %>%
      arrange(desc(score)) %>%
      mutate(name_labels = glue("{participant} : {score}"))

    results_latest$y_position <- rev(seq(
      min(results$score),
      max(results$score),
      length.out = nrow(results_latest)
    ))

    results$participant <- factor(results$participant, levels = results_latest$participant)

    n_lines <- nrow(results_latest)
    pal     <- colorspace::qualitative_hcl(n = n_lines)

    ggplot(results, aes(x = date, y = score, colour = participant)) +
      geom_point() +
      geom_line(alpha = 0.35, linetype = 2) +
      scale_colour_manual(
        name   = "Participant (by latest score)",
        values = pal,
        labels = results_latest$name_labels
      ) +
      scale_y_continuous(
        name         = "Score",
        breaks       = seq(0, 500, 20),
        minor_breaks = seq(0, 500, 10)
      ) +
      scale_x_date(
        name        = "Date",
        expand      = expansion(mult = c(0.1, 0.3)),
        date_labels = "%b %d",
        date_breaks = "1 day"
      ) +
      geom_segment(
        data    = results_latest,
        mapping = aes(
          x    = date,
          xend = date + 0.5,
          y    = score,
          yend = y_position
        ),
        alpha = 0.35
      ) +
      geom_text(
        data    = results_latest,
        mapping = aes(x = date + 0.5, y = y_position, label = name_labels),
        hjust   = -0.2,
        size    = 4
      ) +
      ggthemes::theme_clean() +
      theme(
        legend.position = "none",
        axis.text.x     = element_text(angle = 45, hjust = 1)
      )
  })
}

shinyApp(ui, server)
