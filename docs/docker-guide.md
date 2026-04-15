# Docker Guide

This doc covers the Docker basics you'll need to operate this project day-to-day. It assumes you understand what Docker is conceptually (isolated containers, images) but haven't used it much hands-on.

---

## The mental model for this project

- An **image** is a blueprint — built once from a `Dockerfile`, reused many times.
- A **container** is a running instance of an image.
- `docker-compose.yml` describes all the services (scraper, shiny_pool1, shiny_pool2), their images, ports, and environment variables. It's the single place that wires everything together.

This project has two images:
- `nhlplayoffpool-scraper` — built from `scraper/Dockerfile`
- `nhlplayoffpool-shiny_pool1` / `shiny_pool2` — both built from `shiny/Dockerfile` (same image, different env vars)

---

## Starting and stopping

```bash
# Start everything (dashboards + scraper cron), attach to logs
docker compose up

# Start everything in the background (detached)
docker compose up -d

# Start only the dashboards (most common day-to-day)
docker compose up shiny_pool1 shiny_pool2

# Stop everything
docker compose down
```

When you run `docker compose up` for the first time (or after `--build`), Docker builds the images before starting containers. Subsequent runs reuse the cached images and start instantly.

---

## Rebuilding after code changes

Docker does **not** automatically pick up changes to your source files. If you edit `scraper.py`, `app.R`, `scoring.R`, or any other file, you must rebuild the affected image:

```bash
# Rebuild and restart everything
docker compose up --build

# Rebuild only the scraper (faster if only scraper changed)
docker compose up --build scraper

# Rebuild only the Shiny containers
docker compose up --build shiny_pool1 shiny_pool2
```

`--build` forces Docker to re-copy your source files into the image and reinstall any changed dependencies. Without it, you're running the old code.

---

## Checking what's running

```bash
# List running containers and their status
docker compose ps

# Same, but for all containers (including stopped ones)
docker compose ps -a
```

Example output:
```
NAME                        STATUS          PORTS
nhlplayoffpool-scraper-1    Up 3 hours
nhlplayoffpool-shiny_pool1  Up 3 hours      0.0.0.0:3838->3838/tcp
nhlplayoffpool-shiny_pool2  Up 3 hours      0.0.0.0:3839->3838/tcp
```

---

## Reading logs

```bash
# See logs from all services
docker compose logs

# Logs for a specific service
docker compose logs shiny_pool1

# Follow logs in real time (like tail -f)
docker compose logs -f shiny_pool1

# Follow scraper cron output
docker compose logs -f scraper
```

When the Shiny app crashes or shows an error in the browser, the logs here will have the R error message and stack trace.

---

## Running a one-off command inside a container

Sometimes you want to run something inside a container without starting the full service. Use `docker compose run --rm`:

```bash
# Run the scraper manually (outside of cron)
docker compose run --rm scraper python scraper.py

# Run Python unit tests
docker compose run --rm scraper python -m pytest tests/ -v

# Run R unit tests
docker compose run --rm shiny_pool1 Rscript -e \
  "testthat::test_file('/srv/shiny-server/tests/testthat/test_scoring.R')"

# Open an interactive shell inside the scraper container
docker compose run --rm scraper bash

# Open an interactive R session inside the shiny container
docker compose run --rm shiny_pool1 R
```

`--rm` means the container is deleted after the command finishes. Without it, stopped containers accumulate and waste disk space.

---

## Cleaning up

Docker caches layers aggressively. Over time this can consume significant disk space.

```bash
# Remove stopped containers and unused images for this project
docker compose down --rmi local

# Nuclear option: remove ALL unused Docker data system-wide
# (safe to run, but will remove cached layers and slow down the next build)
docker system prune
```

---

## When things go wrong

| Symptom | Likely cause | Fix |
|---------|-------------|-----|
| Browser shows "Error" or blank page | R error in app startup | Check `docker compose logs shiny_pool1` |
| Container exits immediately | Crash on startup | Check `docker compose logs scraper` |
| Code changes not reflected | Image not rebuilt | Run `docker compose up --build` |
| "Connection refused" to DB | Postgres not listening on Docker bridge | See `docs/infrastructure.md` — PostgreSQL networking section |
| Port already in use | Another process on 3838/3839 | `docker compose down` then retry, or check `ss -tlnp \| grep 3838` |
