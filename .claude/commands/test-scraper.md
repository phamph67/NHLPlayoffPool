Run the Python scraper unit tests inside the scraper container.

```bash
docker compose run --rm scraper python -m pytest tests/ -v
```

Report the full output and flag any failures.
