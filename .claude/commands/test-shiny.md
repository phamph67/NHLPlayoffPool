Run the R scoring unit tests inside the shiny container.

```bash
docker compose run --rm shiny_pool1 Rscript -e "testthat::test_file('/srv/shiny-server/tests/testthat/test_scoring.R')"
```

Report the full output and flag any failures.
