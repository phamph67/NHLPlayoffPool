FROM rocker/shiny:4.4.2

# System dependencies required by RPostgres
RUN apt-get update \
    && apt-get install -y --no-install-recommends libpq-dev \
    && rm -rf /var/lib/apt/lists/*

# Install R packages
COPY install.R /tmp/install.R
RUN Rscript /tmp/install.R

# Remove default sample apps and place ours at the root
RUN rm -rf /srv/shiny-server/*
COPY app.R /srv/shiny-server/
COPY scoring.R /srv/shiny-server/
COPY tests/ /srv/shiny-server/tests/

EXPOSE 3838
