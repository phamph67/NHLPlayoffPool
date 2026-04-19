FROM python:3.12-slim

WORKDIR /app

# Install cron
RUN apt-get update \
    && apt-get install -y --no-install-recommends cron \
    && rm -rf /var/lib/apt/lists/*

COPY requirements.txt .
RUN pip install --no-cache-dir -r requirements.txt

COPY scraper.py .
COPY startup_check.py .
COPY tests/ tests/
COPY docker-entrypoint.sh /docker-entrypoint.sh
RUN chmod +x /docker-entrypoint.sh

# Install crontab
COPY crontab /etc/cron.d/scraper
RUN chmod 0644 /etc/cron.d/scraper

RUN touch /var/log/cron.log

CMD ["/docker-entrypoint.sh"]
