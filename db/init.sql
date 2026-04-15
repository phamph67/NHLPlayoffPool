-- NHL Playoff Pool schema
-- Init script: runs once on first postgres container boot

CREATE TABLE IF NOT EXISTS players (
    name_key       TEXT PRIMARY KEY,          -- form name, e.g. "AARON EKBLAD FLA"
    name_ref       TEXT NOT NULL,             -- hockey-reference name, e.g. "Aaron Ekblad"
    position_type  TEXT NOT NULL              -- 'skater' | 'goalie'
);

-- Pool picks: one row per participant × position slot
-- Placeholder table: schema mirrors the Google Forms / CSV structure from main.R
-- position_slot values: center_1..8, winger_1..10, defenseman_1..6, goalie_1..3
CREATE TABLE IF NOT EXISTS picks (
    participant    TEXT NOT NULL,
    pool_id        INT  NOT NULL,             -- 1 or 2 (one per workplace pool)
    player_key     TEXT NOT NULL REFERENCES players(name_key),
    position_slot  TEXT NOT NULL,
    UNIQUE (participant, pool_id, position_slot)
);

CREATE TABLE IF NOT EXISTS skaters (
    scrape_date  DATE NOT NULL,
    name_ref     TEXT NOT NULL,
    team         TEXT NOT NULL,
    goals        INT  NOT NULL DEFAULT 0,
    assists      INT  NOT NULL DEFAULT 0,
    PRIMARY KEY (scrape_date, name_ref)
);

CREATE TABLE IF NOT EXISTS goalies (
    scrape_date  DATE NOT NULL,
    name_ref     TEXT NOT NULL,
    team         TEXT NOT NULL,
    wins         INT  NOT NULL DEFAULT 0,
    shutouts     INT  NOT NULL DEFAULT 0,
    goals        INT  NOT NULL DEFAULT 0,
    assists      INT  NOT NULL DEFAULT 0,
    PRIMARY KEY (scrape_date, name_ref)
);

-- One row per team; eliminated_date is NULL until the team is knocked out
CREATE TABLE IF NOT EXISTS eliminations (
    team            TEXT PRIMARY KEY,
    eliminated_date DATE NOT NULL,
    round           TEXT NOT NULL
);
