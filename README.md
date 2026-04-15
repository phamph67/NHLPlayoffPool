# NHLPlayoffPool
Data vis project for NHL Playoff Pool. Skater, Goalie, team standing statistics are scraped from the following sites every day at 21:00 EST from the NHL site:
<https://www.hockey-reference.com/playoffs> either by direct web scraping, or using the API (see this [Github link](https://github.com/Zmalski/NHL-API-Reference?tab=readme-ov-file#players) for reference)

### Scoring Rules

* Points always accumulate, you don't lose points if a team is eliminated
* If a team is eliminated, selected player(s) no longer produce points on next update
* Forwards, Defensemen, & Goalies get 1 point for each goal and assist. Goalies get 2 points for each win and 3 points for a shut out.
