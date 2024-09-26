library(tidyverse)
library(ggrepel)
library(nflreadr)
library(nflplotR)
library(nflfastR)
library(DBI)
library(RSQLite)

# example 1: replicate nflscrapR with fast_scraper
# nflscrapR
readr::read_csv("https://github.com/ryurko/nflscrapR-data/blob/master/play_by_play_data/regular_season/reg_pbp_2019.csv?raw=true") %>%
  dplyr::filter(home_team == "SF" & away_team == "SEA") %>%
  dplyr::select(desc, play_type, ep, epa, home_wp) %>%
  utils::head(6) %>%
  # creates table to view play by play data
  knitr::kable(digits = 3)

# nflfastR scraper
nflfastR::fast_scraper("2019_10_SEA_SF") %>%
  # creates name column for the primary player involved
  nflfastR::clean_pbp() %>%
  dplyr::select(desc, play_type, ep, epa, home_wp, name) %>%
  utils::head(6) %>%
  knitr::kable(digits = 3)

# example 2: scrape games quickly with fast_scraper
# fast_scraper_schedules is depreciated
# games_2019 <- nflfastR::fast_scraper_schedules(2019) %>%
# pull 10 game IDs
#   utils::head(10) %>%
#   dplyr::pull(game_id)
# tictoc shows how much time has elapsed between tic and toc
# glue allows you to format the string that is displayed in the console
# tictoc::tic(glue::glue("{length(games_2019)} games with nflfastR:"))
# f <- nflfastR::fast_scraper(games_2019)
# tictoc::toc()

# example 3: looking up CPOE
games_2022 <- nflfastR::load_pbp(2022) %>% dplyr::filter(season_type == "REG")

games_2022 %>%
  dplyr::filter(!is.na(cpoe)) %>%
  dplyr::group_by(passer_player_name) %>%
  dplyr::summarize(cpoe = mean(cpoe), Atts = n()) %>%
  dplyr::filter(Atts > 200) %>%
  dplyr::arrange(-cpoe) %>%
  utils::head(5) %>%
  knitr::kable(digits = 2)

# example 4: using drive information
# compares how much more (or less) drives after touchbacks would result in
# a score in 2022 than in 2016
pbp <- nflfastR::load_pbp(c(2016, 2022))

out <- pbp %>%
  dplyr::filter(season_type == "REG" & down == 1 & ydstogo == 10 & yardline_100 == 75) %>%
  # creates a drive score column
  # the if_else() is basically if the fixed_drive_result is Touchdown or Field goal
  # the score is 1, else it is 0
  # the c() is just how you create a vector
  dplyr::mutate(drive_score = dplyr::if_else(fixed_drive_result %in% c("Touchdown", "Field goal"), 1, 0)) %>%
  dplyr::group_by(season) %>%
  dplyr::summarize(drive_score = mean(drive_score))

out %>% 
  knitr::kable(digits = 3)

# example 6: expected points calculator
# tibble builds a data frame
data <- tibble::tibble(
  "season" = 1999:2023,
  "home_team" = "SEA",
  "posteam" = "SEA",
  "roof" = "outdoors",
  "half_seconds_remaining" = 1800,
  # basically 1999-2015 touchback was at 20 yd line
  # 2016-2023 touch is at 25 yd line
  "yardline_100" = c(rep(80, 17), rep(75, 8)),
  "down" = 1,
  "ydstogo" = 10,
  "posteam_timeouts_remaining" = 3,
  "defteam_timeouts_remaining" = 3
)

# ep is expected points
nflfastR::calculate_expected_points(data) %>%
  dplyr::select(season, yardline_100, td_prob, ep) %>%
  knitr::kable(digits = 2)

# example 7: win probability calculator
data <- tibble::tibble(
  "receive_2h_ko" = 0,
  "home_team" = "SEA",
  "posteam" = "SEA",
  "score_differential" = 0,
  "half_seconds_remaining" = 1800,
  "game_seconds_remaining" = 3600,
  "spread_line" = c(1, 3, 4, 7, 14),
  "down" = 1,
  "ydstogo" = 10,
  "yardline_100" = 75,
  "posteam_timeouts_remaining" = 3,
  "defteam_timeouts_remaining" = 3
)

nflfastR::calculate_win_probability(data) %>%
  dplyr::select(spread_line, wp, vegas_wp) %>%
  knitr::kable(digits = 2)

# example 9: expected yards after catch model
nflfastR::load_pbp(2023) %>%
  dplyr::group_by(receiver, receiver_id, posteam) %>%
  dplyr::mutate(tgt = sum(complete_pass + incomplete_pass)) %>%
  dplyr::filter(tgt >= 50) %>%
  dplyr::filter(complete_pass == 1, air_yards < yardline_100, !is.na(xyac_epa)) %>%
  dplyr::summarize(
    epa_oe = mean(yac_epa - xyac_epa),
    actual_fd = mean(first_down),
    expected_fd = mean(xyac_fd),
    fd_oe = mean(first_down - xyac_fd),
    rec = dplyr::n()
  ) %>%
  dplyr::ungroup() %>%
  # shows YAC EPA over expected (yac_epa) and first down over expected (fd_oe)
  dplyr::select(receiver, posteam, actual_fd, expected_fd, fd_oe, epa_oe, rec) %>%
  dplyr::arrange(-epa_oe) %>%
  utils::head(10) %>%
  knitr::kable(digits = 3)

# example 10: working with roster and position data
# depreciated now
roster <- nflfastR::fast_scraper_roster(2023)

games_2023 <- nflfastR::load_pbp(2023)

# doesn't change the games_2023 data itself
games_2023 %>%
  dplyr::filter(rush == 1 | pass == 1, posteam == "SEA") %>%
  dplyr::select(name, id)

# joins the pbp data with the roster data using receiver id as the key
joined <- games_2023 %>%
  dplyr::filter(!is.na(receiver_id)) %>%
  dplyr::select(posteam, season, desc, receiver, receiver_id, epa) %>%
  dplyr::left_join(roster, by = c("receiver_id" = "gsis_id"))

# shows the top 5 EPA leaders for the WR, TE, and RB positions
joined %>%
  dplyr::filter(position %in% c("WR", "TE", "RB")) %>%
  dplyr::group_by(receiver_id, receiver, position) %>%
  dplyr::summarize(tot_epa = sum(epa), n = n()) %>%
  dplyr::arrange(-tot_epa) %>%
  dplyr::ungroup() %>%
  dplyr::group_by(position) %>%
  dplyr::mutate(position_rank = 1:n()) %>%
  dplyr::filter(position_rank <= 5) %>%
  dplyr::rename(Pos_Rank = position_rank, Player = receiver, Pos = position, Tgt = n, EPA = tot_epa) %>%
  dplyr::select(Player, Pos, Pos_Rank, Tgt, EPA) %>%
  knitr::kable(digits = 0)

# example 11: replicating official stats
# shows passing leaders for the 2023 season
games_2023 %>%
  dplyr::filter(season_type == "REG") %>%
  nflfastR::calculate_player_stats() %>%
  dplyr::arrange(-passing_yards) %>%
  dplyr::select(player_name, recent_team, completions, attempts, passing_yards, passing_tds, interceptions) %>%
  utils::head(10) %>%
  knitr::kable(digits = 0)

# shows total scrimmage yards leaders for the 2023 season
games_2023 %>%
  dplyr::filter(season_type == "REG") %>%
  nflfastR::calculate_player_stats() %>%
  dplyr::mutate(
    yards = rushing_yards + receiving_yards, 
    touches = carries + receptions, 
    tds = rushing_tds + receiving_tds
  ) %>%
  dplyr::arrange(-yards) %>%
  dplyr::select(player_name, recent_team, carries, receptions, touches, yards, tds) %>%
  utils::head(10) %>%
  knitr::kable(digits = 0)

# shows the top ten fantasy players (WRs and RBs) for the 2023 season
games_2023 %>%
  dplyr::filter(week <= 16) %>%
  nflfastR::calculate_player_stats() %>%
  dplyr::mutate(
    ppg = fantasy_points_ppr / games
  ) %>%
  dplyr::filter(games > 5) %>%
  # only keep the WRs and RBs
  dplyr::inner_join(
    roster %>% 
      dplyr::filter(position == "WR" | position == "RB") %>% 
      dplyr::select(player_id = gsis_id),
    by = "player_id"
  ) %>%
  dplyr::arrange(-ppg) %>%
  dplyr::select(player_name, recent_team, games, fantasy_points_ppr, ppg) %>%
  utils::head(10) %>%
  knitr::kable(digits = 1)

# example 8: working with databases
# Creates pbp database when you run for the first time
# Afterwards anytime you want to add new games to the db
# all you have to do is run this command
# Parameter db_connection to use different db drivers
nflfastR::update_db()

# Establish connections with the db, provide file path
connection <- DBI::dbConnect(RSQLite::SQLite(), "./pbp_db")
connection

# list tables in the db
DBI::dbListTables(connection)

# list fields in the table
DBI::dbListFields(connection, "nflfastR_pbp") %>%
  utils::head(10)

# Save db contents into a variable
pbp_db <- dplyr::tbl(connection, "nflfastR_pbp")

# And then you can work with the data as you wish
pbp_db %>%
  dplyr::group_by(season) %>%
  dplyr::summarize(n = dplyr::n())

# if you want to collect queries into memory
# russ <- pbp_db %>%
#   dplyr::filter(name == "R.Wilson" & posteam == "SEA") %>%
#   dplyr::select(desc, epa) %>%
#   dplyr::collect()

#disconnect
DBI::dbDisconnect(connection)
