library(tidyverse)
library(ggrepel)
library(nflreadr)
library(nflplotR)

options(scipen = 9999)

# retrieves and loads the play by play data from 2019
# to keep things simple, <- is basically same as =
data <- load_pbp(2019)
# shows the dimensions of the table
# 47258 rows
# 372 columns
dim(data)
# shows the column labels and datatypes (first 10 cols)
str(data[1:10])
# peeks at the rows (first 10 cols)
glimpse(data[1:10])

# shows the entire table for the selected columns
data %>%
  select(home_team, away_team, posteam, desc) %>%
  View()

# shows the first few rows for the selected columns
data %>%
  select(play_id, game_id, posteam, defteam, desc, rush, pass) %>%
  head()

# filters for rush or pass plays only
data %>%
  filter(rush == 1 | pass == 1) %>%
  select(posteam, desc, rush, pass, name, passer, rusher, receiver) %>%
  head()

# filters for special teams only
data %>% 
  filter(special == 1) %>%
  select(down, ydstogo, desc) %>% 
  head()

# filters for non special teams fourth down plays
data %>% 
  filter(down == 4 & special == 0) %>%
  select(down, ydstogo, desc) %>% 
  head()

# saves a new dataframe where we filter for rush or pass plays with EPA
# is.na() means to exclude plays with NA value
pbp_rp <- data %>%
  filter(rush == 1 | pass == 1, !is.na(epa))

pbp_rp %>%
  # filter for dallas rush plays
  filter(posteam == "DAL", rush == 1) %>%
  # group by the rusher, e.g. Pollard, Zeke
  group_by(rusher) %>%
  # summarizes and displays the stats you specify
  summarize(
    mean_epa = mean(epa), success_rate = mean(success), 
    ypc = mean(yards_gained), plays = n()
  ) %>%
  # presents mean_epa in descending order (the - sign)
  arrange(-mean_epa) %>%
  # filters for players who had more than 20 rushes
  filter(plays > 20)
  # this is equivalent to
  # filter(arrange(summarize(group_by(filter(pbp_rp))))
  # much easier to understand the other way
  # my question is how do we know to put the functions in the correct order?

pbp_rp %>%
  filter(posteam == "DAL", down <= 4, play_type == 'run') %>%
  group_by(rusher) %>%
  summarize(
    mean_epa = mean(epa), success_rate = mean(success), 
    ypc=mean(yards_gained), plays=n()
  ) %>%
  arrange(-mean_epa) %>%
  filter(plays > 20)

pbp_rp %>%
  # add a new variable 'home' where if home team has the ball
  # assign a value of 1, 0 if they don't
  mutate(
    home = if_else(posteam == home_team, 1, 0)
  ) %>%
  select(posteam, home_team, home) %>%
  head(10)

pbp_rp %>%
  mutate(
    home = if_else(posteam == home_team, 1, 0)
  ) %>%
  # group by home and away
  group_by(home) %>%
  # show mean epa for all home teams (1) and away teams (0)
  summarize(epa = mean(epa))

pbp_rp %>%
  # filter out rows that don't have completion % value
  filter(!is.na(cp)) %>%
  # add a new variable 'depth' and assign appropriate conditions
  # for the cases you specify, e.g. short and deep
  mutate(
    depth = case_when(
      air_yards < 0 ~ "Negative",
      air_yards >= 0 & air_yards < 10 ~ "Short",
      air_yards >= 10 & air_yards < 20 ~ "Medium",
      air_yards >= 20 ~ "Deep" 
    )
  ) %>%
  # group by the different depths you specified in mutate()
  group_by(depth) %>%
  # show mean completion % for the different depths
  summarise(cp = mean(cp))

schotty <- pbp_rp %>%
  filter(wp > .20 & wp < .80 & down <= 2 & qtr <= 2 & 
           half_seconds_remaining > 120) %>%
  group_by(posteam) %>%
  summarise(mean_pass = mean(pass), plays = n()) %>%
  arrange(-mean_pass)
schotty

# aes is how you set variables
# reorder sorts the team according to pass rate
ggplot(schotty, aes(x=reorder(posteam,-mean_pass), y=mean_pass)) +
  geom_text(aes(label=posteam))

pbp <- load_pbp(2021:2023)

pbp %>%
  group_by(season) %>%
  summarise(n = n())

pbp %>%
  group_by(play_type) %>%
  summarise(n = n())

qbs <- pbp %>%
  filter(season_type == "REG", !is.na(epa)) %>%
  # included two variables so in case of same name,
  # can differentiate by id
  group_by(id, name) %>%
  summarise(
    epa = mean(qb_epa),
    cpoe = mean(cpoe, na.rm = T),
    n_dropbacks = sum(pass),
    n_plays = n(),
    # last gets the last team the player was on
    team = last(posteam)
  ) %>%
  # good practice after grouping to make sure no weird behavior occurs
  ungroup() %>%
  filter(n_dropbacks > 100 & n_plays > 1000)
qbs

load_teams()

qbs <- qbs %>%
  # left join keeps all rows from table x
  # rows from table y that have matching keys from rows in table x
  # will join to those rows
  # team from qbs and team_abbr from load_teams()
  left_join(load_teams(), by = c('team' = 'team_abbr'))

# with dots
qbs %>%
  ggplot(aes(x = cpoe, y = epa)) +
  #horizontal line with mean EPA
  geom_hline(yintercept = mean(qbs$epa), color = "red", linetype = "dashed", alpha=0.5) +
  #vertical line with mean CPOE
  geom_vline(xintercept = mean(qbs$cpoe), color = "red", linetype = "dashed", alpha=0.5) +
  #add points for the QBs with the right colors
  #cex controls point size and alpha the transparency (alpha = 1 is normal)
  geom_point(color = qbs$team_color, cex=qbs$n_plays / 350, alpha = .6) +
  #add names using ggrepel, which tries to make them not overlap
  geom_text_repel(aes(label=name)) +
  #add a smooth line fitting cpoe + epa
  stat_smooth(geom='line', alpha=0.5, se=FALSE, method='lm')+
  #titles and caption
  labs(x = "Completion % above expected (CPOE)",
       y = "EPA per play (passes, rushes, and penalties)",
       title = "Quarterback Efficiency, 2021 - 2023",
       caption = "Data: @nflfastR") +
  #uses the black and white ggplot theme
  theme_bw() +
  #center title with hjust = 0.5
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
  ) +
  #make ticks look nice
  #if this doesn't work, `install.packages('scales')`
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

# with team logos
qbs %>%
  ggplot(aes(x = cpoe, y = epa)) +
  #horizontal line with mean EPA
  geom_hline(yintercept = mean(qbs$epa), color = "red", linetype = "dashed", alpha=0.5) +
  #vertical line with mean CPOE
  geom_vline(xintercept = mean(qbs$cpoe), color = "red", linetype = "dashed", alpha=0.5) +
  #add points for the QBs with the logos (this uses nflplotR package)
  geom_nfl_logos(aes(team_abbr = team), width = qbs$n_plays / 45000, alpha = 0.75) +
  #add names using ggrepel, which tries to make them not overlap
  geom_text_repel(aes(label=name)) +
  #add a smooth line fitting cpoe + epa
  stat_smooth(geom='line', alpha=0.5, se=FALSE, method='lm')+
  #titles and caption
  labs(x = "Completion % above expected (CPOE)",
       y = "EPA per play (passes, rushes, and penalties)",
       title = "Quarterback Efficiency, 2021 - 2023",
       caption = "Data: @nflfastR") +
  #uses the black and white ggplot theme
  theme_bw() +
  #center title with hjust = 0.5
  theme(
    plot.title = element_text(size = 14, hjust = 0.5, face = "bold")
  ) +
  #make ticks look nice
  #if this doesn't work, `install.packages('scales')`
  scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
  scale_x_continuous(breaks = scales::pretty_breaks(n = 10))

# get pbp and filter to regular season rush and pass plays
pbp_13 <- nflreadr::load_pbp(2023) %>%
  dplyr::filter(season_type == "REG") %>%
  dplyr::filter(!is.na(posteam) & (rush == 1 | pass == 1))
# offense epa
offense <- pbp_13 %>%
  dplyr::group_by(team = posteam) %>%
  # na.rm = TRUE means don't include NA values
  dplyr::summarise(off_epa = mean(epa, na.rm = TRUE))
# defense epa
defense <- pbp_13 %>%
  dplyr::group_by(team = defteam) %>%
  dplyr::summarise(def_epa = mean(epa, na.rm = TRUE))
# make figure
offense %>%
  # joining the offense and defense by the team key
  dplyr::inner_join(defense, by = "team") %>%
  ggplot2::ggplot(aes(x = off_epa, y = def_epa)) +
  # tier lines
  # The slope lines are created using geom_abline()
  ggplot2::geom_abline(slope = -1.5, intercept = (4:-3)/10, alpha = .2) +
  # nflplotR magic
  # The geom_mean_lines() function adds mean lines for offensive and defensive EPA per play
  nflplotR::geom_mean_lines(aes(y0 = off_epa, x0 = def_epa)) +
  nflplotR::geom_nfl_logos(aes(team_abbr = team), width = 0.07, alpha = 0.7) +
  ggplot2::labs(
    x = "Offense EPA/play",
    y = "Defense EPA/play",
    caption = "Data: @nflfastR",
    title = "2023 NFL Offensive and Defensive EPA per Play"
  ) +
  ggplot2::theme_bw() +
  ggplot2::theme(
    plot.title = ggplot2::element_text(size = 12, hjust = 0.5, face = "bold")
  ) +
  # scale_y_reverse() reverses the vertical axis so that up = better defense
  ggplot2::scale_y_reverse()

# win total model
# gets all the past games
games <- nflreadr::load_schedules()
str(games)

# home results
home <- games %>%
  filter(game_type == 'REG') %>%
  select(season, week, home_team, result) %>%
  rename(team = home_team)
home %>% head(5)

# away results
away <- games %>%
  filter(game_type == 'REG') %>%
  select(season, week, away_team, result) %>%
  rename(team = away_team) %>%
  mutate(result = -result)
away %>% head(5)

# adds all the rows together
# that way you have both the home and away games of one team in one table
results <- bind_rows(home, away) %>%
  arrange(week, season) %>%
  mutate(
    win = case_when(
      result > 0 ~ 1,
      result < 0 ~ 0,
      result == 0 ~ 0.5
    )
  )

# this shows the away game rows added below the home game rows on line 16 'MIN'
results %>% print(n=20)

# will show all the games for that team's specified season
results %>% filter(season == 2013 & team == 'SEA')

#grabs the team wins
team_wins <- results %>%
  # groups by team and season
  group_by(team, season) %>%
  summarise(
    # adds up wins and point diff
    wins = sum(win),
    point_diff = sum(result)) %>%
  ungroup()

# shows the 30 most winning seasons, with point diff as tie breaker 
team_wins %>%
  arrange(-wins, -point_diff) %>%
  print(n=30)

# gets the data we need to calculate off rush epa, off pass epa
# def rush epa, and def pass epa
pbp <- load_pbp(1999:2022) %>%
  filter(rush == 1 | pass == 1, season_type == "REG", !is.na(epa), 
         !is.na(posteam), posteam != "") %>%
  select(season, posteam, pass, defteam, epa)

# grabs the EPA/play for pass and for rush
offense <- pbp %>%
  group_by(posteam, season, pass) %>%
  summarize(epa = mean(epa)) %>%
  # pivot_wider pivots rows into columns
  # so instead of having two rows for every team (one row pass and one row rush)
  # every team will have one row with both a pass column and a rush column
  pivot_wider(names_from = pass, values_from = epa) %>%
  # 0 is rush, pass is 1
  rename(off_pass_epa = `1`, off_rush_epa = `0`)

# grabs the EPA/play for def pass and for def rush
defense <- pbp %>%
  group_by(defteam, season, pass) %>% 
  summarize(epa = mean(epa)) %>%
  pivot_wider(names_from = pass, values_from = epa) %>%
  rename(def_pass_epa = `1`, def_rush_epa = `0`)

# shows top 5 offenses by EPA all time
offense %>%
  arrange(-off_pass_epa) %>%
  head(5)

# shows top 5 defenses by EPA all time
defense %>%
  arrange(def_pass_epa) %>%
  head(5)

team_wins <- team_wins %>%
  mutate(
    # these teams have changed cities, so we need to make sure they count
    # for just one team
    team = case_when(
      team == 'OAK' ~ 'LV',
      team == 'SD' ~ 'LAC',
      team == 'STL' ~ 'LA',
      TRUE ~ team
    )
  )

# makes sure every season for every team is accounted for
# e.g. SD seasons count as LAC seasons
team_wins %>%
  group_by(team) %>%
  summarise(n = n()) %>%
  arrange(n) %>%
  print(n = 32)

# joins the offense epa and defense epa dataframe to the team wins dataframe
# by the team and season
data <- team_wins %>%
  left_join(offense, by = c('team' = 'posteam', 'season')) %>%
  left_join(defense, by = c('team' = 'defteam', 'season'))

# shows the team's wins and EPAs since 2012
data %>%
  filter(team == 'SEA' & season >= 2012)

# this adds the prior year's EPAs to the current year's row so we can do
# correlation and regression calculations
data <- data %>% 
  arrange(team, season) %>%
  group_by(team) %>% 
  mutate(
    prior_off_rush_epa = lag(off_rush_epa),
    prior_off_pass_epa = lag(off_pass_epa),
    prior_def_rush_epa = lag(def_rush_epa),
    prior_def_pass_epa = lag(def_pass_epa),
    prior_point_diff = lag(point_diff)
  ) %>% 
  ungroup()

data %>%
  head(5)

# correlation results
# important values
# shows how predictive the stat is for winning
# row: off pass epa col: wins
# row: off rush epa col: wins
# shows how stable the stat is year to year
# row: off pass epa col: prior off pass epa
# row: off rush epa col: prior off rush epa
data %>%
  select(-team, -season) %>%
  # correlation function
  cor(use="complete.obs") %>%
  # rounds to 2 decimal places
  round(2)

data <- data %>% filter(season >= 2009)

# regression results, R squared the most important number to look at
fit <- lm(wins ~ prior_off_pass_epa  + prior_off_rush_epa + prior_def_pass_epa 
          + prior_def_rush_epa, data = data)

summary(fit)

fit2 <- lm(wins ~ prior_point_diff, data = data)

summary(fit2)

# prediction for 2023 season win totals based off EPA
preds <- predict(fit, data %>% filter(season == 2023)) %>%
  # was just a vector, need a tibble to bind
  as_tibble() %>%
  # make the column name make sense
  rename(prediction = value) %>%
  round(1) %>%
  # get team names
  bind_cols(
    data %>% filter(season == 2023) %>% select(team)
  )

preds %>%
  arrange(-prediction) %>%
  print(n = 32)

# prediction for 2023 season win totals based off point differential
preds2 <- predict(fit2, data %>% filter(season == 2023)) %>%
  # was just a vector, need a tibble to bind
  as_tibble() %>%
  # make the column name make sense
  rename(prediction = value) %>%
  round(1) %>%
  # get team names
  bind_cols(
    data %>% filter(season == 2023) %>% select(team)
  )

preds2 %>%
  arrange(-prediction) %>%
  print(n = 32)
