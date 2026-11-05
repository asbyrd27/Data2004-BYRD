# lab 3: acquire and document

library(tidyverse)
library(httr2)
library(rvest)

# you are an intern for Fangraphs (https://www.fangraphs.com/) and they 
# have given your group the opportunity to write a blog post about ballparks. 
# the story is internally called: 'MLB ballparks: 2000 vs now." 

# your supervisor, the editor, has asked you for two things: 
## 1. the data: who played where in 2000, who plays where now, and what today's 
## parks are like. 

## 2. a record of where every number came from, because the last intern asked 
## Gemini to do the story and literally every single number was wrong, which 
## is super embarrassing for a website who really only does numbers. 

# if y'all do well, the supervisor has said that your internships will be extended 
# but if y'all do not do what the supervisor asks, they're going to write 
# bad letters for y'all even if y'all don't ask for them. 

# you will ultimately have three saved files in your data/ path 
# and a data/README.md in that same path.

# you'll start by creating a README. 

# you can find a template I adapted from Cornell's Data Services README
# template on Canvas. 
# you will need to rename it and move it into your project's data/ folder. 
# open it in RStudio and fill it in AS YOU GO. DO NOT do it at the end. 
# when you push at the end of the day, you need to push your README as well. 



# let's do AN API LETS GO!

# MLB's teams endpoint: https://statsapi.mlb.com/api/v1/teams
# parameters: sportId (1 = MLB) and season.
# the story is 2000 vs. now, so you need two seasons: 2000 and 2026.

# build a request for each season: teams_req_2000 and teams_req_2026.
# print each one to check the URL before you send anything.

teams_req_2000 <- request("https://statsapi.mlb.com/api/v1/teams") |>
  req_url_query(
    sportId = 1,
    season = 2000
  )

teams_req_2026 <- request("https://statsapi.mlb.com/api/v1/teams") |>
  req_url_query(
    sportId = 1,
    season = 2026
  )

teams_req_2000
teams_req_2026

# remember weekly 5: what does MLB do with a parameter it doesn't know?
# so what are you checking for when you print the request?
# checking that the URL has the correct parameters and that MLB recognizes the ones we're using


# the two requests are the same except for one blank. what's the blank?
# season parameter changes from 2000 to 2026.

# send both and save each to its own file:
# data/raw/mlb_teams_2000.json and data/raw/mlb_teams_2026.json.
# (hint: req_perform() has a path argument)

teams_req_2000 |>
  req_perform(path = "data/raw/mlb_teams_2000.json")

teams_req_2026 |>
  req_perform(path = "data/raw/mlb_teams_2026.json")
# how many requests did you send? how many files did you get?
#two requests, 2 JSON files

# open one file and check that it's the season its name says.
# (hint: read it in, then look at the season of the first team)
str(teams_json_2026, max.level = 1)
teams_json_2026$teams[[1]]$season

# log both in your README: URLs, when you requested them, status, terms.
# what are the terms? (weekly 5)
#wiki we have to cite, mlb api non commercial 


# from here on, you read the files. you never ask the API again.

teams_json_2026 <- jsonlite::read_json("data/raw/mlb_teams_2026.json")
teams_json_2000 <- jsonlite::read_json("data/raw/mlb_teams_2000.json")

# look at the top level of one file, then one team, one level at a time.
str(teams_json_2026$teams[[1]]$venue)

str(teams_json_2026$teams[[1]]$league)

str(teams_json_2026$teams[[1]]$division)

# you need the same table from both files. instead of writing the hoist twice,
# write a recipe once and use it on both. finish this recipe so it also pulls
# location (locationName), venue_id and venue (inside venue), league and
# division (the name inside each):

teams_table <- \(json) {
  tibble(team = json$teams) |>
    hoist(
      team,
      id = "id",
      name = "name",
      location = "locationName",
      venue_id = c("venue", "id"),
      venue = c("venue", "name"),
      league = c("league", "name"),
      division = c("division", "name")
    ) |>
    select(-team)
}

# test it on one file first. does it look right?
teams_table(teams_json_2026)
teams_table(teams_json_2000)

# now make teams_2026 and teams_2000. how many rows each?
teams_2026 <- teams_table(teams_json_2026)
teams_2000 <- teams_table(teams_json_2000)

nrow(teams_2026)
nrow(teams_2000)

# why write a recipe instead of copying the hoist twice?
#so we can use the same code for both years instead of repeating hoist twice. It also makes sure both tables have the same columns.

# is id a key in each table? check. id is a key in each table because every team has a unique ID.

teams_2026 |>
  count(id) |>
  filter(n > 1)

teams_2000 |>
  count(id) |>
  filter(n > 1)

# the editor wants to know what has changed.
# so now we get to do our comparisons :)

# join 2000 to 2026 by team NAME. how many teams match? 24
# (hint: inner_join. suffix = c("_2000", "_2026") keeps the columns apart)

teams_2000 |>
  inner_join(
    teams_2026,
    by = "name",
    suffix = c("_2000", "_2026")
  ) |>
  nrow()

# which teams didn't match? (hint: a filtering join keeps the ones that
# DIDN'T match)
teams_2000 |>
  anti_join(teams_2026, by = "name") |>
  select(id, name)


# are those teams gone? look at them.
teams_2026 |>
  anti_join(teams_2000, by = "name") |>
  select(id, name)

# now join by id. call it then_now. how many match? 30
then_now <- teams_2000 |>
  inner_join(
    teams_2026,
    by = "id",
    suffix = c("_2000", "_2026")
  )
nrow(then_now)

# what does that tell you about names vs. ids as keys?
#Team IDs are more reliable than names because names can change when teams relocate, while IDs can remain the same.


# how many teams changed their name? their city? their league? which ones?
#6 teams changed thier name, Oakland Athletics went to Athletics, Tampa Bay Devil Rays changed to Tampa Bay Rays, Florida Marlins now the Miami Marlins, Anaheim Angels to Los Angeles Angles,
#Cleveland Indians now guardians,  Montreal Expos are now the Washington Nationals

# Switching cities: Oakland A's went to Sacramento, Montreal expos went to Washington state, called Nationals now
#One team that switched from NA to AL, Houston Astros 

# Teams that changed names
then_now |>
  filter(name_2000 != name_2026) |>
  select(name_2000, name_2026)

# Teams that changed cities
then_now |>
  filter(location_2000 != location_2026) |>
  select(name_2026, location_2000, location_2026)

# Teams that changed leagues
then_now |>
  filter(league_2000 != league_2026) |>
  select(name_2026, league_2000, league_2026)

# how many teams play in a park with a different NAME than in 2000? 23 teams have a different stadium name 
then_now |>
  filter(venue_2000 != venue_2026) |>
  select(name_2026, venue_2000, venue_2026)


# a new name isn't always a new park: Safeco Field became T-Mobile Park, and
# it's the same building. how could you tell a renamed park from a new one
# using this data? try it. what did you find? 
# 23 teams had different stadium names between 2000 and 2026, we compared the venue IDs, if the ID stayed the same but the name changed, it was likely renamed. 


then_now |>
  filter(venue_2000 != venue_2026) |>
  select(name_2026, venue_2000, venue_2026,
         venue_id_2000, venue_id_2026)


# the editor also wants to know what today's parks are like. MLB's API won't
# tell you that, but wikipedia will. time to scrape.

# https://en.wikipedia.org/wiki/List_of_current_Major_League_Baseball_stadiums
# before you scrape: same checks as thursday. robots.txt, the license, and
# "view page source." write the answers in your README.

# get the page and save it to data/raw/mlb_stadiums.html, then read the saved
# file into R and call it stadium_page.

url <- "https://en.wikipedia.org/wiki/List_of_current_Major_League_Baseball_stadiums"

request(url) |>
  req_user_agent("Aiden Byrd, DATA 2004 class, Transylvania University") |>
  req_perform(path = "data/raw/mlb_stadiums.html")

stadium_page <- read_html("data/raw/mlb_stadiums.html")
# when was the page last edited? put it in your README. This page was last edited on 13 September 2026, at 20:03 (UTC).
stadium_page |>
  html_elements("#footer-info-lastmod") |>
  html_text2()

# get all the data tables as tibbles. how many are there? three
tables <- stadium_page |>
  html_elements("table.wikitable") |>
  html_table()

length(tables)
# which one is the current stadiums table? check before you trust a number. table one 
# call it stadiums_raw.
stadiums_raw <- tables[[1]]

glimpse(stadiums_raw)
# look at Name, Capacity, and Distance to center field. what's in them that
# isn't a name or a number?

#Name has extra symbols like ‡. Capacity has commas and footnotes like [1],  Distance to center field has text like "feet", metric conversions, and footnotes.

# clean it. keep the originals; make new columns.
# the park name, without the symbols:

stadiums_raw |>
  mutate(park = str_squish(str_remove_all(Name, "[‡†]"))) |>
  select(Name, park)

# what does [‡†] match? (what does [ ] mean in regex?)
# [‡†] matches either the ‡ or † symbol. The brackets mean match any one character inside them.


# now make stadiums with: park, team, capacity (a number), opened (an
# integer), center_ft (a number), and roof.
# (hint: parse_number() pulls the first number out of text like "41,900" or
#  "400 feet (122 m)")
stadiums <- stadiums_raw |>
  mutate(
    park = str_squish(str_remove_all(Name, "[‡†]")),
    team = Team,
    capacity = parse_number(Capacity),
    opened = as.integer(Opened),
    center_ft = parse_number(`Distance to center field`),
    roof = `Roof type`
  ) |>
  select(park, team, capacity, opened, center_ft, roof)

glimpse(stadiums)
# did R give you a warning? don't skip it. which parks got NA, and what does
# their original cell say? (hint: go back to stadiums_raw)

# if you ignored that warning, what would happen when you look for the oldest
# park later? (this is how the last intern got fired.)

#If we ignored the warning, three stadiums would have missing opening years. We could miss Wrigley Field when looking for the oldest stadium.

# look at the Team column too. is anything stuck to the names?
#some team names have [nb 3]

# fix both. thursday's footnote pattern will help with one of them.
# check that nothing is NA anymore.

stadiums <- stadiums_raw |>
  mutate(
    park = str_squish(str_remove_all(Name, "[‡†]")),
    team = str_squish(str_remove(Team, "\\[.*\\]")),
    capacity = parse_number(Capacity),
    opened = as.integer(parse_number(Opened)),
    center_ft = parse_number(`Distance to center field`),
    roof = `Roof type`
  ) |>
  select(park, team, capacity, opened, center_ft, roof)

glimpse(stadiums)
# now you have two tables from two places. the editor wants one.

# join stadiums to teams_2026 by park name. which parks DON'T match?
# (hint: anti_join, and join_by(park == venue))


# why didn't they match? look closely at both spellings.
#The two parks didn't match because their names are written differently in Wikipedia and the MLB API.

stadiums |>
  anti_join(teams_2026, join_by(park == venue)) |>
  select(park, team)

# now try team name instead. which don't match? all 30 teams match 
stadiums |>
  anti_join(teams_2026, join_by(team == name)) |>
  select(park, team)

# which key would you trust, and why? what would you do with the misses?
# write your decision in the README.
#I would trust team names more than park names for this join because stadium names can be spelled differently between sources.

# make parks: every 2026 team with its park's details. how many rows? 30
# should it be 30? yes, 30 MLB teams 

parks <- teams_2026 |>
  left_join(stadiums, join_by(name == team))

glimpse(parks)

nrow(parks)

parks |>
  filter(is.na(opened))

# time to answer the editor.00

# what are the three oldest parks? the three newest? three oldest in order olderst to younger are Fenway Park, then Wrigley, then dodger stadium
#Three newwest in order from youngest to older are Globe Life Field, Truist park, and loanDepot park

parks |>
  arrange(opened) |>
  select(park, opened) |>
  slice_head(n = 3)

parks |>
  arrange(desc(opened)) |>
  select(park, opened) |>
  slice_head(n = 3)
# how many teams play in a park that opened after 2000? 13

parks |>
  filter(opened > 2000) |>
  count()

# the Athletics are playing in a minor league park while a Las Vegas
# stadium is built. what does "current stadium" mean, and who decides? how
# would your answers change if you'd scraped this page a year ago?

#current stadium is where a team is playing right now, even if temporary.  Wikipedia editors decide what is listed,
#If we scraped the page a year ago, the stadium information could be different because teams can move cities or change stadiums


# save your final table. this is what you hand the editor.
# (raw data goes in data/raw/. data you made goes somewhere else.)

write_csv(parks, "data/processed/mlb_parks_2026.csv")

# why keep it separate from data/raw/?



# last thing before you push: finish your README. the editor is going to hand
# it to a fact-checker.