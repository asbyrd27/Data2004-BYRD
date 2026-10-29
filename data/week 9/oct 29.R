# web scraping 
library(tidyverse)
library(httr2)
library(rvest)

# let's mess around with Wikipedia today :) paste this link in your browser: 
# https://en.wikipedia.org/wiki/List_of_Cincinnati_Reds_seasons
# scroll around, get familiar with it before we start doing any coding. 

# and then before we keep going, we need to talk a bit about ethics. 
# is this Wikipedia page public? is is non-personal? is it factual? public yes, contains non-personal, and is factual

# now let's open https://en.wikipedia.org/robots.txt
# this isn't a law, but it's what is expected for bots. we aren't bots, hopefully. 
# since we aren't bots, does this apply to us? are we allowed? 
# but we should respect Wikipedia's instructions and preferences.

# then, ctr/cmd + f for "Folks get annoyed". what's that mean? 

# lastly, flip back to the main wikipedia page for the reds' seasons. 
# scroll to the very bottom. what license is the text under? CCA-S-A 4.0
# what do you owe wikipedia if you use it? just to cite the source

# let's get started!
# we'll start with some practice with html. 
# html is how web pages get built, and it's bascially a text with tags. 
tiny <- minimal_html('
  <h1>Reds</h1>
  <p class="note">Founded in <b>1882</b>.</p>
  <p class="note" id="nickname">Also called the <a href="/wiki/Redlegs">Redlegs</a>.</p>
  <p>Not a note.</p>
')

# the tags: 
# <h1>: biggest heading 
# <p>: a paragraph 
# <b>: bold 
# <a>: a link

# these tags also define elements. for example 
# <p> ... </p> 
# the element is the start tag, <p>, the end tag, </p>, and everything between them. 

# take a shot: what elements are in tiny? tiny has an h1 heading, three paragraphs, link and bold element 

# we also have ATTRIBUTES. an attribute is extra information inside the start tag
# for example, name="value". 
# what are three attributes in tiny? which element is each one in? 

#class=note 
#id=nickname
#href=/wiki/Redlegs






# two of our attributes are special: class and id
## elements can share a class 
## but only one element on a page should have a given id. 
# which elements have class "note"? which has id "nickname"? 

# class and id are usually used for styling and to help with scraping. 
# a class is like a team name. everyone on the reds shares the class team name. 
# but players don't share names or jersey numbers, for example

# we can find elements with CSS selectors. we have three kinds 

## we can do it by class name 
tiny |> 
  html_elements("p")

## we can do it by class by putting a dot in front
tiny |> 
  html_elements(".note")

## and we can do it by id by putting a # in front
tiny |> 
  html_elements("#nickname")

# how many elements did each one find? 

# and to clarify, we have two separate element selectors
tiny |> 
  html_element("p")

tiny |> 
  html_elements("p")

# what did those separately find? look back at tiny and tell me. element gives u the first times it shows up, elements shows all of them 

# and now let's get what's inside our elements 
tiny |> 
  html_elements(".note") |> 
  html_text2()

# this sets up the basic 2 steps of web scraping: 
# 1. find the elements
# 2: pull something out of them. html_text2() for text, html_attr() for an attribute. 

tiny |> 
  html_elements("a") |> 
  html_attr("href")

# before we move on to our larger application: get some practice. 
# get the text of h1
tiny |>
  html_elements("h1") |>
  html_text2()
# get the text of the bold element 
tiny |>
  html_elements("b") |>
  html_text2()

# get the text of ONLY the paragraph with id nickname
tiny |>
  html_elements("#nickname") |>
  html_text2()

# get the text of every paragraph. how many? which one isn't a note? 3 paragraphs, the third paragraph doesn't bc it doesnt hace class= notes 
tiny |>
  html_elements("p") |>
  html_text2()

# now let's apply this by focusing on the reds again. 

library(httr2)
library(rvest)
url <- "https://en.wikipedia.org/wiki/List_of_Cincinnati_Reds_seasons"

request(url) |>
  req_user_agent("Aiden Byrd, DATA 2004 class, Transylvania University") |>
  req_perform(path = "data/raw/reds_seasons.html")

# why do you think we immediately save it? does it being a wiki matter? becuase wiki can change at anytime 

# now let's read the file and work from there 

reds_page <- read_html("data/raw/reds_seasons.html")
# let's use our selectors. y'all first:
# first, what's the page title? 


tiny |>
  html_elements("h1") |>
  html_text2()
reds_page |>
  html_elements("title") |>
  html_text2()

# second, go back to the wiki and go down to the bottom where it says "This page was last edited"
# right click that text and then click inspect. 
# what's its id? use that to get that text here in here. 

reds_page |>
  html_elements("#footer-info-lastmod") |>
  html_text2()

# third, how many tables are on the page? 
reds_page |>
  html_elements("table") |>
  length()


# fourth, right click the big season table, click inspect. 
# what class does <table> have? how mayn tables have that class? 

# let's turn this into a tibble 
tables <- reds_page |> 
  html_elements("table.wikitable") |> 
  html_table()

# html_table() turns an HTML table into a table. if we had a bunch of tables, 
# we'd get a list of tibbles. 

length(tables)

# we'll use our indexing and positions again. we have a length of 5. so let's start with 
tables[[3]] 

# use that to find the season table. once you find it, call that seasons_raw
seasons_raw <- tables[[3]] 
glimpse(seasons_raw)
# now, look at seasons_raw. scroll all the way through it. 
# notice anything odd? anything we should note? no proper na's, repeating names, 1972 e??

# then, what type is the Season column? Wins? why? 
#season column is chr bc some years have footnotes, wins also is chr bc the row has non numeric values

# let's start cleaning by removing footnotes. let's use regex.
# something that will help us: ".*" = any character. 
# so: ".*+" means what exactly? 

# and then what was ^? and what was \\d? ^ start of the string, \\d is a digit 0-9

# let's look back at our table. season has footnotes stuck to it. 
# so we get thigns like "1972[e]". how should we remove them? 

seasons_raw |> 
  mutate(season = str_remove(Season, "\\[.*\\]"))

# so we're just asking for \\[ = an actual opening bracket 
# then we want .* so any character
# \\] = a literal closing bracket. 
# that lets us find [e]

str_remove("1972[e]", "\\[.*\\]") 
str_remove("1975", "\\[.*\\]")

# and then to be thorough, let's only keep seasons that have 4 numbers in the year. 
# how would you finish the code below: 
# you want season to start with a digit, contain only 4 digits, and then finish after 
# those four digits. 
seasons_raw |> 
  filter(str_detect(Season, "\\d{4}$")) |>
  print(n=146)


# so on your own: 
# finish what i started here. use the regex above and then create 
# columns that are integers that make season, wins, and losses all numeric columns. 
# then select season, league, division, finish, wins, losses, and playoffs. 

# how many rows? 140
# how many seasons is 1882 through 2026? 145 seasons
seasons <- seasons_raw |>
  mutate(season = str_remove(Season, "\\[.*\\]")) |>
  filter(str_detect(Season, "\\d{4}$")) |>
  mutate(
    season = as.integer(season),
    wins = as.integer(Wins),
    losses = as.integer(Losses)
  ) |>
  select(
    season,
    League,
    Division,
    Finish,
    wins,
    losses,
    Playoffs
  )
    
seasons
nrow(seasons)
