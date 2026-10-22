# review: APIs, unnesting, hoist, strings, regex, joins

library(tidyverse)
library(httr2)

# the Art Institute of Chicago wants to do some info sessions on their artworks. 
# however, they for some reason given everyone that knows anything about the artworks
# the week off. 

# i am the director, you're all my temps and I need y'all to get this right before 
# i lose my job as the director :(

# when y'all show up to work, you find a sticky note on your desk: 

# 1. pull the records for the 25 pieces on tuesday's tour. but only give me the
# info i actually need 


# 2. clean it up, whenever the last temps pulled it the last time i gave everyone 
# the day off, it was ugly and i had no idea what i was looking at. 

# 3. the gallery labels need each artist's nationality and their dates. 
# they're crammed into one field somewhere, idk where. find them and dig them out. 

# 4. visitors always ask about the artists. that info is all on a different system
# connect them somehow? DONT DO 

# 5. and then, as a favor for me: i'm supposed to be the one giving the tours. 
# but to be honest i don't know anything about these artists, idk why they expect me to. 
# for the favor, i need you to tell me just what i can and can't tell visitors. 

# don't break anything. good luck. my job depends on you. hope you enjoy your first day 
# at the art institute of chicago :)


# from this moment onward, this is your internal monologue. for example, your first thought is 
# "my data2004 professor, Kay Hales, always had such creative and fun prompts. now that i make 
# a ton of money as a data scientist, i think i'm going to send them $5000". 

# your first instinct: probably should ask the API. is it still down? 
request("https://api.artic.edu/api/v1/artworks") |>
  req_url_query(limit = 1, fields = "id") |>
  req_timeout(30) |>
  req_perform()

# what does that mean? the API timed out and zero bytes were recieved

# thankfully, the people who were already working on this had the tour today already 
# pulled and stored in a json file! 
aic <- jsonlite::read_json("data/raw/aic_artworks.json")

# we can't check the API documentation. you know you should still look at the structure 
# of the json file. 

# how do you check the top level of the hierarchy for the entire file? hint: the STRUCTURE of the json file
str(aic, max.level = 1)

# and which of those do you think holds our artworks? The data section holds our 25 artworks.

# with that, go ahead and check out the structure of the first element of the list 
# containing the artworks. 

str(aic$data[[1]], max.level = 1)

# now you can answer: 
## which fields are single value? id, title, date_start, date_end, artist_id, artist_title, 
## which one is a list? term_titles is a list of 21
## named or unnamed? term_titles is an unnamed list because it contains individual terms rather than named fields.

# you know the director doesn't understand how to use a json file, so you need to clean it up. 
# you think "this should be one row per piece, one column per thing they need"

# create a tibble, called art_tbl, that takes your list of 25 artworks from where you located it above
art_tbl <- tibble(art = aic$data)

art_tbl

# how many rows? what does each cell say? 25 rows,  Each cell says <named list [7]> because each artwork contains 7 named fields.

# you remember that named parts become columns. create an object called art where 
# art becomes one column per field. 
# (if you're stuck, go to chapter 23 of R4DS. you are turning *named* lists into *columns*)

art <- art_tbl |>
  unnest_wider(art)

art

# if you only want to grab a few things, you know that you can use hoist(). 
# use hoist() on art_tbl to pull out just id, title, artist title, and date_start. 

art_tbl |>
  hoist(art,
        id = "id",
        title = "title",
        artist_title = "artist_title",
        date_start = "date_start") |>
  unnest_wider(art) |> 
  unnest_longer(term_titles)

# term_titles is still a list. named or unnamed? so which unnest? term_titles is an unnamed list because it contains unnamed values
#We use unnest_longer to make one row per artwork-term pair.

# make art_terms with one row per piece term pair. 

art_terms <- art |>
  unnest_longer(term_titles) 

art_terms

# the director wants: title, artist, and dates for the gallery labels. 

art |>
  select(title, artist_title, date_start, date_end) |>
  print(n = 50)

# look at the columns. you read the artist column carefully before printing any labels. 
# would you put every one of theses on a label as "the artist"? No,  because sometimes it describes a nationality rather than an actual person's name
# are there any that aren't like the others? English really sticks out, likely the artist is unknown

# then you start by making a date label by gluing start and end together. 
# you remember that you read about 'str_c' at one point
# and that you can str_c(col1, "symbol", col2) and connect col1 and col2 with the symbol
# you create date_label in a new object called labels

labels <- art |>
  mutate(date_label = str_c(date_start, "-", date_end))

labels |>
  select(title, artist_title, date_label) |> print(n=25)

# you know you need to check out the new date_label column. there are ugly ones. 
# you know if a piece was made in one year, the label should just say that year. 
# the label should only use a range when there are a range of years. 

labels <- labels |>
  mutate(date_label = if_else(date_start == date_end,
                         as.character(date_start),
                         date_label))

labels |>
  select(title, artist_title, date_label) |>
  print(n = 25)

# you know you can do most of what the director wants can be found in patterns. 

# the director wants to have the pieces laid out thematically (even if those themes aren't very good)

# first, use regex to grab only the titled for unnamed sitters (Portrait of a...)

labels |>
  filter(str_detect(title, "^Portrait of a "))

# use regex to find the artist name with a period in it. 

labels |>
  filter(str_detect(artist_title, "\\."))

# then use regex to find the portraits that are of women. 

labels |>
  filter(str_detect(title, "Lady|Girl|Woman|Miss|Madame|Countess|Mary"))

# what about ones that contain "Man"?

labels |>
  filter(str_detect(title, "Man"))

# the director wants a tour of men. you use the art_terms object and notice "19th century" is written more than one way. 
# how many ways? 

# in art_terms still: find every term with "man" in it
# did anything make it in that you don't want? 

# then use anchors, boundaries, and stricter patterns. 