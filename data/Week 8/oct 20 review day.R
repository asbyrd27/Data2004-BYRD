# packages 
library(tidyverse)
library(httr2)

# we will be working on the API for the Art Institute of Chicago, they have an API for 
# the whold collection. 
# We need to start with the documentation instead of finding things the hard way 
# like the MLB weekly asked you to do. 

# we'll start by taking a few minutes, 5-6 minutes maybe:
# pair up, go to: https://api.artic.edu/docs/

# i want y'all to find 6 things, i'll call on groups to do each: 
# artworks endpoint--/api/v1/artworks
# default number of records-- 12 records per page 
# the maximum -- 100
# how to ask for only some fields-- fields=id
# the license, who is allowed to use these data-- anyone
# how the API asks you to behave, e.g., limit requests; look for "throttled" and "User-Agent"

# now that we've done that, we need to do the request. in general, you will always 
# need to do this before actually starting to work with any API. 

# so we're going to start with a specific request to get us started: 
# https://api.artic.edu/api/v1/artworks?limit=3&fields=id,title,artist_title,date_start,date_end,term_titles

# break down the link again like we did last week: 
# the server? https://api.artic.edu
# endpoint? /api/v1/artworks
# what marks where the details of our request start? question mark
# change the number of artworks given in a request? limit=3
# and also? and (&)
# parameters? fields=id,title,artist_title,date_start,date_end,term_titles

# paste that full URL into your browser
# then paste that url but erasing &fields=...
# and then go back to the original url on line 25 

# let's pay attention to that json file again 
# { } = object, a set of named things, look them up by name, usually becomes columns
# [ ] = array, a sequence of unnamed things. look things up by position. usually becomes rows

# where are the { } and where are the [ ]?
#{} are used for unnamed list
#[] are used for  named list

# If you look at the json file in your browser, the response is a tree
# DON'T LOOK DOWN FOR THE ANSWERS! 










# { <- object, names everything here 
# parts 
#   "pagination": {...}      <- object: 6 named facts about paging
#   "data": [             <- array: a list of 3 artworks
#     {            <- object: this is the first artwork, with named fields
#      "id": 89262
#      "title": "Half pint covered wine measure"
# blah blah blah 

# we only care about two [ ] 
# data : identifies a row 
# term_titles: a list inside each artwork, we probably need to unnest at some point. 

# license_links is also a [ ], but it's inside info. we aren't turning info into a table. 
# just to help you keep in mind that we only rectangle things that we need :)

# now let's do our request in R
art_request <- request("https://api.artic.edu/api/v1/artworks") |> 
  req_url_query(
    limit = 3,
    fields = "id,title,artist_title,date_start,date_end,term_titles"
  ) |> # they want us to identify ourselves, remember. we can add a header to tell them who we are
  req_headers(`AIC-user-agent` = "Aiden Byrd (asbyrd27@transy.edu")
  
# let's do a dry run and test it 
art_request |> 
  req_dry_run()

# if it worked, let's perform the request 
art_response <- art_request |> 
  req_perform()

resp_status(art_response)

# now do the request again, but change the limit to 101, and then run. 

art_request_bad <- request("https://api.artic.edu/api/v1/artworks") |>
  req_url_query(
    limit = 3,
    fields = "id,title,artist_title,date_start,date_end,term_titles"
  ) |>
  req_headers(`AIC-user-agent` = "Kay Hales (khales@transy.edu")

art_request_bad |>
  req_perform()



# what happened? why? what does the documentation say? 
#the documentation says the maximum is 100 per request

# then let's see the server's explanation: 
last_response() |> 
  resp_body_string()

# now let's dig into it, let's take a look at the json file 
art <- art_response |> 
  resp_body_json()

art$pagination

# what does that tell you? -- There are 133,170 artworks total, but we only requested 3.We are on page 1, and there are 44,390 pages.
# Offset is 0 because we didn't skip any artworks.

# now let's save it so we can work from the file and not the request 
# we're going to work with 25 artworks this time 
request("https://api.artic.edu/api/v1/artworks") |>
  req_url_query(
    limit = 25,
    fields = "id,title,artist_id,artist_title,date_start,date_end,term_titles"
  ) |>
  req_headers(`AIC-User-Agent` = "Aiden Byrd (asbyrd27@transy.edu)") |>
  req_perform(path = "data/raw/aic_artworks.json")

# check your data/raw folder. 
# we're working with that from now on
aic <- jsonlite::read_json("data/raw/aic_artworks.json")

# we need to reshape this, but we need to actually look at it first. 
str(aic, max.level = 1) 

# so at the top of our hierarchy, we have 4 levels: pagination, data, info, config

# let's figure out which part holds the artworks. we talked about this above: 
str(aic$data[[1]], max.level = 1)

# why is artist_id NULL? 
str(aic$data[[1]]$term_titles)



# we can make it a table now. since we know aic$data is where we can identify rows, we'll start theme 
art_tbl <- tibble(art = aic$data)
art_tbl

# we have 25 <named list [7]> just like our limit = 25 above! 
# so those are our rows. 
# <named list[7]> and did { } or [ ] mean soemthing is named?  

# let's make these wider to open the list up 

art_wide <- art_tbl |> 
  unnest_wider(art)

art_wide

# for you: show me which artworks have no artist. 

art_wide |>
  filter(is.na(artist_id)) |>
  select(title, artist_id, artist_title, date_start, date_end)

# do you think that this is random? 
# look at Plum Vase. what does 12 mean for those two dates? 

# now let's give term titles their own row per title. 
art_terms <- art_wide |> 
  unnest_longer(term_titles)

nrow(art_terms)

# did any of our titles repeat per artwork? 
art_terms |>
  count(id, term_titles) |>
  filter(n > 1)

# check that below. 

# and how would we decide to only keep one observation per title per artwork?

art_terms_clean <- art_terms |>
  distinct(id, term_titles)

art_terms_clean
nrow(art_terms)
