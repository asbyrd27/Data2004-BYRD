# text 1: what is a document? 
library(tidyverse)
# install.packages(c("sotu", "tidytext"))
library(tidytext)
library(sotu)

# so we're going to think about data as words for the next week or so. but a little 
# different than we were using strings or regex (although that's useful here!)

# we're going to use that sotu package above to give us State of The Union's! 
# we're specifically going to work with every sotu address from 1790-2020. 

# we've had a common question all semester: what does one row represent?
# text makes this difficult! is it a speech? a paragraph? a sentence?
# a single word? 

# we'll have to do quite a few things to turn the addresses into something meaningful 
# and we'll start with some vocab words :)

## corpus: the whole collection of texts; all 240 addresses in this instance 
## document: one unit of text we treat as a whole; one address in this instance 
## metadata: information about a document that isn't in the text itself 
### can you think of something about a sotu address that might matter beyond the address itself? 
## token: one piece of text after we split it up; today we'll start with one word. 

# before we start, let's think a few questions through: 
# how often do presidents talk about freedom? 
## what would you count? how many times he says freedom
## how would you determine the rate? president a vs president b say freedom 
## x and y times per *what*?
## and would it be a fair comparison to put washington against obama? no, time period would matter, written vs aired lived, 

# let's get started!!!!!!
# sotu gives us two things 

glimpse(sotu_meta)
length(sotu_text)
glimpse(sotu_text)

# what is sotu_meta? info ab each address, includes presiden, party, how it was delivered, etc
#what is sotu_text? sotu_text contains the actual text of all 240 addresses
# how do we know what speech goes with which president?  The two datasets are in the same order.

# if we look at the package, it says they "line up", meaning it's just in order 
# the first speech is the first row, so on and so forth. nothing else conects them. 

# let's take a look at the first row of metadata and the first 300 characters of the first speech
sotu_meta |>
  slice(1)

str_sub(sotu_text[1], 1, 300)


# do the same for the last row of each. who gave it?
sotu_meta |>
  slice(240)

str_sub(sotu_text[240], 1, 300)

# let's put them together. since sotu_text comes from the pacakge and we haven't 
# saved it as an object in our environment, we can just use mutate for now :)
sotu <- sotu_meta |> 
  as_tibble() |> 
  mutate(text = sotu_text)

glimpse(sotu)

# now, what does one row of sotu represent?  one complete State of the Union address including the text and its metadata like
#the president, year, party, and type of address.

# but what would have happened if someone had sorted sotu_meta by present 
# before we ran that? 
sotu_meta |>
  arrange(president) |>
  mutate(text = sotu_text) |>
  slice(1) |>
  mutate(text = str_sub(text, 1, 200))

# is that correct? no its Abe Lincoln

# so every document needs an ID that belongs to *it*, not it's position. 
# what should we use as the ID? X is primary key


# and then let's just rename it to doc_id

sotu <- sotu |>
  rename(doc_id = X)

# now that we have that, we need to break this down a bit more. 
# every cell holds an entire speech, and we just can't really do anything with this
# we need to split these addresses up! 

# this is called tokenizing: splitting text into pieces (tokens). a token 
# can really be anything we're working with: a specific character, a word, 
# a sentence, a run of words, the works. we'll start simple with just one word :)
# doing this down here so y'all can't immediately see it above lol

# we're going to split this up using unnest_tokens() from tidytext. 
sotu_words <- sotu |> 
  unnest_tokens(word, text)

nrow(sotu_words)
glimpse(sotu_words)

# we lost our original 'text' column, though. why do you think? 

# and then look at the rest of the columns. everything repeats on every single word. 
# let's just keep two columns. which two can we keep that would let us get 
# everything else back pretty easily? do it below 

sotu_words <- sotu_words |>
  select(doc_id, word)

glimpse(sotu_words)

# and now that we have it broken down by word, we can start to work with this 
# a bit more! take a guess: what do you think the 3 most common words all 
# across all 240 addresses? 
sotu_words |> 
  count(word, sort = TRUE)

# we'll learn how to deal with those on Thursday :)

# but we should try to address something else. let's take a look at the first 
# 20 words, without sorting. 

sotu_words |>
  slice_head(n = 20)

# what do you notice? its the same order

# in washington's speech, did he say "Fellow Citizens" or "Fellow-Citizens"?

# unnest_tokens() did quite a few things for us that we talked about with strings
# and we talked then about how a lot of those decisions are really contextual! 
# let's use "U/union" as an example. 
# do it below. you'll use unnest_tokens(), but you'll add 'to_lower = FALSE'
# in that call.

sotu |>
  unnest_tokens(word, text, to_lower = FALSE) |>
  filter(word %in% c("Union", "union"))|>
           count(word)
  
glimpse 
# so in other words: 
## take sotu and unnest text to word, without case normalization 
## filter for Union or union 
## and then count 


# what do we get? 
# should we collapse "Union" and "union" No, they could mean diff things

# this all lets us come back to our original question: 
# how often do presents talk about freedom? 

# let's start by just counting how long each speech is. 
# create an object called doc_lengths and count the words in each document 

doc_lengths <- sotu_words |>
  count(doc_id)

doc_lengths

# and now we have a table that just has doc_id and the count of words. 
# how do we get the president, year, and type back? 
# and we want to preserve the grain of doc_lengths

doc_lengths <- doc_lengths |>
  left_join(
    sotu |> select(president, year, sotu_type, doc_id),
    join_by(doc_id))

doc_lengths
# now that we have that, compare speeches vs written messages. 
# which ones are longer? and by how much?

doc_lengths |>
  group_by(sotu_type) |>
  summarise(
    sample_size = n(),
    avg_length = mean(n),
    med_length = median(n)
  )



# and now let's finally count "freedom" in each document!
# create an object called freedom where we only include rows where 
# the word is "freedom" and then let's count them! 

freedom <- sotu_words |>
  filter(word == "freedom") |>
  count(word, name = "freedom")

freedom


# how many rows did we get? 

glimpse(freedom)

# but how many documents are there? what happened?



freedom |>
  group_by(doc_id)|>
  



# a document that doesn't say "freedom" is still a document! we shouldn't drop 
# those documents or treat them as missing. so we need to do another join. 
# let's create an object called freedom_docs, we'll preserve the grain of 
# doc_lengths. we need to join our freedom object onto it, but we don't want 
# to have NA values where we have documents that don't say "freedom" in them. 
# so we'll want to mutate(...replace_na(freedom, 0))


freedom_docs <- doc_lengths |>
  left_join(freedom, join_by(doc_id)) |>
  mutate(freedom = replace_na(freedom, 0))
  
freedom_docs


# and we still need to account for the different sizes, so let's do a per_1000 
# words rate as well 
freedom_docs <- freedom_docs |>
  mutate(rate = freedom / n *1000)

glimpse(freedom_docs)

# and now finally, let's make a plot! i want to see the rate on the y, year on the x, 
# and color by sotu_type 

okabe_ito <- unname(palette.colors(palette = "Okabe-Ito")[2:8])


freedom_docs |>
  ggplot(aes(x = year, y = rate, color = sotu_type)) +
  geom_point() + 
  scale_color_manual(values = okabe_ito) +
  labs(title = "Use of Freedom in SOTU Addresses",
       subtitle = "Freedom Increases over time, in speeches")
       
  
  
  
