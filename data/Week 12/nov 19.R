# text 2

library(tidyverse)
library(tidytext)
library(sotu)

# let's just get ourselves back to where we were, this time i made sure there weren't a bunch 
# of mistakes :)
sotu <- sotu_meta |>
  as_tibble() |>
  mutate(text = sotu_text) |>
  rename(doc_id = X)

sotu_words <- sotu |>
  unnest_tokens(word, text) |>
  select(doc_id, word)

doc_lengths <- sotu_words |>
  count(doc_id, name = "n_words") |>
  left_join(sotu |> select(doc_id, president, year, sotu_type),
            join_by(doc_id))

freedom <- sotu_words |>
  filter(word == "freedom") |>
  count(doc_id, name = "freedom")

freedom_docs <- doc_lengths |>
  left_join(freedom, join_by(doc_id)) |>
  mutate(freedom = replace_na(freedom, 0))

# and let's make our visual one more time 
okabe_ito <- unname(palette.colors(palette = "Okabe-Ito")[2:8])

freedom_docs <- freedom_docs |>
  mutate(per_1000 = freedom / n_words * 1000)

freedom_docs |>
  ggplot(aes(x = year, y = per_1000, color = sotu_type, shape = sotu_type)) +
  geom_point(alpha = 0.7) +
  scale_color_manual(values = c("speech" = okabe_ito[1],
                                "written" = okabe_ito[5])) +
  labs(x = NULL, y = "\"freedom\" per 1,000 words",
       color = NULL, shape = NULL) +
  theme_minimal()

# let's remind ourselves which words were the most common across the speeches: 
sotu_words |> 
  count(word, sort = TRUE)

# we obviously don't want those words. they don't tell us really anything in the slightest 
# about anything at all! # we don't necessarily care about every single combination 
# of letters that *could* create any words in English. we care about words and combinations
# of words that actually tell us something. 

# so we can split this into two questions: 
## what words make a president sound like that president? 
## is there a word that every president says, but still tells us something?

# and then we can look at our plot and ask about the frequency of words. 
# this is called "term frequency". this will matter more later :) 

# now let's work on stop words. 

# stop words are those words like "of" "and" etc 
# tidytext actually gives us a convenient list of them 
stop_words

stop_words |> 
  count(lexicon)

# we can dig through those and see more if we want, but i'll give us a jump start 
stop_words |> 
  filter(word %in% c("we", "our", "us", "we're"))

# are these worrds that you can imagine a president using on purpose? 


# does it depend on the topic? 

# of course it does! that's everything we do in here :) 
# we might not want to get rid of those words if our question was about how presidents 
# talk to the country. 

# is there anything else that you notice about the words that i chose to look at above? 
# here's a hint: think about what unnest_tokens() did on tuesday. 


# maybe we can come back to that later, but let's go ahead and start. 
# let's create an object called words_clean that keeps all the words in sotu_words 
# that *don't* appear in stop_words. 
words_clean <- sotu_words |>
  anti_join(stop_words, join_by(word))


# and how many rows did that remove? 
nrow(sotu_words) - nrow(words_clean)

# so what are the most common words now that we've removed the stop words?
words_clean |> 
  count(word, sort = TRUE)

# these make a whole lot more sense now. but do you think this is good enough to stop for the day? 

# no!!!! we don't even know who said what! 

# let's get back to work! let's make out document one president rather than one address 
# so wer're going to put all of his sotu addresses together. 

# we need to add doc_id and president to our words_clean object. let's retain the grain of words_clean. 

words_clean <- words_clean |>
  left_join(doc_lengths |> select(doc_id, president),
            join_by(doc_id))


words_clean <- words_clean |>
  left_join(sotu |> select(doc_id, president),
    join_by(doc_id))

glimpse(words_clean)

# and now we can count the words by presidents and call it pres_words :)

pres_words <- words_clean |>
  count(word)




# great, but that's ugly. let's make this a bit clearer. 
# look at the top 5 words for lincoln, kennedy, reagan, and obama

pres_words |>
  filter(str_detect(president, "Lincoln|Kennedy|Reagan|Obama")) |>
  group_by(president) |>
  slice_max(n, n = 5) |>
  arrange(president, desc(n))

pres_words |>
  filter(president %in% c("Abraham Lincoln",
                          "John F. Kennedy",
                          "Ronald Reagan",
                          "Barack Obama")) |>
  group_by(president) |>
  slice_max(n, n = 5) |>
  arrange(president, desc(n))


# and then let's just take a quick digression. right now there's one row per president per word. 
# but we could spread it out differently. we could do one row per president, one column per word. 
# that's called a "document term matrix", we won't be messing with this, but there are some 
# tools you might need in the future that require this 
pres_words |>
  filter(str_detect(president, "Lincoln|Kennedy|Reagan|Obama"),
         word %in% c("freedom", "war", "slavery", "jobs")) |>
  pivot_wider(names_from = word, values_from = n, values_fill = 0)


# but now let's get into the meat of the day! 
# we talked briefly about term frequency earlier, let's shorten that down to "tf"
# we are going to focus on tf-idf 


# tf (term frequency): what we've already done, how often a word shows up for a president as a share 
# of all of his words, think the rate variable we made earlier 

# idf (inverse document frequency): this is how *rare* the word is across presidents. 
# a word gets a 0 if every single president uses them. 

# tf-idf is the frequency * inverse document frequency. it's high when a word is common 
# for a specific president *and* rare for everyone else.  

# we can do that with bind_tf_idf() 
pres_tfidf <- pres_words |> 
  bind_tf_idf(word, president, n)

# what words have an idf of 0? 
pres_tfidf |> 
  filter(idf == 0) |> 
  distinct(word)

# let's use the 4 presidents from above. 
four <- pres_tfidf |>
  filter(str_detect(president, "Lincoln|Kennedy|Reagan|Obama")) |>
  group_by(president) |>
  arrange(desc(tf_idf)) |> 
  slice_head(n = 8)

four
# and finally, let's plot them :) 
# words on the y axis, tf-idf on the x axis, and then we'll do a panel of our 4 presidents 
# with one panel for each president. 
# we'll also do two things: 
# you've done fct_reorder() before, but we need to use a related, but distinct, function 
# reorder_within(word, tf_idf, president). 

four |>
  ggplot(aes(x = tf_idf, y = word )) +
           geom_bar() + 
           facet_wrap(~ president, scales = "free_y") +
           scale_y_reordered() +
           labs(x = "TF-IDF",
                y = "Word")
           
         


# which words tell us the most about each president? 
# and what do the words tell us about the politics of the time? 