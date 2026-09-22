# pacakges
library(tidyverse)

# let's start with the same persons_core and crashes_core 
crashes <- read_csv("data/raw/crashes.csv")
persons <- read_csv("data/raw/person.csv")
glimpse(persons)
glimpse(crashes)

crashes_core <- crashes |> 
  select(
    COLLISION_ID,
    `CRASH DATE`,
    BOROUGH,
    `NUMBER OF PERSONS INJURED`,
    `NUMBER OF PERSONS KILLED`
  )

persons_core <- persons |> 
  select(
    UNIQUE_ID,
    COLLISION_ID,
    PERSON_TYPE,
    PERSON_INJURY,
    PERSON_AGE,
    PERSON_SEX
  )

glimpse(crashes_core)
glimpse(persons_core)

# let's do a brief review of our mutating joins :) 

## what are the primary keys? what about the foreign key? 

## what are our mutating joins? what's the difference? 
#left join-keeps every row from the left, gets matched up from the right table 
#inner join-keeps only rows that have a match in both tables
## let's check out the homework briefly. 

# Which crashes involved at least one bicyclist? I want one row per crash. 
# we'll start by making a table of just the bicyclist person records. how many are there? 6228
bicyclists <- persons_core |> 
  filter(PERSON_TYPE == "Bicyclist")

nrow(bicyclists)
n_distinct(bicyclists$COLLISION_ID) #6016-mulitple bikes or tandom bikes

crashes_bike <- crashes_core |> 
  left_join(bicyclists, join_by(COLLISION_ID))

crashes_bike <- crashes_bike |>
  filter(PERSON_TYPE == "Bicyclist")
# does that number answer our question? why not? 

# if it doesn't, which join should we reach for?

# use nrow() on the join and n_distinct() on that join's collision ID. Why are they different? 

# we can answer this by thinking about the grain.
# we're joining persons to the crashes grain, so what does one row represent? 

# is it every crash involving a bicyclist? let's check out the first 10 rows. 
crashes_bike|>
  slice_head(n = 10)

# our mutating join adds columns so it has changed our grain, but we don't want it to right now. 

# so we'll need to use *filtering* joins
# we got exposed to one filtering join already: anti_join(). 
# which filtering join that will keep matches instead of non-matches?
bike_crashes <-crashes_core |> 
  semi_join(bicyclists, join_by(COLLISION_ID))

nrow(bike_crashes)
n_distinct(bike_crashes$COLLISION_ID)

glimpse(bike_crashes)
# this doesn't add more columns, so we're not working with crash-bicyclists combination

# now do anti_join for crashes that do not involve a bicyclist. 

no_bike_crashes <-crashes_core |> 
  anti_join(bicyclists, join_by(COLLISION_ID))
nrow(no_bike_crashes)
n_distinct(no_bike_crashes$COLLISION_ID)

# what should the nrow() of each of your filtering joins dataframes be?
nrow(bike_crashes) + nrow(no_bike_crashes)
nrow(bike_crashes) + nrow(no_bike_crashes) == nrow(crashes_core)
# now it's y'all's turn: identify crashes that involve at least one pedestrian, 
# one row per crash. 

pedestrians <- persons_core |> 
  filter(PERSON_TYPE == "Pedestrian")

nrow(pedestrians)
n_distinct(pedestrians$COLLISION_ID)

crashes_with_pedestrian <- crashes_core |> 
  semi_join(pedestrians, join_by(COLLISION_ID))
nrow(crashes_with_pedestrian)

## after that, narrow it down. crashes where at least one pedestrian was recorded as female. 

female_pedestrians <- persons_core |> 
  filter(PERSON_TYPE == "Pedestrian", PERSON_SEX == "F")

nrow(female_pedestrians)
n_distinct(female_pedestrians$COLLISION_ID)

crashes_with_female <- crashes_core |> 
  semi_join(female_pedestrians, join_by(COLLISION_ID))

nrow(crashes_with_female)
# back together
## where did you put the PERSON_SEX condition? why? Inside the filter on persons_core and combined with the person type= pedestrain, so that female pedestrains nly contains records that are both a pedestrian and female
