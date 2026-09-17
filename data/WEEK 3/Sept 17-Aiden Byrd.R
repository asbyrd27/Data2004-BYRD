library(tidyverse)

crashes <- read_csv("data/raw/crashes.csv")
persons <- read_csv("data/raw/person.csv")
glimpse(crashes)
glimpse(persons)
# What does one row represent in each table?in crashes, one row is a vehicle crash, in persons its a person involved in a crash 

# What variable appears in both? Does it do the same job in both? collision id is in both but in crashes, its the primary key, in persons it is a foriegn key 


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

# You work for the NYC Department of Transportation and you have been given a media request:
# How do person-level injury outcomes compare across boroughs?

# A PRIMARY KEY uniquely identifies a row in its own table.
# A FOREIGN KEY points at another table's primary key.

# How should we figure out which one COLLISION_ID is?
crashes_core |>
  count(COLLISION_ID)|>
  filter(n > 1)

persons_core |>
  count(UNIQUE_ID)|>
  filter(n > 1)

persons_core |>
  count(COLLISION_ID)|>
  filter(n > 1) |>
  arrange(desc(n))

# Why does COLLISION_ID repeat in the person table?

# Could we try to learn something about that wreck with 52 people in it? Let's use the persons df

# Do we have any missing keys?
crashes_core |>
  filter(is.na(COLLISION_ID))
persons_core |>
  filter(is.na(UNIQUE_ID))
persons_core |>
  filter(is.na(COLLISION_ID))

# Cardinality

# Cardinality is how many rows on each side can share a key value.
## one-to-one: each key appears once in both
## one-to-many: unique on the left, repeats on the right
## many-to-many: repeats on both sides


# Which one do we have? What does that tell you about what a join will
# do to our rows? #one to many, one car/vehicle to many people 
nrow(crashes_core)
nrow(persons_core)
n_distinct(persons_core$COLLISION_ID)

# Two of those numbers are close but not equal. What does that difference
# tell you before we join anything?

# inner_join = rows that matched in both
# left_join = every row in the LEFT table, matched or not

crashes_inner <- crashes_core |>
  inner_join(persons_core, join_by(COLLISION_ID),
             relationship = "one-to-many")
glimpse(crashes_inner)

persons_inner <- persons_core |> 
  inner_join(crashes_core, join_by(COLLISION_ID))
glimpse(persons_inner)             

#one row represents one crash w person-level crash info PRIMARY
#one row represents one person involved in a matched crash SECONDARY

            
crashes_left<- crashes_core|>
  left_join(persons_core, join_by(COLLISION_ID))
glimpse(crashes_left)  
nrow(crashes_inner) #317941
nrow(crashes_left) #318319

# COVERAGE is which rows on each side found a match. The table got
# bigger, so nothing was lost, right?

crashes_core |>
  anti_join(persons_core, join_by(COLLISION_ID)) |>
  nrow()
persons_core |>
  anti_join(crashes_core, join_by(COLLISION_ID)) |>
  nrow() #ppl without crashes

##### Your turn #####

# Build a person-level table that includes borough.

# Before you write anything:
## What should one row represent when you're done?
## Which table goes on the left? persons

persons_borough <- persons_core |>
  left_join(
    crashes_core,
    join_by(COLLISION_ID))
glimpse(persons_borough)

persons_core |>
  anti_join(crashes_core, join_by(COLLISION_ID))
crashes_core |>
  anti_join(persons_core, join_by(COLLISION_ID))
# Then:
## Join them.
## Declare the cardinality with relationship = and see if R agrees.
## Use anti_join() to look at whatever didn't match.
## Count records by BOROUGH, PERSON_TYPE, and PERSON_INJURY.
persons_borough |>
  count(BOROUGH, PERSON_TYPE, PERSON_INJURY)

# Some of those rows will have no borough. Before you filter them out:
# how many are there, and are they all missing for the same reason? 103229
persons_borough |>
  filter(is.na(BOROUGH)) |>
  count()
crashes_core |>
  filter(is.na(BOROUGH)) |>
  nrow()
#One row represents one person involved in a crash. persons goes on the left, giving a many-to-one relationship with crashes. There are 103,229 people with no borough.
