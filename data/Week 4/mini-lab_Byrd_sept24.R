# Lab 2

# We'll start by working with the actual crashes and persons fully 
library(tidyverse)

crashes <- read_csv("data/raw/crashes.csv")
persons <- read_csv("data/raw/person.csv")

# 1: Make a table of person records for female pedestrians 
persons_core <- persons |> 
  select(
    UNIQUE_ID,
    COLLISION_ID,
    PERSON_TYPE,
    PERSON_INJURY,
    PERSON_AGE,
    PERSON_SEX
  )

female_pedestrians <- persons_core |> 
  filter(PERSON_TYPE == "Pedestrian", PERSON_SEX == "F")

glimpse(female_pedestrians)
nrow(female_pedestrians)
## what does one row represent? One row represents one female pedestrian's involvement in one crash.

# 2: Keep only the crashes that involved at least one female pedestrian. 
# Keep COLLISION_ID, BOROUGH, and the five vehicle type columns. 

# how can we select every variable that starts with "VEHICLE TYPE CODE"? Use starts_with("VEHICLE TYPE CODE") inside select() so it matches every column whose name begins with that exact text, so you don't have to type out all five vehicle type column names individually.
crashes_female_ped <- crashes |> 
  select(COLLISION_ID, BOROUGH, starts_with("VEHICLE TYPE CODE")) |> 
  semi_join(female_pedestrians, by = join_by(COLLISION_ID))

nrow(crashes_female_ped)
# does one row still represent one crash? check it. 
n_distinct(crashes_female_ped$COLLISION_ID) == nrow(crashes_female_ped)

# why a filtering join instead of a mutating join? Because we only want to know which crashes involved a female pedestrian, not trying to attach any columns from persons_core (age/injury) onto the crash table
#mutating join would add those columns and could duplicate crash rows if multiple female pedestrians were in the same crash.

# 3: Right now the vehicle types are columns. We want one row per vehicle. 
# before writing your code, how many rows should we have? 24700
nrow(crashes_female_ped) * 5

vehicle_records <- crashes_female_ped |> 
  pivot_longer(
    cols = starts_with("VEHICLE TYPE CODE"),
    names_to = "vehicle_slot",
    values_to = "vehicle_type"
  )
nrow(vehicle_records) == nrow(crashes_female_ped) * 5

# how many missing values are in the new dataframe? 20203
sum(is.na(vehicle_records$vehicle_type))

# why do we think that slots 3, 4, and 5 have so many more missing values? most crashes only have 1 or 2 cars involved, so those slots almost always get filled in. But not many crashes involve 3, 4, or 5 cars at once, so those slots stay empty a lot of times
vehicle_records |> 
  group_by(vehicle_slot) |> 
  summarise(missing = sum(is.na(vehicle_type)))

# does every crash have a first vehicle recorded? No 601 are missing, not every crash has a first vehicle on record.
crashes_female_ped |> 
  filter(is.na(`VEHICLE TYPE CODE 1`)) |> 
  nrow()

# are we safe to drop missing values? No, we would get rid of a lot of the data in slots 3-5

# 5: what kinds of vehicles are involved in crashes with a female pedestrian? Sedan has the most with station wagon behind, lot of taxis too, so many different vehicles on here though
vehicle_records |> 
  filter(!is.na(vehicle_type)) |> 
  count(vehicle_type, sort = TRUE) |> 
  print(n = 60)
