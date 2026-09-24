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

nrow(female_pedestrians)
## what does one row represent? One row represents one female pedestrian's involvement in one crash.

# 2: Keep only the crashes that involved at least one female pedestrian. 
# Keep COLLISION_ID, BOROUGH, and the five vehicle type columns. 
# how can we select every variable that starts with "VEHICLE TYPE CODE"?

# does one row still represent one crash? check it. 

# why a filtering join instead of a mutating join? 

# 3: Right now the vehicle types are columns. We want one row per vehicle. 
# before writing your code, how many rows should we have? 


# how many missing values are in the new dataframe? 

# why do we think that slots 3, 4, and 5 have so many more missing values? 

# does every crash have a first vehicle recorded? 

# are we safe to drop missing values?

# 5: what kinds of vehicles are involved in crashes with a female pedestrian? 
vehicle_records |> 
  count(vehicle_type, sort = TRUE) |> 
  print(n = 40)