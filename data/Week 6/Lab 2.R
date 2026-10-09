# Lab 2: is this file ready to analyze?

library(tidyverse)


# you're a junior analyst at a state library association. a colleague
# downloaded the 2024 Public Libraries Survey and wants to compare
# library systems on visits, circulation, and programs.

# your supervisor wants an audit before anyone analyzes anything.

# nothing today is new. you've done every piece of this already.


# grain and key 


libraries <- read_csv(
  "data/raw/PLS_FY24_AE_pud24i.csv",
  locale = locale(encoding = "latin1"),
  show_col_types = FALSE
)

glimpse(libraries)

# one row is one administrative entity records for a library 

# the download also has an "outlet" file. it has more rows.
# why would it? (the user's guide will tell you.) The outlet file has more rows because one library system can have multiple branches or locations.

# which column should identify a row? FSCSKEY 

libraries |>
  count(FSCSKEY) |>
  filter(n > 1)

# what did those tell you? FSCSKEY is unique for every library system.
# what would it have meant if the count() came back with rows?
#It would mean some library systems share the same ID, so FSCSKEY is not unique.


# let's focus on VISITS for now. what does this column mean? 

# VISITS counts visits to the library in a year.
# before you run anything: what values would be impossible?
# Negatives would be impossible because visits cannot be below zero.

libraries |>
  summarise(
    min_visits = min(VISITS, na.rm = TRUE),
    max_visits = max(VISITS, na.rm = TRUE)
  )

libraries |>
  filter(VISITS < 0) |>
  count(VISITS)

# how many different negative values? how many rows of each? two diff negative values, -3 has 5 rows and -1 has 69 rows 

# is a negative number here bad data, missing data, or a code?
# can you tell from the data alone? The doc is saying there is a small proportion of data is not public knowledge, but this is probably bad data

# does R think any of these are missing? no, counted all 9249 rows

is.na(libraries$VISITS) |>
  table()

# on tuesday, NA told us THAT something was missing but not WHY.
# what's different here? NA only tells us data is missing, but -1 and -3 may tell us why it is missing.


# go to the user's guide. for each negative value:
# what does it mean, in the guide's words? where did you find it?
#page 43 or A-10 on the Appendix, -1 means library's visit count is missing, -3 is the library entity is closed

# do the two codes mean the same thing? No, -1 means library's visit count is missing, -3 is the library entity is closed

# every numeric column has a flag column. find the one for VISITS.


# what does the flag tell you? does it tell the two codes apart? Both -1 and -3 have the U_24 flag. It does not tell the two codes apart bc they are the same, 
#U_24 means its either closed or was not recorded
libraries |>
  filter(VISITS < 0) |>
  count(VISITS, F_VISITS)


# make a clean version. VISITS stays exactly as it is.

libraries <- libraries |>
  mutate(visits_clean = if_else(VISITS %in% c(-1, -3), NA_real_, VISITS))

# why list the codes instead of writing VISITS < 0? Because they represent something other missing data

# both codes just turned into NA. what did we lose? We lose the reasoning the value is missing
# where can we still find it? We can still find the original in VISITS.


# did it do what we meant? three questions:
nrow(libraries)
# same number of library systems? yes 
# did every code become NA? No, 74 value became NA

libraries |>
  filter(VISITS %in% c(-1, -3)) |>
  count(is.na(visits_clean))

# did anything else become NA? No, just the 74 values

libraries |>
  summarise(missing = sum(is.na(visits_clean)))

# does any of this matter? It matters we are aware that 74 values are NA now 


# why is the raw mean lower? what's in each denominator?
# It is lower because it includes the negative values -1 and -3, the denominator would be the number of rows, so for raw its 9249 and for clean it is 9175


# your turn :)
# get in your group project groups

# do that process for each of the following:

# TOTATTEN - program attendance
# TOTATTEN measures total attendance at library programs, F_TOTATT is the flag, and both have U_24.
# -1 appears 401 times and means missing data.-3 appears 5 times and means temporarily closed.

libraries <- libraries |>
  mutate(
    attendance_clean = if_else(TOTATTEN %in% c(-1, -3),
                               NA_real_, TOTATTEN))
libraries |>
  summarise(
    rows = n(),
    missing = sum(is.na(attendance_clean)))

libraries |>
  summarise(
    raw_mean = mean(TOTATTEN),
    clean_mean = mean(attendance_clean, na.rm = TRUE))

libraries |>
  filter(is.na(attendance_clean)) |>
  count(STABR, sort = TRUE)
# TOTPRO - number of programs

libraries |>
  summarise(
    min_programs = min(TOTPRO, na.rm = TRUE),
    max_programs = max(TOTPRO, na.rm = TRUE)
  )

libraries |>
  filter(TOTPRO < 0) |>
  count(TOTPRO)


# TOTCIR - total circulation

libraries |>
  filter(TOTPRO < 0) |>
  count(TOTPRO, F_TOTPRO)

libraries <- libraries |>
  mutate(
    programs_clean = if_else(TOTPRO %in% c(-1, -3),
                             NA_real_, TOTPRO))
libraries |>
  summarise(
    rows = n(),
    missing = sum(is.na(programs_clean)))

libraries |>
  summarise(
    raw_mean = mean(TOTPRO),
    clean_mean = mean(programs_clean, na.rm = TRUE))

libraries |>
  filter(is.na(programs_clean)) |>
  count(STABR, sort = TRUE)


# you're not solving a new problem. same steps, different variable.
# everything you need is in the VISITS section.

# what should it measure? what would be impossible? TOTATTEN measures total attendance at library programs, negative attendance would be impossible


# range. any odd values? how many of each? The range is -3 to 1,386,900. -1 appears 401 times and -3 appears 5 times


# what does the user's guide say they mean?  -1 means missing and -3 means temporarily closed. 
# ("we couldn't find it" is an answer. "we assumed" isn't.)


# what's the flag column? (the names get shortened. look for it.)
#F_TOTATT. Both negative codes have the U_24 flag.

# clean version. keep the raw column.

libraries <- libraries |>
  mutate(
    attendance_clean = if_else(TOTATTEN %in% c(-1, -3),
                               NA_real_, TOTATTEN))
libraries |>
  summarise(
    rows = n(),
    missing = sum(is.na(attendance_clean)))


# check it: same rows? every code became NA? nothing else did? All 9,249 rows stayed, and all 406 negative codes became NA.

# mean before and after. big change or small? The raw mean was 11,387 and the clean mean was 11,910.

libraries |>
  summarise(
    raw_mean = mean(TOTATTEN),
    clean_mean = mean(attendance_clean, na.rm = TRUE))


# where are they?  
# The most missing values are in Puerto Rico (40),Vermont (40), Arkansas (39), Iowa (35), and New Jersey (32).

  filter(is.na(attendance_clean)) |>
  count(STABR, sort = TRUE)

#recoding to NA fixes the number. does it finish the job?
#No, because we still need to see why the values are missing and whether they affect our results.

# are the coded rows spread out, or do they bunch up?
#The missing values are not evenly spread out. Some states have more missing values than others.

# if someone compares circulation across states, what goes wrong?
#States with more missing data may have inaccurate totals. This could make comparisons between states misleading.


# this is for you to answer

# is this file ready to analyze as is? 3-4 sentences.
#No, this file is not ready to analyze as it is. Some columns have negative values like -1 and -3 that represent missing data, not actual numbers. 
#These values need to be changed to NA so they don't affect the averages. We also need to check where the missing data is because some states have more missing values than others, which could make comparisons unfair.