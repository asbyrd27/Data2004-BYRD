# weekly assignment review 

# now let's review the reading together, briefly 

## what does is.na() do? Retturns a true/false command or if something is missing 

## what does na.rm = TRUE mean? removes the missing values 

## what is the differnece between an explicit and implicit missing value? Labeled missing values are explicit, implicit would be missing data bc the data isn't there at all
# brief introduction to statistical understanding of missingness: 
## there are (basically) three kinds of missingness that statisticians evaluate 
#MAR-missing at random 
#MNAR- missing not at random
#MCAR- missing completely at random

# packages 
library(tidyverse)

# we will get our data from this package today 
# install.packages("NHANES")
library(NHANES)
glimpse(NHANESraw)
# this is the CDC's National Health and Nutrition Examination Survey; 
# interviews, physical exams, and lab tests, from 2009 to 2012. 
# use ?NHANESraw for documentation
?NHANESraw

# note: make sure you use NHANESraw NOT NHANES. NHANES is a weighted survey to 
# give inferences about the US population; some people are repeated to weight 
# their respective groups heavier. 


# again we'll start with a simple question: what share of adults currently smoke? 47%
NHANESraw |> 
  summarise(pct_smoke = mean(SmokeNow == "Yes", na.rm = TRUE))

# is that reasonable? is something else going on here? no, seems high
# let's look at how many people that answer actually used, and out of how many

# check the documentation for SmokeNow. is there an explanation of the missing 
# values? SmokeNow is missing for people who were not asked the question, because they had not smoked 100 cigarettes

# given that, what did na.rm = TRUE actually do? it literally removed missing values, 
# yes, but changed the question that we're asking. think about where the NA values 
# would systematically be located. 

# the question ..."Yes", na.rm = TRUE)) is asking isn't "what share of adults currently smoke?"
# it's asking "Among adults who've smoked 100 cigarettes, what share still smoke?"

# how should we deal with these missing values?
# R4DS says that we can use caolese() when a missing value stands for a value 
# that you actually know. 

NHANESraw |> 
  summarise(pct_smoke = mean(coalesce(SmokeNow, "No") == "Yes"))
#12.1%

# is that reasonable? what did coalesce() assume? It assumes everyone missing a SmokeNow answer does not currently smoke.

# who else is missing in SmokeNow? look at Age. 
# create a column called age_group and define the groups as 
# 20+ and under 20. 

NHANES_age <- NHANESraw |>
  mutate(
    age_group = if_else(Age >= 20, "20+", "under 20"))

NHANES_age |>
  group_by(age_group) |>
  count()

# after that, keep only adults *first*, then use coalesce() again. 
NHANES_age |>
  filter(age_group == "20+") |>
  summarise(
    pct_missing = sum(is.na(SmokeNow)),
    pct_smoke = mean(coalesce(SmokeNow, "No") == "Yes"))
 

# so how can we create a column called 'smokes' that handles these missing values 
# correctly? 
# hint: we know Age matters here, and we know that participants who responded with 
# "No" in Smoke100 are actually "No"


adults <- NHANES_age |>
  mutate(
    smokes = case_when(
      age_group == "under 20" ~ NA_character_,
      Smoke100 == "No" ~ "No",
      TRUE ~ SmokeNow
    )
  )

adults |>
  summarise(
    missing_smokes = sum(is.na(smokes))
  )
# do we have any missing in our smokes column? 8524


# should they become "No"? No, because we don't know that all of those missing values mean the person does not smoke.
# with that, does NA mean actually one thing in this column? yes, NA means we do not know whether the person smokes now or used to smoke

# your turn :) 

# for each of the following variables: 
## 1. what percent is missing?
## 2. who was eligible to have a value? check ?NHANESraw.
## 3. is the missingness mostly outside that group?
## 4. answer the question, and say exactly who your answer describes.

# A. SleepHrsNight 
## how many hours do people usually sleep on a weeknight? People age 16 and older usually sleep about 6.9 hours on a weeknight.
#about 35.8% is missing, ages 16 and up were eligible, The missingness is almost entirely outside the eligible group

NHANESraw |>
  summarise(
    pct_missing = mean(is.na(SleepHrsNight)))

?NHANESraw

NHANESraw |>
  mutate(
    sleep_group = if_else(Age >= 16, "16+", "under 16")) |>
  group_by(sleep_group) |>
  summarise(
    pct_missing = mean(is.na(SleepHrsNight)))

NHANESraw |>
  filter(Age >= 16) |>
  summarise(
    avg_sleep = mean(SleepHrsNight, na.rm = TRUE))


# B. Testosterone 
## what's the average testosterone? does it differ between years? 184.944 ng/dL for participants age 6+ who were measured in 2011–2012.
## hint: after you drop the missing values, count by SurveyYr. where did 2009_10 go?
# about 66.4% is missing, Participants age 6 and older were eligible but not measured in 2009–2010, missingness is mostly because testosterone was not measured in 2009–2010

NHANESraw |>
  summarise(
    pct_missing = mean(is.na(Testosterone)))

NHANESraw |>
  filter(!is.na(Testosterone)) |>
  count(SurveyYr)

NHANESraw |>
  filter(!is.na(Testosterone)) |>
  summarise(
    avg_testosterone = mean(Testosterone))


# let's co one more thing: income 
# let's focus on adults 
adults <- NHANESraw |> 
  filter(Age >= 20)

adults |> 
  summarise(pct_missing = mean(is.na(HHIncomeMid)))

# does the doc give us anything at all here? # No, the doc does not explain the missing values.

# let's see if the missingness is systematically related to another variable 
adults |> 
  group_by(Education) |> 
  summarise(
    pct_missing = mean(is.na(HHIncomeMid)),
    mean_income = mean(
      HHIncomeMid, na.rm = TRUE)
  ) |> 
  arrange(Education)

mean(adults$HHIncomeMid, na.rm = TRUE)

# how does the pattern of missigness bias the results? People with a lower education are more likely to have missing income, and they also tend to have lower incomes.
# what happens if we just estimate the mean of HHIncomeMid and remove NAs?
#Removing the missing values could make the average income look higher than it really is.