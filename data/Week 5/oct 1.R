# packages 
library(tidyverse)

# data
requests <- read_csv(
  "data/raw/nyc311_2023_sample_50000.csv.gz",
  col_types = cols(.default = col_character()),
  show_col_types = FALSE
)

glimpse(requests) 

# what does one row represent? how do you know? 
# what identifies one request? how do you know?

n_distinct(requests$unique_key)
nrow(requests)

# take a look at our columns. what are the data types? 

# pay attention to the *_date columns. what types are those? 

# the columns are dates and times, but is that their actual data type being recognized? 

# let's see how this is a problem by dealing with a seeming easy question: 
# how long did each request take to close? 

# what if we just tried the most straightforward option: subtraction? 
requests |>
  summarise(
    time_to_close = closed_date - created_date
  )
# what's the problem here? 
# The date columns are character values, so R cannot subtract them.

# we can try another approach, let's use day() to pull the day out of a date. 
# bracket the first row of created_date and let's just use day() on it. 
requests$created_date[1]
day(requests$created_date[1])
month(requests$created_date[1])

# what is the value? where is that number even coming from??
# R is interpreting the character string incorrectly because it has not been parsed as a date.
# let's take a look at day day(), month(), and year() do under the hood. 
as.Date(requests$created_date[1])

# as.Date tries %Y-%m-%d so it's interpreting "02/07/2023 02:38:31 PM" as 
# July 20th, 0002 AD. 

# this is the problem even if we do it in lubridate
ymd(requests$created_date[1])

# let's figure out what's up with our representation. 
# select created_date and look at the first 10 rows. 
requests |>
  select(created_date) |>
  slice_head(n = 10) 

# look at those 10 lines. before we can choose a parser, what do we need to know? 

# took at look at line 4 in the output. 


# what is the date? 
# 01/12/2023 08:36:16 AM

# how could you get evidence from the data itself? imagine you're in a scenario where the
# answer isn't abundantly clear from that output. hint: regex can help with things like this. 

requests |>
  filter(str_detect(created_date, "/2[0-9]/")) |>
  select(created_date) 

# what does your regex idea tell you about the data? 
# The second number can be greater than 12, so it must be the day.
# This shows the data is in month/day/year format.

# since we've done that, we can actually do our parsing now. 
# create a new column called created. keep created_date. 
# how do we do this? 
requests <- requests |>
  mutate(created_wrong = mdy_hms(created_date))


requests <- requests |>
  mutate(created = mdy_hms(created_date,
      tz = "America/New_York"))
  

requests |>
  select(created_date, created_wrong) |> 
  slice_head(n = 10)

requests$created[1] - requests$created_wrong[1]

# let's check out something interesting about parsing. what are the arguments 
# that are allowed in our mdy_hms() function?

# let's return to our new created column. are there any potential problems here? 
## could we have accidentally created some missing values? 
## what do you need to know to assess this? 

requests|>
  summarise(raw_missing = sum(is.na(created_date)),
            created_missing = sum(is.na(created)), 
            newly_missing = created_missing - raw_missing)

# are there any problems that could occur even if we didn't have any missing values at all? 
# If newly_missing is 0, parsing did not create any new missing values.
# use this as an example: 01/05/2023 01:30:00 PM
# could something be potentially risky with that? how can we check on 01:30:00 PM?
requests |> 
  count(hour = hour(created)) |> 
  print(n = 24)

# what does this tell you? It lets us check whether the AM and PM times were parsed correctly.

# briefly, what about chronological operations? 
requests |> 
  summarise(
    earliest = min(created, na.rm = TRUE),
    latest = max(created, na.rm = TRUE)
  )

# what would this give us on the original character column? guess first.

requests |> 
  summarise(
    earliest_string = min(created_date, na.rm = TRUE),
    latest_string = max(created_date, na.rm = TRUE)
  )


# here are a few things to do on your own: 

# first, make a plot of requests per week. however, notice there isn't a week column. 
# how should you make one? 

requests <- requests |>
  mutate(week = floor_date(created, unit = "week"))

requests |>
  count(week)


# second, here is a larger bit of practice. 

## your question: which city agencies close 311 requests fastest? New York City Police Department has the fastest typical close time among the agencies with at least 500 requests.
requests <- requests |>
  mutate(
    closed = mdy_hms(closed_date,
                     tz = "America/New_York"))

requests <- requests |>
  mutate(
    time_to_close = closed - created
  )

requests |>
  group_by(agency_name) |>
  summarise(
    requests_received = n(),
    requests_measured = sum(!is.na(time_to_close)),
    typical_time = median(time_to_close, na.rm = TRUE)
  ) |>
  filter(requests_received >= 500) |>
  arrange(typical_time)
## you should report: 
### how many requests it received. NYPD received 21,750 requests.

### how many you were able to measure. all 21,750 NYPD requests.
### a typical time to close. The median closing time was 2,786.5 seconds. 

## you should limit your results to only include agencies that had at least 500 requests.  
### 
