# Strings and categorical representation

library(tidyverse)

pa <- read_delim(
  "data/raw/fec_pa_2024_strings.psv",
  delim = "|",
  quote = "",
  na = character(),
  col_types = cols(.default = col_character())
)

# only contributions > $200 are included
# only three of the original columsn are there. record_id is mine, not the FEC's
# helps to protect anonymity. 

glimpse(pa)

nrow(pa)
n_distinct(pa$record_id)

# What does one row represent? Is one row one contributor? one row is a contribution record that is greater than $200

# Which variable are we interested in today? Employer, we are going to figure out who employs the people giving money to campaigns.

# Our task: who employs the people giving money to campaigns?

pa |> 
  count(employer, sort = TRUE) |> 
  print(n = 25)

# Same question as the vehicles. Is this counting employers? It's counting employer entries/values, not distinct real-world employers.

# Look for ways someone could say they are self employed. self-employed, self, self employed, N/A(?)
# What about unemployed? un-employed, not employed, N/A(?), not-employed, none, blank, retiree, 

n_distinct(pa$employer)

# Does that mean there are 6,950 different employers? No, some employers may be written in different ways

# Normalizing representation
examp_text <- "Hello, this  is an example. "

str_to_upper(examp_text)
str_trim(examp_text)
str_squish(examp_text)

# let's create employer_normalized, where we make every observation in all caps
# and we remove traiing and leading white spaces. 
pa_clean<- pa |>
  mutate(
    employer_normalized = employer |>
      str_to_upper() |>
      str_squish()
  )

n_distinct(pa$employer)
n_distinct(pa_clean$employer_normalized)

# That barely changed. Why not? Because the data as already recorded in terms of spacing, means we have to look out for puncuation more

# What does that tell you about how this data was recorded?
#Since cleaning everything up barely changed anything, this means people were already typing things pretty consistently

# What's actually inconsistent?
pa |> 
  filter(
    employer %in% c(
      "NOT EMPLOYED", "NOT-EMPLOYED",
      "SELF EMPLOYED", "SELF-EMPLOYED", "SELF",
      "ELECTRICIANS LOCAL 98", "ELECTRICIANS LOCAL98",
      "HIGHMARK INC", "HIGHMARK, INC.", "HIGHMARK HEALTH"
    )
  ) |> 
  count(employer, sort = TRUE)

# Which of these differences are just representation? NOT EMPLOYED vs. NOT-EMPLOYED; SELF EMPLOYED vs. SELF-EMPLOYED; ELECTRICIANS LOCAL 98 vs. ELECTRICIANS LOCAL98
# Which might actually mean different things? self could be self employed or something else like a company's name
# Which would you change without outside information? I would merge the similar variables listed above

# Punctuation 
## let's get rid of those hyphenations now. 
# we can use str_replace_all(fixed()) to do this. 
# should we use str_squish again?

pa_clean <- pa_clean |> 
  mutate(
    employer_no_hyphen = employer_normalized |> 
      str_replace_all(fixed("-"), " ") |> 
      str_squish()
  )

pa_clean |> 
  filter(
    employer %in% c(
      "NOT EMPLOYED", "NOT-EMPLOYED",
      "SELF EMPLOYED", "SELF-EMPLOYED", "SELF"
    )
  ) |> 
  count(employer_no_hyphen, sort = TRUE)

# What collapsed? What didn't? not employed and not-employed and self employed and self-employed both collapsed 

# SELF looks like it belongs with SELF EMPLOYED. Does it? Not necessarily, it is just alphabetically there with the self employed's
# What if Self is a company? An abbreviation for something else? 

# Removing a hyphen changed how two words were written.
# Merging SELF would be a claim about who these people are.


# Exact replacement
# When you know exactly which value you want to change, change that
# value and nothing else.

# Here's where we can use case_when()

pa_clean <- pa_clean |> 
  mutate(
    employer_clean = case_when(
      employer_normalized == "NOT-EMPLOYED" ~ "NOT EMPLOYED",
      employer_normalized == "SELF-EMPLOYED" ~ "SELF EMPLOYED",
      employer_normalized == "ELECTRICIANS LOCAL98" ~ "ELECTRICIANS LOCAL 98",
      employer_normalized == "HIGHMARK, INC." ~ "HIGHMARK INC",
      TRUE ~ employer_normalized
    )
  )

# Notice this matches on employer_normalized, not employer_no_hyphen.
# If you had already stripped punctuation, would
# "HIGHMARK, INC." still be there for case_when to find? If punctuation had already been stripped, "HIGHMARK, INC." wouldn't exist anymore


# Why keep raw column?
pa_clean |> 
  filter(employer != employer_clean) |> 
  count(employer, employer_clean, sort = TRUE) |> 
  print(n = 61)

# I wrote four replacement rules. Why did more rows than that change?  Because the true command carries through every change already made like uppercase and squish

# Find ORRICK HERRINGTON  SUTCLIFFE LLP. Why is there a double space
# in the middle of a law firm's name? What did str_squish() do to it?
#The double space I dont think means anything significant, str_squish gets rid of the double space 

pa_clean |> 
  select(employer, employer_normalized, employer_clean) |> 
  distinct() |> 
  arrange(employer_clean) |> 
  print(n = 30)

# What would we lose if we overwrote employer? We would lose the original record of each filer we wrote


# Weekly 2

coffee <- read_csv("data/raw/coffee_ratings.csv")

coffee |> 
  count(producer, sort = TRUE)

n_distinct(coffee$producer)

# Tuesday: Regular expression 

# Today I told you which values to look at. What if I hadn't? 
# Would you read all 6,950? No 

# Tuesday: how do you find suspicious or related values without
# reading every one?