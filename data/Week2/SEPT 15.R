library(tidyverse)
library(readxl)

fishing <- read_excel("data/raw/erie.xlsx", sheet = "Erie")
glimpse(fishing)

##What does row one represent- one row is one species in a particular year, catch weight in lbs, and region location
#How would we look at grain over time by region--Year on x axis and weight on y 

fishing |> 
  ggplot(aes(x = Year, y = `Grand Total`)) +
  geom_line()
# What goes on the color aesthetic if we want one line per region? color = region but region isnt a colum yet
# Are these data tidy? If not, which one breaks the rules of tidy data? No region isn´t variable 
nrow(fishing)
# We're about to move 7 columns into one. How many rows should we have? 2562

fishing_long <- fishing |> 
  pivot_longer(
    names_to = "region",
    values_to = "values",
    cols = 4:10)
    # what does this mean?--telling pivotlonger to grab columns 4-10 and put
#them into new columns called region
fishing |> 
  rename(
    `Comments ` = `Comments`
  ) |> 
  pivot_longer(
    names_to = "region",
    values_to = "values",
    cols = contains(" ") # what does this mean? select any column whose name has
    #a space in it
  )#ERROR

fishing_long <- fishing |> 
  pivot_longer(
    names_to = "region",
    values_to = "values",
    cols = !c(Year, Lake, Species, Comments) # what does this mean?selects all 
    #but these four columns because theyre names 
  ) 

 # what does this mean?--pivot everything except these four columns

fishing_long <- fishing |> 
  pivot_longer(
    names_to = "region",
    values_to = "values",
    cols = c("Michigan (MI)", "New York (NY)", "Ohio (OH)",
             "Pennsylvania (PA)", "U.S. Total", "Canada (ONT)",
             "Grand Total") # what does this mean?--listing every region column name
  )
nrow(fishing_long)
nrow(fishing_long) / nrow(fishing)
#Did we get what we predicted yes
#Which one of these four pivots would u use and why not some of the others
#number three bc it pivots everything that isnt an ID column
fishing_long |> 
  distinct(region)

# Are all seven of these the same kind of thing? What's the grain? No grand
#total and us total are outliers, others are cities though
fishing_long |> 
  filter(Year == 1885, Species == "Lake Whitefish") |> 
  select(region, values)

# So how many levels are stacked into this one column now? And what happens if 
#we just add the catch? Grand total has us and canada, we would count them  in 
#jurisdiction, us and grand toal
fishing_long |> 
  summarise(total = sum(values, na.rm = TRUE))

fishing_long |> 
  filter(region != "Grand Total") |> 
  summarise(total = sum(values, na.rm = TRUE))

fishing_long |> 
  filter(!region %in% c("U.S. Total", "Grand Total")) |> 
  summarise(total = sum(values, na.rm = TRUE))

fishing_long |> 
  filter(!region %in% c("U.S. Total", "Grand Total")) |> 
  ggplot(aes(x = Year, y = values, color = Species)) +
  geom_line()
# Why isn't the first total just triple the third total? BC Us total and grand 
#total aren't seperate equal copies of the total, us total = Mich+NY+OH+PA, 
#grand total= Us total + canada

# Now we can plot it. But if we do below, it's a bit hectic and ugly. 
fishing_long |> 
  filter(
    !region %in% c("U.S. Total", "Grand Total"),
    !is.na(values)
  ) |> 
  mutate(species = fct_lump_n(Species, 6)) |> 
  ggplot(aes(x = Year, y = values, color = species)) +
  geom_line() 

# So let's trim it down to the top 6 species. We can do this in dplyr directly 
# using fct_lump_n(). Then let's use a better color paletter :)
fishing_long |> 
  filter(
    !region %in% c("U.S. Total", "Grand Total"),
    !is.na(values)
  ) |> 
  mutate(species = fct_lump_n(Species, 6)) |> 
  ggplot(aes(x = Year, y = values, color = species)) +
  scale_color_manual(values = palette.colors(7, "Okabe-Ito")) +
  geom_line() 

# let's take it back the other way. What if we widened it by region? How many rows? 
fishing_long |> 
  select(Year, Species, region, values) |> 
  pivot_wider(names_from = region, values_from = values) |> 
  print(width = Inf)

# Now let's break. Choose a different sheet in the spreadsheet. 
excel_sheets("data/raw/commercial.xlsx")
superior <- read_excel("data/raw/commercial.xlsx", sheet = "Superior")
glimpse(superior)
# What is the grain? one row is one species in one year with catch weight across Superior's region columns
superior_long <- superior |> 
  pivot_longer(
    names_to = "region",
    values_to = "values",
    cols = !c(Year, Lake, Species, Comments)
  )
superior_long |> distinct(region)
nrow(superior)
nrow(superior_long)
nrow(superior_long) / nrow(superior)

# What is the total catch for that lake? 1903881
superior_long |> 
  filter(!region %in% c("U.S. Total", "Grand Total")) |> 
  summarise(total = sum(values, na.rm = TRUE))

## What distinct regions are you left with? 
superior_long |> distinct(region)

## How many rows did you start with? 1673 How many did your pivot have?10038
nrow(superior)
nrow(superior_long)
