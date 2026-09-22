library(tidyverse)
library(readxl)

fishing <- read_excel("data/raw/commercial.xls", sheet = "Erie")

glimpse(fishing)

# What does one row represent?
## One row represents region-year weight (pounds) of caught fish. 

# How would we look at our grain over time by region? 

fishing %>%
  ggplot(aes(x = "what would go here?", y = "what would go here?"))
fishing |> 
  ggplot(aes(x = Year, y = `Grand Total`)) +
  geom_line()

# What goes on the y-axis?


# What goes on the color aesthetic if we want one line per region?

# Are these data tidy? If not, which one breaks the rules of tidy data?

# Let's predict something

nrow(fishing)

# We're about to move 7 columns into one. How many rows should we have?

# Pivot 

fishing_long <- fishing |> 
  pivot_longer(
    names_to = "region",
    values_to = "values",
    cols = 4:10 # what does this mean?
  )

fishing |> 
  rename(
    `Comments ` = `Comments`
  ) |> 
  pivot_longer(
    names_to = "region",
    values_to = "values",
    cols = contains(" ") # what does this mean? 
  )
  
fishing_long <- fishing |> 
  pivot_longer(
    names_to = "region",
    values_to = "values",
    cols = contains(" ") # what does this mean? 
  )

fishing_long <- fishing |> 
  pivot_longer(
    names_to = "region",
    values_to = "values",
    cols = !c(Year, Lake, Species, Comments) # what does this mean?
  )

fishing_long <- fishing |> 
  pivot_longer(
    names_to = "region",
    values_to = "values",
    cols = c("Michigan (MI)", "New York (NY)", "Ohio (OH)",
             "Pennsylvania (PA)", "U.S. Total", "Canada (ONT)",
             "Grand Total") # what does this mean?
  )

nrow(fishing_long)
nrow(fishing_long) / nrow(fishing)

# Did we get what we predicted?

# Which one of these four pivots would you use? Why not some of the others? 

# let's check out what we made
fishing_long |> 
  distinct(region)

# Are all seven of these the same kind of thing? What's the grain? 

# Let's see what these 7 regions are. Let's look at Lake Whitefish in 1885 then just look at the 
# region and values. 

fishing_long |> 
  filter(Year == 1885, Species == "Lake Whitefish") |> 
  select(region, values)

# So how many levels are stacked into this one column now? And what happens if we just add the catch? 

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

# Why isn't the first total just triple the third total?

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
# What is the grain? 
# What is the total catch for that lake? 
## What distinct regions are you left with? 
## How many rows did you start with? How many did your pivot have?

