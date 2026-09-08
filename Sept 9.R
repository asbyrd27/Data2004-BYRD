library(tidyverse)
chocolate <- read_csv("data/raw/chocolate.csv")
glimpse(chocolate)

#select n filter
chocolate |>
  select(ref,company_manufacturer, company_location, rating, cocoa_percent)

chocolate |> 
  filter(company_location == "U.S.A")

chocolate |> 
  filter(company_location == "U.S.A",
         rating >= 3.5)

chocolate|>
  filter(company_location == "U.S.A" & rating >= 3.5)

chocolate |>
  filter(company_location == "U.S.A" | rating >= 3.5)

chocolate |>
  filter(company_location %in% c("U.S.A", "France","Canada" ))

#reviews for 2021, rating of 3.5 and higher, USA and Vietnam
chocolate|>
  filter(rating >= 3.5, 
         review_date == 2021, 
         company_location %in% c("U.S.A", "Vietnam"))

# arrange in top ten highest rated bars
chocolate|>
  select(rating, country_of_bean_origin) |>
  arrange(desc(rating))|>
  head(n = 10)

#mean of cocoa percentage 
mean(chocolate$cocoa_percent)
glimpse(chocolate)

chocolate |> 
  select(cocoa_percent) |>
  mutate(
    cocoa_num = parse_number(cocoa_percent)
  ) 
  
chocolate <- chocolate |>
  mutate(
    cocoa_num = parse_number(cocoa_percent)
  
)

mean(chocolate$cocoa_num)

#avg of cocoa percentage in countries
chocolate |> 
  group_by(company_location) |>
  summarise(
    n = n(),
    avg_rating = mean(rating, na.rm = TRUE),
    avg_cocoa = mean(cocoa_num, na.rm = TRUE)
  ) |>
  slice_head(n = 10)


sum(is.na(chocolate$review_date))
    
    
#histogram
chocolate |> 
  ggplot(aes(x = rating)) +
  geom_histogram()

#basic barplot with usa france and uk
chocolate |> 
  filter(company_location %in% c("U.S.A", "Canada", "France")) |>
  ggplot(aes(x = rating, y = company_location, color = company_location)) +
  coord_flip() +
  geom_boxplot() +
    labs(title = "Boxplot of ratings by country",
         x = "country",
         y = "ratings")
    
