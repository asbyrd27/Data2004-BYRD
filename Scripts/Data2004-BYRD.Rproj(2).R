install.packages("tidyverse")
library(tidyverse)
getwd()
population_csv <- read_csv("data/raw/API_SP.POP.TOTL_DS2_en_csv_v2_285942.csv")
population_csv <- read_csv("data/raw/API_SP.POP.TOTL_DS2_en_csv_v2_285942.csv",
                           skip=4)
glimpse(population_csv)
population_csv <- population_csv |>
  select(...71)
glimpse(population_csv)
library(readxl)

population_xls <- read_excel("data/raw/API_SP.POP.TOTL_DS2_en_excel_v2_290931.xls", 
                             sheet = "Data", skip=3)

download.file(
  url = "https://api.worldbank.org/v2/country/all/indicator/SP.POP.TOTL?date=2020%3A2024&format=json&per_page=20000",
  destfile = "data/raw/population_json.json",
  mode = "wb"
  )                             
install.packages("jsonlite")                             
library("jsonlite")                             
population_json_raw <- read_json("data/raw/population_json.json")
glimpse(population_json_raw)

population_obs <- population_json_raw[[2]]
glimpse(population_obs)

population_json_simple<- read_json("data/raw/population_json.json",
                                   simplifyVector = TRUE)

glimpse(population_json_simple)
population_obs