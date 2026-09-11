library(tidyverse)
data<- read_csv("data/raw/co-est2025-alldata.csv",
                locale = locale(encoding = "Latin1"),
                col_types = cols(.default = col_character())
)
glimpse(data)
names(data)

#find us population in 2025, Ky counties population change 2020-25, number of counties

#what can we determine about this?  Annual Resident Population Estimates, 
#Estimated Components of Resident Population Change, and Rates of the Components
#of Resident Population Change for States and Counties: April 1, 2020 to July 1, 2025

data_trimmed <- data |>
  select(SUMLEV, REGION, DIVISION, STATE, COUNTY, STNAME, CTYNAME, POPESTIMATE2024, POPESTIMATE2025, NPOPCHG2025) 
data_trimmed

#040=state, 050=county

data_trimmed |>
  slice_head(n = 10)

#declare the grain- One row represents geographic location, one row represents either county or state
## one row represents a mixed state-county grain

data_trimmed_numeric <- data_trimmed |>
  mutate(POPESTIMATE2024 = as.numeric(POPESTIMATE2024),
         POPESTIMATE2025 = as.numeric(POPESTIMATE2025))

data_trimmed_numeric <- data_trimmed |>
  mutate(
    pop2025 = as.numeric(POPESTIMATE2025),
    pop2024 = as.numeric(POPESTIMATE2024),
    popchg2025 = as.numeric(NPOPCHG2025)
    
  )

data_trimmed_numeric |>
  summarise(total_pop_2025 = sum(pop2025)
  )
#us population 
#ky population
data_trimmed_numeric |>
  filter(STNAME == "Kentucky") |>
  slice_head(n = 10)

data_trimmed_numeric |>
  summarise(
    total_pop_2025 = sum(pop2025)
  )

data_trimmed_numeric |> 
  filter(SUMLEV == "040") |>
  summarise(
    total_pop_2025 = sum(pop2025)
  )

#which ky counties grew most from 24-25
data_trimmed_numeric |>
  filter(STNAME == "Kentucky", SUMLEV == "050")|>
  filter(popchg2025 > 0)|>
  select(CTYNAME, popchg2025) |>
  arrange(desc(popchg2025)) |>
  print(n = 81)

data_trimmed_numeric |>
  filter(SUMLEV == "050", STNAME == "Kentucky", popchg2025 > 0) |>
  mutate(
    popchgvalid = pop2025 - pop2024
  ) |>
  select(CTYNAME, popchgvalid) |> 
  arrange(desc(popchgvalid)) |>
  print(n = 120)

#HOw many countiesrecords are in this file
data_trimmed_numeric |>
  
  filter(SUMLEV == "050")|>
  count()

data_trimmed_numeric |>
  filter(SUMLEV == "050")|>
  nrow()
