library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(readxl)
nibrs <- read_excel("NIBRS Records Description updated.xlsx")
head(nibrs)
getwd()

list.files()

ls()

getwd()

load("nibrs2023.RData")

if (!exists("fmtXL")) {
  fmtXL <- read_excel("NIBRS Records Description updated.xlsx", skip = 3,
                      sheet = "INCIDENT RECORD")
}
print(fmtXL$Description)
head(fmtXL)

head(fmtXL$Description, 50)
tail(fmtXL$Description, 50)

colnames(fmtXL) <- c("Field", "Position", "TypeLength", "Description")
colnames(fmtXL)

head(fmtXL)
colnames(fmtXL)

# Q1: Create UCR offense lookup table

fmtXL <- read_excel("NIBRS Records Description updated.xlsx", skip = 3,
                    sheet = "INCIDENT RECORD",
                    range = "A5:D819")


i <- which(fmtXL$Description=="720 - Animal Cruelty Offenses - Animal Cruelty")
j <- which(fmtXL$Description=="90Z - All Other Offenses - All Other Offenses")
a <- fmtXL$Description[i:j]
b <- substr(a,1,3)
description <- gsub("[0-9][0-9]....(.*) -.*$","\\1", a)

C <- data.frame(UCRcode = b,
                crime = description)

#2 Find all crimes in Philadelphia reported by the Philadelphia Police 
#Department

philly_ori <- "PAPEP0000"
philly_crimes <- nibrsCrm02 |>
  filter(ORI == "PAPEP0000") |>
  count(UCR.OFFENSE.CODE) |>
  rename(UCR_CODE = UCR.OFFENSE.CODE, n = n) |>
  left_join(UCRlookup, by = c("UCR_CODE" = "UCR"))

# View first 20 rows
head(philly_crimes, 20)

#3 

names(nibrsProp03)
mobile_phones <- nibrsProp03 |>
  filter(grepl("MOBILE|CELL|PHONE|SMARTPHONE", PROPERTY.DESCRIPTION, ignore.case = TRUE))

#a) What percentage of stolen mobile phones are recovered within 1 week? 
head(mobile_phones$INCIDENT.DATE)
head(mobile_phones$DATE.RECOVERED)
unique(nibrsProp03$PROPERTY.DESCRIPTION)

mobile_phones <- nibrsProp03 |>
  filter(PROPERTY.DESCRIPTION %in% c("17", "18"))

mobile_phones <- mobile_phones |>
  mutate(days_to_recovery = as.numeric(difftime(ymd(DATE.RECOVERED), ymd(INCIDENT.DATE), units = "days")))

# Percentage recovered within 1 week
pct_recovered_1week <- mobile_phones %>%
  summarise(pct = mean(days_to_recovery <= 7, na.rm = TRUE) * 100)

pct_recovered_1week

#b) median value recovered vs not recovered 
median_values <- mobile_phones |>
  mutate(recovered = !is.na(DATE.RECOVERED)) |>
  group_by(recovered) |>
  summarise(median_value = median(VALUE.OF.PROPERTY, na.rm = TRUE))

median_values

#c) Which state has the most expensive stolen mobile phones 
most_expensive <- mobile_phones |>
  group_by(STATE) |>
  summarise(median_value = median(VALUE.OF.PROPERTY, na.rm = TRUE)) |>
  arrange(desc(median_value)) |>
  slice(1)

most_expensive

StateLookup |>
  filter(StateCode == 27)

#4 Where is the highest homicide rate
unique(nibrsCrm02$UCR.OFFENSE.CODE)

homicide_rate <- nibrsCrm02 |>
  filter(UCR.OFFENSE.CODE %in% c("09A","09B","09C")) |>
  left_join(nibrsBH %>% select(ORI, CITY.NAME, STATE.ABBREVIATION, TOTAL.POP), by = "ORI") |>
  group_by(CITY.NAME, STATE.ABBREVIATION) |>
  summarise(
    total_homicides = n(),
    population = first(TOTAL.POP),
    rate_per_100k = total_homicides / population * 100000,
    .groups = "drop"
  ) |>
  arrange(desc(rate_per_100k))

head(homicide_rate, 10)

#a) 10 law enforcement agencies having highest homicide rate per 100,000 residents
top10_agencies <- nibrsCrm02 |>
  filter(UCR.OFFENSE.CODE %in% c("09A","09B","09C")) |>
  left_join(
    nibrsBH |>
      select(ORI, CITY.NAME, STATE.ABBREVIATION, TOTAL.POP),
    by = "ORI"
  ) |>
  group_by(ORI, CITY.NAME, STATE.ABBREVIATION) |>
  summarise(
    total_homicides = n(),
    population = first(TOTAL.POP),
    rate_per_100k = total_homicides / population * 100000,
    .groups = "drop"
  ) |>
  arrange(desc(rate_per_100k)) |>
  slice_head(n = 10)

top10_agencies

#b) 
top10_agencies_b <- nibrsCrm02 |>
  filter(UCR.OFFENSE.CODE %in% c("13A","13B","13C")) |>
  left_join(
    nibrsBH |>
      select(ORI, CITY.NAME, STATE.ABBREVIATION, CURRENT.POPULATION.1),
    by = "ORI"
  ) |>
  group_by(ORI, CITY.NAME, STATE.ABBREVIATION) |>
  summarise(
    total_homicides = n(),
    population = first(CURRENT.POPULATION.1),
    rate_per_100k = total_homicides / population * 100000,
    .groups = "drop"
  ) |>
  arrange(desc(rate_per_100k)) |>
  slice_head(n = 10)

print(top10_agencies_b)

#c ) creating a costlookup table based on slides

crime_cost <- data.frame(
  UCR = c("01A", "02A", "03A"),   
  cost = c(1000, 5000, 2000)     
)
city_costs <- nibrsCrm02 |>
  left_join(nibrsBH |> select(ORI, CITY.NAME, STATE.ABBREVIATION), by = "ORI") |>
  left_join(crime_cost, by = c("UCR.OFFENSE.CODE" = "UCR")) |>
  filter(!is.na(cost)) |>
  group_by(CITY.NAME, STATE.ABBREVIATION) %>%
  summarise(total_cost = sum(cost, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_cost)) %>%
  slice_head(n = 10)
