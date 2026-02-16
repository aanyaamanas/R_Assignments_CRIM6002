library(dplyr)
library(tidyr)
load("NCVS2022.RData")

colnames(dataExt)


head(dataExt)
head(dataAssaults)

summarize_ncvs_correct <- function(df, varname, title) {
  df |>
    summarize(
      EstimatedTotal = sum(SERIES_WEIGHT),
      .by = {{ varname }}
    ) |>
    mutate({{ varname }} := gsub("\\([0-9]+\\)\\s*", "", as.character({{ varname }}))) |>
    filter(!grepl("OUT OF UNIVERSE|NOT APPLICABLE|Don't know|RESIDUE|Blank", {{ varname }})) |>
    arrange(desc(EstimatedTotal)) |>
    mutate(
      Percent = 100 * EstimatedTotal / sum(EstimatedTotal)
    ) |>
    rename(!!title := {{ varname }})
}
  


ASSAULT_CODES <- c("(11) Ag aslt w injury", "(12) At ag aslt w wea", 
                   "(13) Thr aslt w weap", "(14) Simp aslt w inj", 
                   "(17) Asl wo weap, wo inj", "(20) Verbal thr aslt")


# --- Part 1: Context of Assaults (Using SERIES_WEIGHT) ---

# Filter the main dataset for assault incidents only
assaults <- dataExt |> filter(V4529 %in% ASSAULT_CODES)
assaults |> 
  select(V4529) |> 
  distinct()

# 1a: Where did assaults occur? (V4022: Location of Incident)
assault_location_estimates <- assaults |>
  group_by(V4022) |>
  summarize(EstimatedTotal = sum(SERIES_WEIGHT)) |>
  arrange(desc(EstimatedTotal))
print(assault_location_estimates, n = Inf)

# 1b: When did assaults occur? (V4014: Month of Crime, V4021B: ABOUT WHAT TIME DID INCIDENT OCCUR)
assault_time_estimates <- assaults |>
  group_by(V4014, V4021B) |>
  summarize(EstimatedTotal = sum(SERIES_WEIGHT)) |>
  arrange(desc(EstimatedTotal))
print(assault_time_estimates, n = Inf)

# 1c: Who was the offender? (V4245: SINGLE OFF HOW DID RESP KNOW OFFENDER)
assault_offender_estimates <- assaults |>
  group_by(V4245) |>
  summarize(EstimatedTotal = sum(SERIES_WEIGHT)) |>
  arrange(desc(EstimatedTotal))
print(assault_offender_estimates, n = Inf)

# 1d: What sort of weapons were used? (V4048: Type of Weapon Used/Threatened)
assault_weapon_estimates <- assaults |>
  filter(!is.na(V4048) & !grepl("Not applicable|No weapon", V4048, ignore.case = TRUE)) |>
  group_by(V4048) |>
  summarize(EstimatedTotal = sum(SERIES_WEIGHT)) |>
  arrange(desc(EstimatedTotal))
print(assault_weapon_estimates, n = Inf)


# 1e: Were the police called? (V4156: Police Notified)
assault_police_estimates <- assaults |>
  filter(!is.na(V4156) & !grepl("Not applicable|No weapon", V4156, ignore.case = TRUE)) |>
  group_by(V4156) |>
  summarize(EstimatedTotal = sum(SERIES_WEIGHT)) |>
  arrange(desc(EstimatedTotal))
print(assault_police_estimates, n = Inf)

# 1f: How many victims used firearms defensively? (V4086: Defensive action taken)
firearm_used_defence <- assaults |>
  filter(!is.na(V4144) & !grepl("Not applicable|No weapon", V4144, ignore.case = TRUE)) |>
  group_by(V4144) |>
  summarize(EstimatedTotal = sum(SERIES_WEIGHT)) |>
  arrange(desc(EstimatedTotal))
print(firearm_used_defence, n = Inf)

# Part 2: Crime by Victim Race (Using SERIES_WEIGHT) 

# 2a: Estimate the number of crimes by race of victim by crime type.
crimes_by_race_and_type <- dataExt |>
  filter(!is.na(V3023A) & !grepl("unavail|unknown|not applicable|not done", V3023A, ignore.case = TRUE)) |>
  filter(!is.na(V4529) & !grepl("unavail|unknown|not applicable|not done", V4529, ignore.case = TRUE)) |>
  group_by(V4529, V3023A) |>
  summarize(EstimatedTotal = sum(SERIES_WEIGHT, na.rm = TRUE), .groups = 'drop') |>
  arrange(V4529, desc(EstimatedTotal))
print(crimes_by_race_and_type, n = Inf)

# 2b: Find crime types that disproportionately affect black victims.

#CONSOLIDATING RACE CATEGORIES AND CRIME NAMES 
df_race_crime <- dataExt |>
  mutate(
    # V3023A is the Recoded Race field. We must consolidate mixed-race categories.
    raceSimple = case_when(
      grepl("White only", V3023A) ~ "White",
      grepl("Black only", V3023A) ~ "Black",
      grepl("Amer Ind", V3023A) ~ "American Indian/Alaska Native",
      # Consolidate all mixed-race, Asian, and Pacific Islander categories into one 'Other' group
      TRUE ~ "Other/Mixed Races" 
    ),
    # V4529 is the Type of Crime Code
    crimeName = gsub("\\([0-9]+\\)\\s*", "", as.character(V4529))
  ) |>
  # Filter to the required comparison groups (White, Black) for the proportionality check.
  filter(raceSimple %in% c("White", "Black"))

#a) Show a table with the estimated total number of victims by race and crime type ---
cat("a) Estimated Total Victimizations by Crime Type and Victim Race (Consolidated)\n")

crimes_by_race_and_type <- df_race_crime |>
  group_by(crimeName, raceSimple) |>
  summarize(EstimatedTotal = round(sum(SERIES_WEIGHT, na.rm = TRUE)), .groups = 'drop') |>
  arrange(crimeName, desc(EstimatedTotal))

crimes_by_race_wide <- crimes_by_race_and_type |>
  pivot_wider(
    names_from = raceSimple,
    values_from = EstimatedTotal,
    values_fill = 0
  )

print(crimes_by_race_wide, n = Inf)

#b) Find crime types that disproportionately affect black victims
cat("b) Crime Types that Disproportionately Affect Black Victims\n")

# 1. Calculate total weighted victimizations for Black and White groups (Denominator)
#    We sum the raw SERIES_WEIGHT column, NOT the non-existent 'EstimatedTotal' column.
total_black_victims <- df_race_crime |>
  filter(raceSimple == "Black") |>
  summarize(Total = sum(SERIES_WEIGHT, na.rm = TRUE)) |>
  pull(Total)

total_white_victims <- df_race_crime |>
  filter(raceSimple == "White") |>
  summarize(Total = sum(SERIES_WEIGHT, na.rm = TRUE)) |>
  pull(Total)

# 2. Calculate proportionality ratio (Crime's share of Black total / Crime's share of White total)
disproportionate_analysis <- crimes_by_race_and_type |>
  pivot_wider(
    names_from = raceSimple,
    values_from = EstimatedTotal,
    values_fill = 0
  ) |>
  mutate(
    Prop_Black = Black / total_black_victims,
    Prop_White = White / total_white_victims,
    Disproportionate_Ratio = Prop_Black / Prop_White
  ) |>
  filter(Prop_Black >= 0.001) |>
  arrange(desc(Disproportionate_Ratio)) |>
  select(crimeName, Black_Prop_Share = Prop_Black, Disproportionate_Ratio)

cat(paste("Total Black Victimizations (Weighted):", format(round(total_black_victims), big.mark = ",", scientific = FALSE), "\n"))
cat(paste("Total White Victimizations (Weighted):", format(round(total_white_victims), big.mark = ",", scientific = FALSE), "\n"))

disproportionate_analysis_filtered <- disproportionate_analysis |>
  filter(Disproportionate_Ratio > 1.25)

print(disproportionate_analysis_filtered, n = Inf)

