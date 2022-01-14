source('download.r')
library(tidycensus)

# full$fips_code <- as.character(full$fips_code)
full$collection_week_formatted <- as.Date(full$collection_week)
full$collection_week_end <- full$collection_week_formatted + 6

full[,12:104] <- lapply(full[,12:104], as.numeric)

full$total_icu_beds_7_day_sum_available <- full$total_icu_beds_7_day_sum - full$icu_beds_used_7_day_sum

fips <- fips_codes
fips$statecountyfips <- paste0(
  as.character(fips$state_code),
  as.character(fips$county_code)
)

full[full==-999999] <- NA

fullwithifips <- merge(
  x = full,
  y = fips,
  by.x = 'fips_code',
  by.y = 'statecountyfips',
  all = T
)

# weeklyus <- group_by(
#   .data = fullwithifips,
#   collection_week_formatted
# ) %>%
#   summarise(
#     total_beds_7_day_sum = sum(total_beds_7_day_sum, na.rm = T),
#     all_adult_hospital_beds_7_day_sum = sum(all_adult_hospital_beds_7_day_sum, na.rm = T),
#     all_adult_hospital_inpatient_beds_7_day_sum = sum(all_adult_hospital_inpatient_beds_7_day_sum, na.rm = T),
#     inpatient_beds_7_day_sum = sum(inpatient_beds_7_day_sum, na.rm = T),
#     inpatient_beds_used_7_day_sum = sum(inpatient_beds_used_7_day_sum, na.rm = T),
#     all_adult_hospital_inpatient_bed_occupied_7_day_sum = sum(all_adult_hospital_inpatient_bed_occupied_7_day_sum, na.rm = T),
#     total_adult_patients_hospitalized_confirmed_covid_7_day_sum = sum(total_adult_patients_hospitalized_confirmed_covid_7_day_sum, na.rm = T),
#     total_pediatric_patients_hospitalized_confirmed_covid_7_day_sum = sum(total_pediatric_patients_hospitalized_confirmed_covid_7_day_sum, na.rm = T),
#     total_pediatric_patients_hospitalized_confirmed_and_suspected_covid_7_day_sum = sum(total_pediatric_patients_hospitalized_confirmed_and_suspected_covid_7_day_sum, na.rm = T),
#     total_icu_beds_7_day_sum = sum(total_icu_beds_7_day_sum, na.rm = T),
#     icu_beds_used_7_day_sum = sum(icu_beds_used_7_day_sum, na.rm = T),
#     total_icu_beds_7_day_sum_available = sum(total_icu_beds_7_day_sum_available, na.rm = T)
#   )

# weeklystate <- group_by(
#   .data = fullwithifips,
#   collection_week_formatted,
#   state.x
# ) %>%
#   summarise(
#     total_beds_7_day_sum = sum(total_beds_7_day_sum, na.rm = T),
#     all_adult_hospital_beds_7_day_sum = sum(all_adult_hospital_beds_7_day_sum, na.rm = T),
#     all_adult_hospital_inpatient_beds_7_day_sum = sum(all_adult_hospital_inpatient_beds_7_day_sum, na.rm = T),
#     inpatient_beds_7_day_sum = sum(inpatient_beds_7_day_sum, na.rm = T),
#     inpatient_beds_used_7_day_sum = sum(inpatient_beds_used_7_day_sum, na.rm = T),
#     all_adult_hospital_inpatient_bed_occupied_7_day_sum = sum(all_adult_hospital_inpatient_bed_occupied_7_day_sum, na.rm = T),
#     total_adult_patients_hospitalized_confirmed_covid_7_day_sum = sum(total_adult_patients_hospitalized_confirmed_covid_7_day_sum, na.rm = T),
#     total_pediatric_patients_hospitalized_confirmed_covid_7_day_sum = sum(total_pediatric_patients_hospitalized_confirmed_covid_7_day_sum, na.rm = T),
#     total_pediatric_patients_hospitalized_confirmed_and_suspected_covid_7_day_sum = sum(total_pediatric_patients_hospitalized_confirmed_and_suspected_covid_7_day_sum, na.rm = T),
#     total_icu_beds_7_day_sum = sum(total_icu_beds_7_day_sum, na.rm = T),
#     icu_beds_used_7_day_sum = sum(icu_beds_used_7_day_sum, na.rm = T),
#     total_icu_beds_7_day_sum_available = sum(total_icu_beds_7_day_sum_available, na.rm = T)
#   )

weeklycounty <- group_by(
  .data = fullwithifips,
  collection_week_formatted,
  fips_code
) %>%
  summarise(
    total_beds_7_day_sum = sum(total_beds_7_day_sum, na.rm = T),
    all_adult_hospital_beds_7_day_sum = sum(all_adult_hospital_beds_7_day_sum, na.rm = T),
    all_adult_hospital_inpatient_beds_7_day_sum = sum(all_adult_hospital_inpatient_beds_7_day_sum, na.rm = T),
    inpatient_beds_7_day_sum = sum(inpatient_beds_7_day_sum, na.rm = T),
    inpatient_beds_used_7_day_sum = sum(inpatient_beds_used_7_day_sum, na.rm = T),
    all_adult_hospital_inpatient_bed_occupied_7_day_sum = sum(all_adult_hospital_inpatient_bed_occupied_7_day_sum, na.rm = T),
    total_adult_patients_hospitalized_confirmed_covid_7_day_sum = sum(total_adult_patients_hospitalized_confirmed_covid_7_day_sum, na.rm = T),
    total_pediatric_patients_hospitalized_confirmed_covid_7_day_sum = sum(total_pediatric_patients_hospitalized_confirmed_covid_7_day_sum, na.rm = T),
    total_pediatric_patients_hospitalized_confirmed_and_suspected_covid_7_day_sum = sum(total_pediatric_patients_hospitalized_confirmed_and_suspected_covid_7_day_sum, na.rm = T),
    total_icu_beds_7_day_sum = sum(total_icu_beds_7_day_sum, na.rm = T),
    icu_beds_used_7_day_sum = sum(icu_beds_used_7_day_sum, na.rm = T),
    total_icu_beds_7_day_sum_available = sum(total_icu_beds_7_day_sum_available, na.rm = T)
  ) 

weeklycounty <- merge(
  x = weeklycounty,
  y = fips,
  by.x = 'fips_code',
  by.y = 'statecountyfips',
)

# weeklyfl <- filter(
#   .data = weeklystate,
#   state.x == 'FL'
# )

weeklypbc <- filter(
  .data = weeklycounty,
  fips_code == '12099'
)

pbcfacilities <- filter(
  .data = fullwithifips,
  fips_code == '12099'
)

o <- 'output'
dir.create(o)

# write.csv(
#   x = weeklyus,
#   file = paste0(o,'/us-weekly-covid-hospitalizations.csv'),
#   na = '',
#   row.names = F
# )

write.csv(
  x = weeklycounty,
  file = paste0(o,'/fl-weekly-covid-hospitalizations-by-county.csv'),
  na = '',
  row.names = F
)

# write.csv(
#   x = weeklystate,
#   file = paste0(o,'/us-weekly-covid-hospitalizations-by-state.csv'),
#   na = '',
#   row.names = F
# )

# write.csv(
#   x = weeklyfl,
#   file = paste0(o,'/fl-weekly-covid-hospitalizations.csv'),
#   na = '',
#   row.names = F
# )

write.csv(
  x = weeklypbc,
  file = paste0(o,'/pbc-weekly-covid-hospitalizations.csv'),
  na = '',
  row.names = F
)
