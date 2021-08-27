source('download.r')
library(tidycensus)

full2 <- full

full2$collection_week_formatted <- as.Date(full2$collection_week)
full2[,8:99] <- lapply(full2[,8:99], as.numeric)
fips <- fips_codes
fips$statecountyfips <- paste0(fips$state_code,fips$county_code)
full2[full2==-999999] <- NA

fullwithifips <- merge(
  x = full2,
  y = fips,
  by.x = 'fips_code',
  by.y = 'statecountyfips',
  all = T
)


weeklyus <- group_by(
  .data = fullwithifips,
  collection_week_formatted
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
    total_icu_beds_7_day_sum = sum(total_icu_beds_7_day_sum, na.rm = T),
    icu_beds_used_7_day_sum = sum(icu_beds_used_7_day_sum, na.rm = T)
  )

weeklystate <- group_by(
  .data = fullwithifips,
  collection_week_formatted,
  state.x
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
    total_icu_beds_7_day_sum = sum(total_icu_beds_7_day_sum, na.rm = T),
    icu_beds_used_7_day_sum = sum(icu_beds_used_7_day_sum, na.rm = T)
  )

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
    total_icu_beds_7_day_sum = sum(total_icu_beds_7_day_sum, na.rm = T),
    icu_beds_used_7_day_sum = sum(icu_beds_used_7_day_sum, na.rm = T)
  ) 

weeklycounty <- merge(
  x = weeklycounty,
  y = fips,
  by.x = 'fips_code',
  by.y = 'statecountyfips',
)

weeklyfl <- filter(
  .data = weeklystate,
  state.x == 'FL'
)

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

write.csv(
  x = weeklyus,
  file = paste0(o,'/us-weekly-covid-hospitalizations.csv'),
  na = '',
  row.names = F
)

write.csv(
  x = weeklycounty,
  file = paste0(o,'/us-weekly-covid-hospitalizations-by-county.csv'),
  na = '',
  row.names = F
)

write.csv(
  x = weeklystate,
  file = paste0(o,'/us-weekly-covid-hospitalizations-by-state.csv'),
  na = '',
  row.names = F
)

write.csv(
  x = weeklyfl,
  file = paste0(o,'/fl-weekly-covid-hospitalizations.csv'),
  na = '',
  row.names = F
)

write.csv(
  x = weeklypbc,
  file = paste0(o,'/pbc-weekly-covid-hospitalizations.csv'),
  na = '',
  row.names = F
)
