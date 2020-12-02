
# load necessary packages
library(tidyverse)
library(lubridate)
library(zoo)

# load VDOH data object
source("R/DataAccess.R")

# get a list of all function files
functionSource <- list.files(
  path = "R/functions",
  pattern = ".R",
  full.names = TRUE
)

# source all function files
sapply(
  X = functionSource,
  FUN = source
)

# metrics to be used from the VDOH data
metrics <- c("total_cases", "hospitalizations", "deaths")

# build base object for generating summaries without demographic groups
casesBaseNoGroup <- vdoh_data$CasesLocality %>%
  left_join(
    vaMapData,
    by.x = locality,
    by.y = name
  ) %>%
  left_join(vaPop2019) %>%
  mutate(
    across(
      .cols = all_of(metrics),
      .fns = as.numeric
    ),
    Week = epiweek(report_date)
  ) %>%
  select(
    "Date" = report_date,
    Week,
    fips,
    HealthPlanningRegion,
    "HealthDistrict" = vdh_health_district,
    "Locality" = locality,
    "LocalityCode" = `hc-a2`,
    Population2019,
    "TotalCases" = total_cases,
    "Hospitalizations" = hospitalizations,
    "Deaths" = deaths
  ) %>%
  AppendMetrics()

# build base object for generating summaries for age groups
casesBaseAgeGroup <- vdoh_data$CasesByAgeGroupDistrict %>%
  filter(age_group != "Missing") %>%
  replace_na(
    replace = list(
      report_date = as.POSIXct("01/01/01"),
      age_group = "Not Reported",
      number_of_cases = "0",
      number_of_hospitalizations = "0",
      number_of_deaths = "0",
      health_district = "Not Reported"
    )
  ) %>%
  mutate(
    across(
      .cols = c("number_of_cases", "number_of_hospitalizations",
                "number_of_deaths"),
      .fns = as.numeric
    )
  ) %>%
  rename(
    "Date" = report_date,
    "AgeGroup" = age_group,
    "HealthPlanningDistrict" = health_district
  ) %>%
  left_join(vaPopAge2019)  %>%
  group_by(Date, AgeGroup, HealthPlanningDistrict, Population2019) %>%
  summarize(
    TotalCases = sum(number_of_cases),
    Hospitalizations = sum(number_of_hospitalizations),
    Deaths = sum(number_of_deaths)
  ) %>%
  AppendMetrics() %>%
  ungroup()

source("R/sections/locality.R")
source("R/sections/healthPlanningRegion.R")
source("R/sections/state.R")

# objects to save
toSave <- list(
  LMSBD = localityMetricSummaryByDate,
  LCMS = localityCurrentMetricSummary,
  LNMSBD = localityNewMetricSummaryByDate,
  LCNMS = localityCurrentNewMetricSummary,
  HPRMSBD = healthPlanningRegionMetricSummaryByDate,
  HPRCMS = healthPlanningRegionCurrentMetricSummary,
  HPRNMSBD = healthPlanningRegionNewMetricSummaryByDate,
  HPRCNMS = healthPlanningRegionCurrentNewMetricSummary,
  SMSBD = stateMetricSummaryByDate,
  SCMS = stateCurrentMetricSummary,
  SNMSBD = stateNewMetricSummaryByDate,
  SCNMS = stateCurrentNewMetricSummary,
  SAGMSBD = stateAgeGroupMetricSummaryByDate,
  SAGCMS = stateAgeGroupCurrentMetricSummary,
  SAGNMSBD = stateAgeGroupNewMetricSummaryByDate,
  SAGCNMS = stateAgeGroupCurrentNewMetricSummary
)

# write to disk
saveRDS(
  object = toSave,
  file = "data/covid_refactor.rds"
)
