# load necessary packages
library(tidyverse)
library(lubridate)

# load VDOH data object
source("R/DataAccess.R")

# metrics to be used from the VDOH data
metrics <- c("total_cases", "hospitalizations", "deaths")

# build base object for generating summaries
allLocalMetricsByDate <- vdoh_data$CasesLocality %>%
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
  )  %>%
  mutate(
    CasesPerCapita = TotalCases / Population2019,
    CasesPer100K = CasesPerCapita * 1e5,
    PercentCasesHospitalized = Hospitalizations / TotalCases,
    HospitalizationPerCapita = Hospitalizations / Population2019,
    HospitalizationsPer100K = HospitalizationPerCapita * 1e5,
    MortalityRate = Deaths / TotalCases,
    DeathsPerCapita = Deaths / Population2019,
    DeathsPer100K = DeathsPerCapita * 1e5,
    across(
      .cols = where(is.numeric),
      .fns = ~ round(
        .x,
        4
      )
    ),
    across(
      .cols = where(is.numeric),
      .fns = ~ replace(
        .x,
        is.nan(.x),
        0
      )
    )
  )

# summary of current metrics for entire state
stateCurrentMetricSummary <- allLocalMetricsByDate %>%
  filter(Date == max(Date)) %>%
  group_by(Date) %>%
  summarize(
    Population2019 = sum(Population2019),
    TotalCases = sum(TotalCases),
    Hospitalizations = sum(Hospitalizations),
    Deaths = sum(Deaths)
  ) %>%
  mutate(
    CasesPerCapita = TotalCases / Population2019,
    CasesPer100K = CasesPerCapita * 1e5,
    PercentCasesHospitalized = Hospitalizations / TotalCases,
    HospitalizationPerCapita = Hospitalizations / Population2019,
    HospitalizationsPer100K = HospitalizationPerCapita * 1e5,
    MortalityRate = Deaths / TotalCases,
    DeathsPerCapita = Deaths / Population2019,
    DeathsPer100K = DeathsPerCapita * 1e5,
    across(
      .cols = where(is.numeric),
      .fns = ~ round(
        .x,
        4
      )
    )
  )

# summary of current metrics for health planning regions
healthPlanningRegionCurrentMetricSummary <- allLocalMetricsByDate %>%
  filter(Date == max(Date)) %>%
  group_by(Date, HealthPlanningRegion) %>%
  summarize(
    Population2019 = sum(Population2019),
    TotalCases = sum(TotalCases),
    Hospitalizations = sum(Hospitalizations),
    Deaths = sum(Deaths)
  ) %>%
  mutate(
    CasesPerCapita = TotalCases / Population2019,
    CasesPer100K = CasesPerCapita * 1e5,
    PercentCasesHospitalized = Hospitalizations / TotalCases,
    HospitalizationPerCapita = Hospitalizations / Population2019,
    HospitalizationsPer100K = HospitalizationPerCapita * 1e5,
    MortalityRate = Deaths / TotalCases,
    DeathsPerCapita = Deaths / Population2019,
    DeathsPer100K = DeathsPerCapita * 1e5,
    across(
      .cols = where(is.numeric),
      .fns = ~ round(
        .x,
        4
      )
    )
  )

# summary of current metrics for health districts
healthDistrictCurrentMetricSummary <- allLocalMetricsByDate %>%
  filter(Date == max(Date)) %>%
  group_by(Date, HealthPlanningRegion, HealthDistrict) %>%
  summarize(
    Population2019 = sum(Population2019),
    TotalCases = sum(TotalCases),
    Hospitalizations = sum(Hospitalizations),
    Deaths = sum(Deaths)
  ) %>%
  mutate(
    CasesPerCapita = TotalCases / Population2019,
    CasesPer100K = CasesPerCapita * 1e5,
    PercentCasesHospitalized = Hospitalizations / TotalCases,
    HospitalizationPerCapita = Hospitalizations / Population2019,
    HospitalizationsPer100K = HospitalizationPerCapita * 1e5,
    MortalityRate = Deaths / TotalCases,
    DeathsPerCapita = Deaths / Population2019,
    DeathsPer100K = DeathsPerCapita * 1e5,
    across(
      .cols = where(is.numeric),
      .fns = ~ round(
        .x,
        4
      )
    )
  )

# summary of current metrics for localities
localityCurrentMetricSummary <- allLocalMetricsByDate %>%
  filter(Date == max(Date))




# objects to save
toSave <- c("stateCurrentMetricSummary",
            "healthPlanningRegionCurrentMetricSummary",
            "healthDistrictCurrentMetricSummary",
            "localityCurrentMetricSummary"
  )

# write RData object with current data
save(list = toSave, file = "data/CovidData.RData")
