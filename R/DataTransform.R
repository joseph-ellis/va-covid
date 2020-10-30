# load necessary packages
library(tidyverse)
library(lubridate)
library(zoo)

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

# operations for summary objects
SummarizeMetrics <- function(.data) {
  .data %>%
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
          digits = 4
        )
      )
    )
}

# summary of current metrics for entire state
stateCurrentMetricSummary <- allLocalMetricsByDate %>%
  filter(Date == max(Date)) %>%
  group_by(Date) %>%
  SummarizeMetrics()

# summary of current metrics for health planning regions
healthPlanningRegionCurrentMetricSummary <- allLocalMetricsByDate %>%
  filter(Date == max(Date)) %>%
  group_by(Date, HealthPlanningRegion) %>%
  SummarizeMetrics()

# summary of current metrics for health districts
healthDistrictCurrentMetricSummary <- allLocalMetricsByDate %>%
  filter(Date == max(Date)) %>%
  group_by(Date, HealthPlanningRegion, HealthDistrict) %>%
  SummarizeMetrics()

# summary of current metrics for localities
localityCurrentMetricSummary <- allLocalMetricsByDate %>%
  filter(Date == max(Date))

# summary of metrics for entire state for all dates
stateMetricSummaryByDate <- allLocalMetricsByDate %>%
  group_by(Date) %>%
  SummarizeMetrics()

# summary of daily new metrics with moving averages
stateNewMetricSummaryByDate <- stateMetricSummaryByDate %>%
  select(Date, TotalCases, Hospitalizations, Deaths) %>%
  mutate(
    NewCases = TotalCases - lag(TotalCases, default = 0),
    NewHospitalizations = Hospitalizations - lag(Hospitalizations, default = 0),
    NewDeaths = Deaths - lag(Deaths, default = 0)
  ) %>%
  select(Date, NewCases, NewHospitalizations, NewDeaths) %>%
  mutate(
    NewCases_7MA = round(
      rollmean(NewCases, k = 7, fill = NA),
      digits = 2
    ),
    NewHospitalizations_7MA = round(
      rollmean(NewHospitalizations, k = 7, fill = NA),
      digits = 2
    ),
    NewDeaths_7MA = round(
      rollmean(NewDeaths, k = 7, fill = NA),
      digits = 2
    )
  )

# objects to save
toSave <- list(
  SCMS = stateCurrentMetricSummary,
  HPRCMS = healthPlanningRegionCurrentMetricSummary,
  HDCMS = healthDistrictCurrentMetricSummary,
  LCMS = localityCurrentMetricSummary,
  SMSBD = stateMetricSummaryByDate,
  SNMSBD = stateNewMetricSummaryByDate
)

saveRDS(
  object = toSave,
  file = "data/covid.rds"
)
