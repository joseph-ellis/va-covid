# load necessary packages
library(tidyverse)
library(lubridate)
library(zoo)

# load VDOH data object
source("R/DataAccess.R")

# metrics to be used from the VDOH data
metrics <- c("total_cases", "hospitalizations", "deaths")

# build base object for generating summaries
localMetricSummaryByDate <- vdoh_data$CasesLocality %>%
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

MovingAverage7Day <- function(.field) {
  round(
    rollmean(
      .field,
      k = 7,
      fill = NA
    ),
    digits = 2
  )
}

localCurrentMetricSummary <- localMetricSummaryByDate %>%
  filter(Date == max(Date))

localNewMetricSummaryByDate <- localMetricSummaryByDate %>%
  select(Date, fips, HealthPlanningRegion, HealthDistrict, Locality, LocalityCode,
         TotalCases, Hospitalizations, Deaths) %>%
  group_by(fips, HealthPlanningRegion, HealthDistrict, Locality, LocalityCode) %>%
  mutate(
    NewCases = TotalCases - lag(TotalCases, default = 0),
    NewHospitalizations = Hospitalizations - lag(Hospitalizations, default = 0),
    NewDeaths = Deaths - lag(Deaths, default = 0)
  ) %>%
  select(Date, fips, HealthPlanningRegion, HealthDistrict, Locality, LocalityCode,
         NewCases, NewHospitalizations, NewDeaths) %>%
  group_by(fips, HealthPlanningRegion, HealthDistrict, Locality, LocalityCode) %>%
  mutate(
    NewCases_7MA = MovingAverage7Day(NewCases),
    NewHospitalizations_7MA = MovingAverage7Day(NewHospitalizations),
    NewDeaths_7MA = MovingAverage7Day(NewDeaths)
  ) %>%
  ungroup()

localCurrentNewMetricSummaryByDate <- localNewMetricSummaryByDate %>%
  filter(Date == max(Date)) %>%
  select(Date, fips, HealthPlanningRegion, HealthDistrict, Locality, LocalityCode,
         NewCases, NewHospitalizations, NewDeaths)

###############################
## STATE METRIC CALCULATIONS ##
###############################

# summary of metrics for entire state for all dates
stateMetricSummaryByDate <- localMetricSummaryByDate %>%
  group_by(Date) %>%
  SummarizeMetrics()

# summary of current metrics for entire state
stateCurrentMetricSummary <- stateMetricSummaryByDate %>%
  filter(Date == max(Date))

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
    NewCases_7MA = MovingAverage7Day(NewCases),
    NewHospitalizations_7MA = MovingAverage7Day(NewHospitalizations),
    NewDeaths_7MA = MovingAverage7Day(NewDeaths)
  )

# summary of current daily new metrics
stateCurrentNewMetricSummary <- stateNewMetricSummaryByDate %>%
  filter(Date == max(Date)) %>%
  select(Date, NewCases, NewHospitalizations, NewDeaths)

#summary of
stateAgeGroupMetricSummaryByDate <- vdoh_data$CasesByAgeGroupDistrict %>%
  mutate(
    across(
      .cols = c("number_of_cases", "number_of_hospitalizations",
                "number_of_deaths"),
      .fns = as.numeric
    )
  ) %>%
  rename(
    "Date" = report_date,
    "AgeGroup" = age_group
  ) %>%
  left_join(vaPopAge2019) %>%
  group_by(Date, AgeGroup, Population2019) %>%
  summarize(
    TotalCases = sum(number_of_cases),
    Hospitalizations = sum(number_of_hospitalizations),
    Deaths = sum(number_of_deaths)
  ) %>%
  mutate(
    across(
      .cols = where(is.numeric),
      .fns = ~ replace(
        .x,
        is.na(.x),
        0
      )
    )
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
  ) %>%
  ungroup()

stateAgeGroupNewMetricSummaryByDate <- stateAgeGroupMetricSummaryByDate %>%
  select(Date, AgeGroup, TotalCases, Hospitalizations, Deaths) %>%
  group_by(AgeGroup) %>%
  mutate(
    NewCases = TotalCases - lag(TotalCases, default = 0),
    NewHospitalizations = Hospitalizations - lag(Hospitalizations, default = 0),
    NewDeaths = Deaths - lag(Deaths, default = 0)
  ) %>%
  select(Date, AgeGroup, NewCases, NewHospitalizations, NewDeaths) %>%
  group_by(AgeGroup) %>%
  mutate(
    NewCases_7MA = MovingAverage7Day(NewCases),
    NewHospitalizations_7MA = MovingAverage7Day(NewHospitalizations),
    NewDeaths_7MA = MovingAverage7Day(NewDeaths)
  ) %>%
  ungroup()

stateAgeGroupCurrentMetricSummaryByDate <- stateAgeGroupMetricSummaryByDate %>%
  filter(Date == max(Date))

stateAgeGroupCurrentNewMetricSummary <- stateAgeGroupNewMetricSummaryByDate %>%
  filter(Date == max(Date)) %>%
  select(Date, AgeGroup, NewCases, NewHospitalizations, NewDeaths)

################################################
## HEALTH PLANNING REGION METRIC CALCULATIONS ##
################################################

# summary of metrics for health planning regions for all dates
healthPlanningRegionMetricSummaryByDate <- localMetricSummaryByDate %>%
  group_by(Date, HealthPlanningRegion) %>%
  SummarizeMetrics() %>%
  ungroup()

# summary of current metrics for health planning regions
healthPlanningRegionCurrentMetricSummary <- healthPlanningRegionMetricSummaryByDate %>%
  filter(Date == max(Date))

# summary of daily new metrics with moving averages
healthPlanningRegionNewMetricSummaryByDate <- healthPlanningRegionMetricSummaryByDate %>%
  select(Date, HealthPlanningRegion,TotalCases, Hospitalizations, Deaths) %>%
  group_by(HealthPlanningRegion) %>%
  mutate(
    NewCases = TotalCases - lag(TotalCases, default = 0),
    NewHospitalizations = Hospitalizations - lag(Hospitalizations, default = 0),
    NewDeaths = Deaths - lag(Deaths, default = 0)
  ) %>%
  select(Date, HealthPlanningRegion, NewCases, NewHospitalizations, NewDeaths) %>%
  group_by(HealthPlanningRegion) %>%
  mutate(
    NewCases_7MA = MovingAverage7Day(NewCases),
    NewHospitalizations_7MA = MovingAverage7Day(NewHospitalizations),
    NewDeaths_7MA = MovingAverage7Day(NewDeaths)
  ) %>%
  ungroup()

healthPlanningRegionCurrentNewMetricSummary <- healthPlanningRegionNewMetricSummaryByDate %>%
  filter(Date == max(Date)) %>%
  select(Date, HealthPlanningRegion, NewCases, NewHospitalizations, NewDeaths)

#########################################
## HEALTH DISTRICT METRIC CALCULATIONS ##
#########################################

# summary of current metrics for health districts
healthDistrictCurrentMetricSummary <- localMetricSummaryByDate %>%
  filter(Date == max(Date)) %>%
  group_by(Date, HealthPlanningRegion, HealthDistrict) %>%
  SummarizeMetrics()

##################################
## LOCALITY METRIC CALCULATIONS ##
##################################

# summary of current metrics for localities
localityCurrentMetricSummary <- localMetricSummaryByDate %>%
  filter(Date == max(Date))

# objects to save
toSave <- list(
  LCMS = localCurrentMetricSummary,
  LMSBD = localMetricSummaryByDate,
  LCNMS = localCurrentNewMetricSummaryByDate,
  LNMSBD = localNewMetricSummaryByDate,
  SCMS = stateCurrentMetricSummary,
  SMSBD = stateMetricSummaryByDate,
  SCNMS = stateCurrentNewMetricSummary,
  SNMSBD = stateNewMetricSummaryByDate,
  SAGMSBD = stateAgeGroupMetricSummaryByDate,
  SAGCMSBD = stateAgeGroupCurrentMetricSummaryByDate,
  SAGNMSBD = stateAgeGroupNewMetricSummaryByDate,
  SAGCNMS = stateAgeGroupCurrentNewMetricSummary,
  HPRCMS = healthPlanningRegionCurrentMetricSummary,
  HPRMSBD = healthPlanningRegionMetricSummaryByDate,
  HPRCNMS = healthPlanningRegionCurrentNewMetricSummary,
  HPRNMSBD = healthPlanningRegionNewMetricSummaryByDate
)

# write to disk
saveRDS(
  object = toSave,
  file = "data/covid.rds"
)
