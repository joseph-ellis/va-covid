
##################################
## LOCALITY METRIC CALCULATIONS ##
##################################

# total metrics to keep
localitySelectTotals <- c(
  "Date",
  "fips",
  "HealthPlanningRegion",
  "HealthDistrict",
  "Locality",
  "LocalityCode",
  "TotalCases",
  "Hospitalizations",
  "Deaths"
)

# new metrics to keep
localitySelectNew <- c(
  "Date",
  "fips",
  "HealthPlanningRegion",
  "HealthDistrict",
  "Locality",
  "LocalityCode",
  "NewCases",
  "NewHospitalizations",
  "NewDeaths"
)

# grouping fields
localityGroups <- c(
  "fips",
  "HealthPlanningRegion",
  "HealthDistrict",
  "Locality",
  "LocalityCode"
)

#############################
## NO DEMOGRAPHIC GROUPING ##
#############################

# summary of total metrics for localities by date
localityMetricSummaryByDate <- casesBaseNoGroup

# summary of current total metrics for localities (current date)
localityCurrentMetricSummary <- localityMetricSummaryByDate %>%
  filter(Date == max(Date))

# summary of new metrics for localities by date
localityNewMetricSummaryByDate <- localityMetricSummaryByDate %>%
  select(
    all_of(localitySelectTotals)
  ) %>%
  group_by(
    across(
      all_of(localityGroups)
    )
  ) %>%
  LagMetrics() %>%
  select(
    all_of(localitySelectNew)
  ) %>%
  AppendMovingAverage() %>%
  ungroup()

# summary of current total metrics for localities (current date)
localityCurrentNewMetricSummary <- localityNewMetricSummaryByDate %>%
  GatherNewMetrics(fips)
