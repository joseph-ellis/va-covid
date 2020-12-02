
# total metrics to keep
hprSelectTotals <- c(
  "Date",
  "HealthPlanningRegion",
  "TotalCases",
  "Hospitalizations",
  "Deaths"
)

# new metrics to keep
hprSelectNew <- c(
  "Date",
  "HealthPlanningRegion",
  "NewCases",
  "NewHospitalizations",
  "NewDeaths"
)

# grouping fields
hprGroups <- c(
  "HealthPlanningRegion"
)

################################################
## HEALTH PLANNING REGION METRIC CALCULATIONS ##
################################################

# summary of total metrics for health planning regions by date
healthPlanningRegionMetricSummaryByDate <- casesBaseNoGroup %>%
  group_by(Date, HealthPlanningRegion) %>%
  SummarizeMetrics() %>%
  ungroup()

# summary of current total metrics for health planning regions (current date)
healthPlanningRegionCurrentMetricSummary <- healthPlanningRegionMetricSummaryByDate %>%
  filter(Date == max(Date))

# summary of new metrics for health planning regions by date
healthPlanningRegionNewMetricSummaryByDate <- healthPlanningRegionMetricSummaryByDate %>%
  select(
    all_of(hprSelectTotals)
  ) %>%
  group_by(
    across(
      all_of(hprGroups)
    )
  ) %>%
  LagMetrics() %>%
  select(
    all_of(hprSelectNew)
  ) %>%
  AppendMovingAverage() %>%
  ungroup()

# summary of current total metrics for localities (current date)
healthPlanningRegionCurrentNewMetricSummary <- healthPlanningRegionNewMetricSummaryByDate %>%
  GatherNewMetrics(HealthPlanningRegion)
