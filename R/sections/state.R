
###############################
## STATE METRIC CALCULATIONS ##
###############################

# total metrics to keep
stateSelectTotals <- c(
  "Date",
  "TotalCases",
  "Hospitalizations",
  "Deaths"
)

# total metrics to keep for age groups
stateAgeGroupSelectTotals <- c(
  "Date",
  "AgeGroup",
  "TotalCases",
  "Hospitalizations",
  "Deaths"
)

# new metrics to keep
stateSelectNew <- c(
  "Date",
  "NewCases",
  "NewHospitalizations",
  "NewDeaths"
)

# new metrics to keep for age groups
stateAgeGroupSelectNew <- c(
  "Date",
  "AgeGroup",
  "NewCases",
  "NewHospitalizations",
  "NewDeaths"
)

#############################
## NO DEMOGRAPHIC GROUPING ##
#############################

# summary of total metrics for entire state by date
stateMetricSummaryByDate <- casesBaseNoGroup %>%
  group_by(Date) %>%
  SummarizeMetrics() %>%
  ungroup()

# summary of current total metrics for entire state (current date)
stateCurrentMetricSummary <- stateMetricSummaryByDate %>%
  filter(Date == max(Date))

# summary of new metrics for entire state by date
stateNewMetricSummaryByDate <- stateMetricSummaryByDate %>%
  select(
    all_of(stateSelectTotals)
  ) %>%
  LagMetrics() %>%
  select(
    all_of(stateSelectNew)
  ) %>%
  AppendMovingAverage()

# summary of current new metrics for entire state (current date)
stateCurrentNewMetricSummary <- stateNewMetricSummaryByDate %>%
  GatherNewMetrics()

################
## AGE GROUPS ##
################

# summary of total age group metrics for entire state by date
stateAgeGroupMetricSummaryByDate <- casesBaseAgeGroup %>%
  group_by(Date, AgeGroup) %>%
  SummarizeMetrics() %>%
  ungroup()

# summary of current total age group metrics for entire state (current date)
stateAgeGroupCurrentMetricSummary <- stateAgeGroupMetricSummaryByDate %>%
  filter(Date == max(Date))

# summary of new age group metrics for entire state by date
stateAgeGroupNewMetricSummaryByDate <- stateAgeGroupMetricSummaryByDate %>%
  select(
    all_of(stateAgeGroupSelectTotals)
  ) %>%
  group_by(AgeGroup) %>%
  LagMetrics() %>%
  select(
    all_of(stateAgeGroupSelectNew)
  ) %>%
  AppendMovingAverage() %>%
  ungroup()

# summary of current new age group metrics for entire state (current date)
stateAgeGroupCurrentNewMetricSummary <- stateAgeGroupNewMetricSummaryByDate %>%
  GatherNewMetrics(AgeGroup)
