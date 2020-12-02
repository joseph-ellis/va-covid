
# FUNCTION - generate lagged metrics
LagMetrics <- function(.data) {
  .data %>%
    mutate(
      NewCases = TotalCases - lag(TotalCases, default = 0),
      NewHospitalizations = Hospitalizations - lag(Hospitalizations, default = 0),
      NewDeaths = Deaths - lag(Deaths, default = 0)
    )
}
