
# FUNCTION - calculate 7-day moving average
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

# FUNCTION - append 7-day moving average to dataframe
AppendMovingAverage <- function(.data) {
  .data %>%
    mutate(
      NewCases_7MA = MovingAverage7Day(NewCases),
      NewHospitalizations_7MA = MovingAverage7Day(NewHospitalizations),
      NewDeaths_7MA = MovingAverage7Day(NewDeaths)
    )
}
