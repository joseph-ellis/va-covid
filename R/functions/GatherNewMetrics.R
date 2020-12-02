
# FUNCTION - gather new metrics into a list
GatherNewMetrics <- function(.data, ...) {
  if (missing(...)) {
    temp <- .data %>%
      slice_tail(n = 4)
  }
  else {
    temp <- .data %>%
      group_by(...) %>%
      slice_tail(n = 4)
  }

  output <- list(
    movingAverage = temp %>%
      slice_head(n = 1) %>%
      select(-c(NewCases, NewHospitalizations, NewDeaths)),
    dailyNew = temp %>%
      slice_tail(n = 1) %>%
      select(-c(NewCases_7MA, NewHospitalizations_7MA, NewDeaths_7MA))
  )
}
