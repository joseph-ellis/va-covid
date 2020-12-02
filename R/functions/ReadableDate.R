
# FUNCTION - construct a pretty date
ReadableDate <- function(.date) {
  month <- month(
    .date,
    label = TRUE,
    abbr = FALSE
  )

  day <- day(.date)

  year <- year(.date)

  output <- paste0(month, " ", day, ", ", year)

  return(output)
}
