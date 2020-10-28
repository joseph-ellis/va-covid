
#####################################################
## SET GLOBAL OPTIONS FOR PACKAGES WHERE NECESSARY ##
#####################################################

# set global thousand separator for Highcharts
hcOptions <- getOption("highcharter.lang")
hcOptions$thousandsSep <- ","
options(highcharter.lang = hcOptions)

###########################################################
## DEFINE FUNCTIONS AVAILABLE THROUGHOUT THE APPLICATION ##
###########################################################

# generate a gradient palette using the viridis package
ViridisStops <- function(.n, .option) {
  library(viridis)

  stops <- data.frame(
    q = 1:.n / .n,
    c = viridis(
      n = .n,
      option = .option
    ),
    stringsAsFactors = FALSE
  )

  stops <- list_parse2(stops)
}

# generate a gradient palette using the RColorBrewer package
BrewerStops <- function(.n, .name) {
  library(RColorBrewer)

  stops <- data.frame(
    q = 1:.n / .n,
    c = brewer.pal(
      n = .n,
      name = .name
    ),
    stringsAsFactors = FALSE
  )

  stops <- list_parse2(stops)
}

# generic function to generate a gradient palette using viridis or RColorBrewer
ColorStops <- function(.package, .number, .color_option) {
  output <- switch(
    tolower(.package),
    "viridis" = ViridisStops(.number, .color_option),
    "colorbrewer" = BrewerStops(.number, .color_option)
  )
}

# construct a pretty date
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
