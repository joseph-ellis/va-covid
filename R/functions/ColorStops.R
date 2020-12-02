
# FUNCTION - generate a gradient palette using the viridis package
ViridisStops <- function(.n, .option = "D", .reverse = FALSE) {
  library(viridis)

  stops <- data.frame(
    q = 1:.n / .n,
    c = if (.reverse == TRUE) {
      rev(
        viridis(
          n = .n,
          option = .option
        )
      )
    }
    else {
      viridis(
        n = .n,
        option = .option
      )
    },
    stringsAsFactors = FALSE
  )

  stops <- list_parse2(stops)
}

# FUNCTION - generate a gradient palette using the RColorBrewer package
BrewerStops <- function(.n, .name, .reverse = FALSE) {
  library(RColorBrewer)

  stops <- data.frame(
    q = 1:.n / .n,
    c = if (.reverse == TRUE) {
      rev(
        brewer.pal(
          n = .n,
          name = .name
        )
      )
    }
    else {
      brewer.pal(
        n = .n,
        name = .name
      )
    },
    stringsAsFactors = FALSE
  )

  stops <- list_parse2(stops)
}

# FUNCTION - generic function to generate a gradient palette using viridis or RColorBrewer
ColorStops <- function(.package, .number, .color_option) {
  output <- switch(
    tolower(.package),
    "viridis" = ViridisStops(.number, .color_option),
    "colorbrewer" = BrewerStops(.number, .color_option)
  )
}
