
# basic cloumn-line combo chart
HighChartColumnLineDate <- function(.data, .yTitle) {
  highchart() %>%
    hc_add_series(
      name = .yTitle(),
      data = .data(),
      type = "column",
      hcaes(
        x = as.Date(plotX),
        y = plotYC
      )
    ) %>%
    hc_add_series(
      name = "7-Day Moving Average",
      data = .data(),
      type = "line",
      hcaes(
        x = as.Date(plotX),
        y = plotYL
      )
    )
}

# application specific styling
StyleColumnLineDate <- function(.hc, .yTitle) {

  # styling
  .hc %>%
    hc_xAxis(
      title = list(
        useHTML = TRUE,
        text = as.character(h6("Date"))
      ),
      labels = list(
        format = "{value:%m/%e/%Y}"
      ),
      crosshair = TRUE
    ) %>%
    hc_yAxis(
      title = list(
        useHTML = TRUE,
        text = as.character(h6(.yTitle()))
      ),
      labels = list(
        format = "{value:,.0f}"
      ),
      crosshair = TRUE
    ) %>%
    hc_legend(
      enabled = FALSE
    ) %>%
    hc_plotOptions(
      line = list(
        color = "#21445F",
        lineWidth = 4,
        marker = list(
          enabled = FALSE
        ),
        shadow = TRUE
      )
    ) %>%
    hc_tooltip(
      useHTML = TRUE,
      headerFormat = "{point.x:%B %d, %Y} <br />",
      pointFormat = "<strong>{series.name}:</strong>  {point.y}<br />",
      shared = TRUE
    )
}
