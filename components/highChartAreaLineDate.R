
# basic cloumn-line combo chart
HighChartAreaLineDate <- function(.data, .yTitle) {
  highchart() %>%
    hc_add_series(
      name = .yTitle(),
      data = .data(),
      type = "area",
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
StyleAreaLineDate <- function(.hc, .yTitle) {

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
      min = 0,
      tickAmount = 5,
      crosshair = TRUE
    ) %>%
    hc_legend(
      enabled = FALSE
    ) %>%
    hc_plotOptions(
      area = list(
        fillOpacity = 0.5,
        marker = list(
          enabled = FALSE
        ),
        shadow = TRUE
      ),
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
