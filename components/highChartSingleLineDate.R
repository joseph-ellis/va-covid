
# basic single line chart
HighChartSingleLineDate <- function(.data, .yTitle) {
  highchart() %>%
    hc_add_series(
      name = .yTitle(),
      data = .data(),
      type = "line",
      hcaes(
        x = as.Date(plotX),
        y = plotY
      )
    )
}

# application specific styling
StyleSingleLineDate <- function(.hc, .yTitle, .subtitle) {

  # callback function for formatting y-axis labels
  JS_yAxisFormatter <- V8::JS(
    "function () {
      if(this.value < 1 && this.value > 0) {
        return Highcharts.numberFormat((this.value * 100), 0) + '%';
      }
      else {
        return Highcharts.numberFormat(this.value, 0);
      }
    }"
  )

  # callback function for formatting tooltip values
  JS_tooltipPointFormatter <- V8::JS(
    "function () {
      if(this.y < 1 && this.y > 0) {
        return '<strong>' + this.series.name + ':  </strong>' +
          Highcharts.numberFormat((this.y * 100), 2) + '%';
      }
      else {
        return '<strong>' + this.series.name + ':  </strong>' +
          Highcharts.numberFormat(this.y, 0);
      }
    }"
  )

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
        formatter = JS_yAxisFormatter
      ),
      min = 0,
      tickAmount = 5,
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
      pointFormatter = JS_tooltipPointFormatter
    ) %>%
    hc_title(
      align = "left",
      style = list(
        color = "#21445F"
      ),
      text = paste0("COVID-19 ", .yTitle())
    ) %>%
    hc_subtitle(
      align = "left",
      style = list(
        color = "#21445F"
      ),
      text = .subtitle
    )
}
