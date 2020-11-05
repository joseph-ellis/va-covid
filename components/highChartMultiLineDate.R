
# basic single line chart
HighChartMultiLineDate <- function(.data, .yTitle) {
  highchart() %>%
    hc_add_series(
      data = .data(),
      type = "line",
      hcaes(
        x = as.Date(plotX),
        y = plotY,
        group = plotGroup
      )
    )
}

# application specific styling
StyleMultiLineDate <- function(.hc, .yTitle, .subtitle) {

  # callback function for formatting y-axis labels
  JS_yAxisFormatter <- V8::JS(
    "function() {
      if(this.value < 1 && this.value > 0) {
        return Highcharts.numberFormat((this.value * 100), 0) + '%';
      }
      else {
        return Highcharts.numberFormat(this.value, 0);
      }
    }"
  )

  JS_tooltipFormatter <- V8::JS(
    "function() {
      return this.points.reduce(function(s, point) {
        var value = point.y;

        if(value < 1 && value > 0) {
          value = Highcharts.numberFormat((value * 100), 2) + '%';
        }
        else {
          value = Highcharts.numberFormat(value, 0);
        }

        return s + '<span style=\"color:' + point.color + '\">●</span>' +
          '<strong>' + point.series.name + ':  </strong>' + value + '<br />';
      }, Highcharts.dateFormat('%B %d, %Y', this.x) + '<br />');
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
      enabled = TRUE
    ) %>%
    hc_plotOptions(
      line = list(
        lineWidth = 4,
        marker = list(
          enabled = FALSE
        ),
        shadow = TRUE
      )
    ) %>%
    hc_tooltip(
      useHTML = TRUE,
      formatter = JS_tooltipFormatter,
      shared = TRUE
    ) %>%
    hc_title(
      align = "left",
      style = list(
        color = "#21445F"
      ),
      text = paste0("COVID-19 ", .yTitle(), " in Virginia")
    ) %>%
    hc_subtitle(
      align = "left",
      style = list(
        color = "#21445F"
      ),
      text = .subtitle
    )
}
