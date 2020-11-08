
# basic cloumn-line combo chart
HighChartMultiAreaLineDate <- function(.data, .yTitle) {
  highchart() %>%
    hc_add_series(
      #name = .yTitle(),
      data = .data(),
      type = "area",
      hcaes(
        x = as.Date(plotX),
        y = plotYC,
        group = plotGroup
      )
    ) %>%
    hc_add_series(
      #name = "7-Day Moving Average",
      data = .data(),
      type = "line",
      hcaes(
        x = as.Date(plotX),
        y = plotYL,
        group = plotGroup
      )
    )
}

JS_tooltipFormatter <- V8::JS(
  "function () {
      return this.points.reduce(function(s, point) {
        if(point.y % 1 == 0) {
          return s + '<span style=\"color:' + point.color + '\">●</span>' +
          '<strong>' + point.series.name + ' - New:  </strong>' + point.y + '<br />';
        }
        else {
          return s + '<span style=\"color:' + point.color + '\">●</span>' +
          '<strong>' + point.series.name + ' - 7-Day Average:  </strong>' + point.y + '<br />';
        }
      }, Highcharts.dateFormat('%B %d, %Y', this.x) + '<br />');
    }"
)

# application specific styling
StyleMultiAreaLineDate <- function(.hc, .yTitle, .subtitle) {

  # styling
  .hc %>%
    hc_colors(
      colors = c("#7CB5EC", "#F7A35C", "#21445F", "#CC5500")
    ) %>%
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
    hc_plotOptions(
      area = list(
        fillOpacity = 0.5,
        marker = list(
          enabled = FALSE
        ),
        shadow = TRUE
      ),
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
