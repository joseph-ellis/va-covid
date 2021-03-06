
HighChartBarDynamic <- function(.data, .yTitle) {
  hchart(
    name = .yTitle(),
    .data(),
    type = "column",
    hcaes(
      x = plotX,
      y = plotY
    )
  )
}

StyleBarDynamic <- function(.hc) {
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
  JS_tooltipFormatter <- V8::JS(
    "function () {
      if(this.y < 1 && this.y >=0) {
        return 'Age Group: ' + this.point.name + '<br />' +
        '<span style=\"font-size:1.2em; font-weight:bold; color:' +
        this.point.color + ';\"><strong>' + this.series.name + ': </strong>' +
        Highcharts.numberFormat((this.y * 100), 1) + '%</span>';
      }
      else {
        return 'Age Group: ' + this.point.name + '<br />' +
        '<span style=\"font-size:1.2em; font-weight:bold; color:' +
        this.point.color + ';\"><strong>' + this.series.name + ': </strong>' +
        Highcharts.numberFormat(this.y, 0) + '</span>';
      }
    }"
  )

  # callback function for statically positioning the tooltip
  JS_tooltipPositioner <- V8::JS(
    "function (labelWidth) {
      return {
        x: 50,
        y: 50
      };
  }"
  )

  .hc %>%
    hc_xAxis(
      tickInterval = 1,
      labels = list(
        align = "center",
        useHTML = TRUE
      ),
      title = list(
        enabled = FALSE
      )
    ) %>%
    hc_yAxis(
      title = list(
        enabled = FALSE
      ),
      endOnTick = TRUE,
      showLastLabel = TRUE,
      labels = list(
        formatter = JS_yAxisFormatter
      )
    ) %>%
    hc_plotOptions(
      column = list(
        borderWidth = 0,
        pointPadding = 0,
        colorByPoint = TRUE
      )
    ) %>%
    hc_tooltip(
      useHTML = TRUE,
      backgroundColor = "#FFF",
      shape = "square",
      formatter = JS_tooltipFormatter
    )
}
