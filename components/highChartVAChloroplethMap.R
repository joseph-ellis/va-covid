
# callback function for formatting tooltip values
JS_tooltipPointFormatter <- V8::JS(
  "function () {
      if(this.value < 1 && this.value > 0) {
        return '<strong>' + this.name + ':  </strong>' +
          Highcharts.numberFormat((this.value * 100), 2) + '%';
      }
      else {
        return '<strong>' + this.name + ':  </strong>' +
          Highcharts.numberFormat(this.value, 0);
      }
    }"
)

JS_clickEvent <- V8::JS(
  "function () {
    Shiny.onInputChange('mapClick', {
      locality: this.name
    })
  }"
)

JS_pointSelect <- V8::JS(
  "function () {
    const text = 'You selected ' + this.name + ' (' + this.value)',
    chart = this.series.chart;
    if (!chart.selectedLabel) {
      chart.selectedLabel = chart.renderer.label(text, 0, 320)
      .add();
    }
    else {
      chart.selectedLabel.attr({
        text: text
      });
    }
  }"
)

HighChartVAChloroplethMap <- function(.data, .value, .name) {
  hcmap(
    map = "countries/us/us-va-all",
    data = .data(),
    value = .value,
    joinBy = "fips",
    name = .name()
  ) %>%
    hc_colorAxis(
      #stops = ViridisStops(10, .option = "B")
      stops = BrewerStops(8, "RdYlGn", .reverse = TRUE)
    ) %>%
    hc_plotOptions(
      map = list(
        point = list(
          events = list(
            click = JS_clickEvent
          )
        ),
        states = list(
          select = list(
            borderColor = "#FFFFFF",
            borderWidth = 2,
            color = "#04BF9D"
          )
        )
      )
    ) %>%
    hc_tooltip(
      useHTML = TRUE,
      headerFormat = "{series.name} <br />",
      pointFormatter = JS_tooltipPointFormatter
    )
}

