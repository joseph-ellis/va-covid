
#####################################################
## SET GLOBAL OPTIONS FOR PACKAGES WHERE NECESSARY ##
#####################################################

# set global thousand separator for Highcharts
hcOptions <- getOption("highcharter.lang")
hcOptions$thousandsSep <- ","
options(highcharter.lang = hcOptions)
