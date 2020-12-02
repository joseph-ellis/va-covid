library(RSocrata)
library(tidyverse)
library(config)

# load Virginia map data
load("data/VaMapData.RData")

# load Virginia population data
vaPop2019 <- read_csv(
  file = "data/va-pop-2019.csv",
  col_types = cols(
    fips = col_character(),
    Population2019 = col_number()
  )
)

# load Virginia population data by age
vaPopAge2019 <- read_csv(
  file = "data/va-pop-age-2019.csv"
)

# FUNCTION - make a request to the VDH Socrata Open Data API
VdohApiRequest <- function(.url, .credentials) {
  read.socrata(
    url = .url,
    app_token = .credentials[[1]],
    email     = .credentials[[2]],
    password  = .credentials[[3]]
  )
}

# VDH Socrata Open Data API endpoints
api_urls <- list(
  CasesLocality = "https://data.virginia.gov/resource/bre9-aqqr.json",
  CasesByConfirmationState = "https://data.virginia.gov/resource/uqs3-x7zh.json",
  EventDateHealthPlanningRegion = "https://data.virginia.gov/resource/9d6i-p8gz.json",
  CasesByAgeGroupDistrict = "https://data.virginia.gov/resource/uktn-mwig.json",
  CasesByRaceEthnicityDistrict = "https://data.virginia.gov/resource/9sba-m86n.json",
  CasesBySexDistrict = "https://data.virginia.gov/resource/tdt3-q47w.json",
  HospitalKeyMeasuresState = "https://data.virginia.gov/resource/28wk-762y.json",
  TestingByZip = "https://data.virginia.gov/resource/8bkr-zfqv.json",
  Outbreaks = "https://data.virginia.gov/resource/kqre-szn4.json",
  OutbreaksDetail = "https://data.virginia.gov/resource/rx3b-xxds.json",
  TestingByReportDateDistrict = "https://data.virginia.gov/resource/3u5k-c2gr.json",
  MiscDistrict = "https://data.virginia.gov/resource/nkw4-x92z.json",
  CliHealthRegion = "https://data.virginia.gov/resource/m2rx-3wgw.json",
  CliHealthDistrict = "https://data.virginia.gov/resource/nchp-nti3.json"
)

# load API credentials
# In order to call the Virginia Department of Health Open Data API, you must
# register your application and generate a token. This application uses a
# config.yml file at the project root to store the app token, registered email
# address, and password necessary to access the API.
creds <- get(value = "covidApp")

# get data from VDH Socrata Open Data API
vdoh_data <- api_urls %>%
  map(
    .f = VdohApiRequest,
    creds
)

# remove unnecessary objects from environment
rm(api_urls, creds, VdohApiRequest)
