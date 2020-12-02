
# FUNCTION - append metrics to dataframe
AppendMetrics <- function(.data) {
  .data %>%
    mutate(
      CasesPerCapita = TotalCases / Population2019,
      CasesPer100K = CasesPerCapita * 1e5,
      PercentCasesHospitalized = Hospitalizations / TotalCases,
      HospitalizationPerCapita = Hospitalizations / Population2019,
      HospitalizationsPer100K = HospitalizationPerCapita * 1e5,
      MortalityRate = Deaths / TotalCases,
      DeathsPerCapita = Deaths / Population2019,
      DeathsPer100K = DeathsPerCapita * 1e5,
      across(
        .cols = where(is.numeric),
        .fns = ~ round(
          .x,
          4
        )
      ),
      across(
        .cols = where(is.numeric),
        .fns = ~ replace(
          .x,
          is.nan(.x),
          0
        )
      )
    )
}


