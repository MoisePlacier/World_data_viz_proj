####----
### Data importation (global)
dta_pib_sante <- readRDS("../Data/dta_pib_sante.rds")
dta_air_quality <- readRDS("../Data/dta_air_quality.rds")
dta_agri <- readRDS("../Data/dta_agri.rds")
dta_health <- readRDS("../Data/dta_health.rds")
dta_pib_hab <- readRDS("../Data/dta_pib_hab.rds")
dta_pib <- readRDS("../Data/dta_pib.rds")
dta_medicament <- readRDS("../Data/dta_medicament.rds")
dta_RH_sante <- readRDS("../Data/dta_RH_sante.rds")

# helper: liste de tables utilisées pour les heatmaps / corrélations
liste_tableaux <- list(
  dta_pib_sante %>% group_by(Reference.area, TIME_PERIOD) %>%
    slice_max(order_by = OBS_VALUE, n = 1, with_ties = FALSE) %>% ungroup(),
  dta_air_quality, dta_health, dta_pib_hab, dta_pib, dta_medicament,
  dta_RH_sante %>% filter(Health.profession == "Total", Unit.of.measure == "Per 1 000 inhabitants") %>% select(-Health.profession)
)

# calculer années disponibles dans les jeux de données (pour le slider)
years_all <- sort(unique(na.omit(c(
  dta_pib_sante$TIME_PERIOD,
  dta_air_quality$TIME_PERIOD,
  dta_agri$TIME_PERIOD,
  dta_health$TIME_PERIOD,
  dta_pib_hab$TIME_PERIOD,
  dta_pib$TIME_PERIOD,
  dta_medicament$TIME_PERIOD,
  dta_RH_sante$TIME_PERIOD
))))

# ---- Déterminer la liste des pays disponibles ----
countries_all <- sort(unique(c(
  dta_pib_sante$Reference.area,
  dta_air_quality$Reference.area,
  dta_agri$Reference.area,
  dta_health$Reference.area,
  dta_pib_hab$Reference.area,
  dta_pib$Reference.area,
  dta_medicament$Reference.area,
  dta_RH_sante$Reference.area
)))

years_all <- years_all[years_all >= 2015]

min_year <- min(years_all, na.rm = TRUE)
max_year <- max(years_all, na.rm = TRUE)

# fonction existante (inchangée)
extract_indicator <- function(data, measure_name, year) {
  data[
    Measure == measure_name & TIME_PERIOD == year, .(Reference.area, OBS_VALUE)
  ]
}