# # But : trouve automatiquement les colonnes d'intérêt issue du jeu de données
# # Entrée : le jeu de données brut
# # Sortie : une liste avec comme nom : "time"       "country"    "value"      "unit"       "indicators"


identify_columns <- function(df) {
  cols <- list()
  
  # Trouver la colonne temps (année)
  time_candidates <- c("TIME_PERIOD", "TIME", "YEAR", "obsTime")
  cols$time <- intersect(names(df), time_candidates)[1]
  
  # Trouver la colonne pays
  country_candidates <- c("REF_AREA", "COUNTRY", "LOCATION")
  cols$country <- intersect(names(df), country_candidates)[1]
  
  # Trouver la colonne valeur (les données numériques)
  value_candidates <- c("obsValue", "OBS_VALUE", "VALUE")
  cols$value <- intersect(names(df), value_candidates)[1]
  
  # Trouver la colonne unité
  unit_candidates <- c("UNIT_MEASURE", "UNIT", "UNITS", "Unit.of.measure", "Unité.de.mesure")
  cols$unit <- intersect(names(df), unit_candidates)[1]
  
  # Trouver les colonnes indicateurs (pour filtrer les données)
  indicator_candidates <- c("MEASURE", "INDICATOR", "VARIABLE", "SUBJECT", "Measure")
  cols$indicators <- intersect(names(df), indicator_candidates)[1]
  
  return(cols)
}

validate_columns <- function(cols, df) {
  errors <- c()
  # Vérifier les colonnes obligatoires
  if (is.null(cols$time) || !cols$time %in% names(df)) {
    errors <- c(errors, "Colonne temps/année non trouvée")
  }
  if (is.null(cols$country) || !cols$country %in% names(df)) {
    errors <- c(errors, "Colonne pays non trouvée")
  }
  if (is.null(cols$value) || !cols$value %in% names(df)) {
    errors <- c(errors, "Colonne valeur non trouvée")
  }
  if (is.null(cols$indicators) || !cols$indicators %in% names(df)) {
    errors <- c(errors, "Colonne indicateur non trouvée")
  }
  # Retourner les erreurs s'il y en a
  if (length(errors) > 0) {
    return(list(valid = FALSE, errors = errors))
  }
  
  return(list(valid = TRUE, errors = NULL))
}


prepare_wide_data <- function(df, cols, year_input) {
  indicator_col <- cols$indicators
  
  df_wide <- df %>%
    filter(.data[[cols$time]] == year_input) %>%
    select(cols$country, indicator_col, cols$value) %>%
    # Gérer les doublons en prenant la moyenne
    group_by(across(c(cols$country, indicator_col))) %>%
    summarise(value = mean(get(cols$value), na.rm = FALSE), .groups = "drop") %>%
    pivot_wider(
      names_from = indicator_col,
      values_from = value
    )
  
  return(df_wide)
}