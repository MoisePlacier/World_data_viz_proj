euclid_sim <- function(x, y) {
  1 / (1 + sqrt(sum((x - y)^2)))
}

calc_similarity <- function(dt, selected_country, vars) {
  dt_scaled <- copy(dt)
  # On ne scale pas ici si on veut garder les unités réelles
  x_ref <- as.numeric(dt_scaled[REF_AREA == selected_country, ..vars])
  sims <- dt_scaled[, .(REF_AREA, sim = apply(.SD, 1, function(y) euclid_sim(x_ref, as.numeric(y)))), .SDcols = vars]
  sims[]
}

calc_contrib <- function(dt, ref_country, clicked_country, vars){
  x <- as.numeric(dt[REF_AREA == ref_country, ..vars])
  y <- as.numeric(dt[REF_AREA == clicked_country, ..vars])
  
  diff <- y - x
  contrib <- diff / sum(abs(diff))  # pondération en conservant le signe
  
  data.table(variable = vars,
             difference = diff,
             contribution = contrib)[order(-abs(contribution))]
}



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


# Préparer les données au format wide (une colonne par indicateur)
prepare_wide_data <- function(df, cols) {
  indicator_col <- cols$indicators
  
  df_wide <- df %>%
    select(cols$time, cols$country, indicator_col, cols$value) %>%
    # Gérer les doublons en prenant la moyenne
    group_by(across(c(cols$time, cols$country, indicator_col))) %>%
    summarise(value = mean(get(cols$value), na.rm = FALSE), .groups = "drop") %>%
    pivot_wider(
      names_from = indicator_col,
      values_from = value
    )
  
  return(df_wide)
}