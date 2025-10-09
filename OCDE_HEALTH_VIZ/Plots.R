
render_map_similarity <- function(geo_sf, dt, ref_country, vars) {

  if (!(ref_country %in% geo_sf$sovereignt)) {
    warning("Le pays de référence choisi n'existe pas dans geo_sf$sovereignt.")
    return(NULL)
  }
  ref_code <- geo_sf %>%
    filter(sovereignt == ref_country) %>%
    pull(REF_AREA)
  
  sims <- calc_similarity(dt, ref_code, vars)
  sims <- as.data.table(sims)
  sims <- sims[!(REF_AREA %in% ref_code)]
  
  # Fusion avec les géométries
  map_data <- merge(geo_sf, sims, by = "REF_AREA", all.x = TRUE)
  
  #  Définition de la palette
  pal <- colorNumeric("Blues", domain = map_data$sim, na.color = "white")

  leafletProxy("map") %>%
    clearShapes() %>%
    clearControls() %>%
    addPolygons(
      data = map_data,
      layerId = ~REF_AREA,
      fillColor = ~pal(sim),
      fillOpacity = 0.8,
      color = "white",
      weight = 0.5,
      label = ~paste0(sovereignt, ": ", round(sim, 3)),
      highlightOptions = highlightOptions(
        weight = 1,
        color = 'black',
        bringToFront = TRUE
      )
    ) %>%
    addLegend(
      "bottomright",
      pal = pal,
      values = map_data$sim,
      title = paste("Similarité à", ref_country)
    )
  return(sims)
}

#####################################################################################

render_map_scores <- function(geo_sf, scores_global, var = "score_global") {
  map_data <- merge(geo_sf, scores_global, by = "REF_AREA", all.x = TRUE)
  
  if (!(var %in% colnames(map_data))) {
    warning(paste("La variable", var, "n'existe pas dans scores_global."))
    return(NULL)
  }
  
  pal <- colorNumeric("RdYlGn", domain = map_data[[var]], na.color = "grey90")
  
  leafletProxy("map") %>%
    clearShapes() %>%
    clearControls() %>%
    addPolygons(
      data = map_data,
      layerId = ~REF_AREA,
      fillColor = ~pal(get(var)),
      fillOpacity = 0.8,
      color = "white",
      weight = 0.5,
      label = ~paste0(sovereignt, ": ", round(get(var), 2)),
      highlightOptions = highlightOptions(
        weight = 1,
        color = "black",
        bringToFront = TRUE
      )
    ) %>%
    addLegend(
      "bottomright",
      pal = pal,
      values = map_data[[var]],
      title = paste("Score :", gsub("_", " ", var))
    )
}





render_data_table <- function(output, output_id, data_vector, titre = NULL) {
  
  output[[output_id]] <- renderUI({
    req(data_vector)
    
    # Si le vecteur est simple, le convertir en data.frame
    if (is.vector(data_vector)) {
      data <- data.frame(Variable = data_vector)
    } else {
      data <- as.data.frame(data_vector)
    }
    
    tagList(
      if (!is.null(titre)) h4(titre),
      DT::datatable(
        data,
        options = list(
          scrollY = "300px",
          paging = FALSE,
          searching = FALSE,
          info = FALSE,
          dom = 't'
        ),
        rownames = FALSE
      )
    )
  })
}

