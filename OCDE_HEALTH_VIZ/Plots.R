
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
      labelOptions = labelOptions(
        direction = "auto",
        sticky = TRUE,   
        textsize = "12px"
      ),
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
  
  title_text <- paste("Score :", gsub("_", " ", var))
  title_wrapped <- gsub("(.{1,15})(\\s|$)", "\\1<br>", title_text)
  
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
      labelOptions = labelOptions(
        direction = "auto",
        sticky = TRUE,    # <--- fait que le label reste affiché
        textsize = "12px"
      ),
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
      title = paste(title_wrapped)
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





render_super_plot <- function(output, output_id, dt, dict, scores_global, geo_sf,
                              ref_country, comp_country, vars_groupe, user_choice) {
  
  output[[output_id]] <- renderPlotly({
    
    
    scores <- copy(scores_global)
    # Ajout du nom de pays
    scores <- merge(scores, geo_sf[, c("REF_AREA", "sovereignt")],
                    by = "REF_AREA", all.x = TRUE)
    scores$REF_AREA <- scores$sovereignt
    scores$sovereignt <- NULL
    scores$geometry <- NULL
    
    
    df_long <- melt(
      dt[, c("REF_AREA", ..vars_groupe)],
      id.vars = "REF_AREA",
      variable.name = "variable",
      value.name = "valeur"
    )
    
    df_long[, REF_AREA := geo_sf$sovereignt[REF_AREA]]
    
    # Moyennes et extrêmes
    moyenne <- df_long[, .(
      moyenne_globale = mean(valeur, na.rm = TRUE),
      valeur_max = max(valeur, na.rm = TRUE),
      valeur_min = min(valeur, na.rm = TRUE),
      pays_max = REF_AREA[which.max(valeur)],
      pays_min = REF_AREA[which.min(valeur)]
    ), by = variable]


    df_long <- merge(df_long, moyenne, by = "variable", all.x = TRUE)
    
    df_plot <- df_long[REF_AREA %in% c(ref_country, comp_country)]
    
    ordre_vars <- df_plot[REF_AREA == ref_country][order(valeur, decreasing = TRUE), variable]
    df_plot[, variable := factor(variable, levels = ordre_vars)]
    
    
    
    
    # --- Construction du graphique ---
    p <- plot_ly() %>%
      add_bars(
        data = df_plot,
        x = ~valeur,
        y = ~variable,
        color = ~REF_AREA,
        colors = c("#1f77b4", "#ff7f0e"),
        orientation = "h",
        text = "",
        textposition = "none",
        hoverinfo = "text",
        hovertext = ~paste0(REF_AREA, "<br>", variable, " : ", round(valeur, 2))
      )
    
    if ("moyenne_globale" %in% user_choice) {
      p <- p %>% add_trace(
        data = df_plot,
        x = ~moyenne_globale,
        y = ~variable,
        type = "scatter",
        mode = "markers",
        marker = list(color = "#2ca02c", size = 16, symbol = "star-diamond",line = list(color = "black",width = 1)),
        name = "Moyenne globale",
        hoverinfo = "text",
        text = ~paste0("Moyenne globale<br>", variable, " : ", round(moyenne_globale, 2))
      )
    }
    
    if ("valeur_min" %in% user_choice) {
      p <- p %>% add_trace(
        data = df_plot,
        x = ~valeur_min,
        y = ~variable,
        type = "scatter",
        mode = "markers",
        marker = list(color = "#d62728", size = 16, symbol = "star-diamond",line = list(color = "black",width = 1)),
        name = "Valeur min",
        hoverinfo = "text",
        text = ~paste0(
          "Valeur min<br>", variable, " : ", round(valeur_min, 2),
          "<br>Pays : ", pays_min
        )
      )
    }
    
    if ("valeur_max" %in% user_choice) {
      p <- p %>% add_trace(
        data = df_plot,
        x = ~valeur_max,
        y = ~variable,
        type = "scatter",
        mode = "markers",
        marker = list(color = "#9467bd", size = 16, symbol = "star-diamond",line = list(color = "black",width = 1)),
        name = "Valeur max",
        hoverinfo = "text",
        text = ~paste0(
          "Valeur max<br>", variable, " : ", round(valeur_max, 2),
          "<br>Pays : ", pays_max
        )
      )
    }
    
    p %>% layout(
      barmode = "group",
      xaxis = list(title = "Valeur observée" ,font = list(size = 20)),
      yaxis = list(title = "", tickfont = list(size = 16), automargin = TRUE),
      legend = list(title = list(text = "Indicateur"), font = list(size = 20),automargin = TRUE),
      hoverlabel = list(bgcolor = "white", font = list(size = 13))
    )
  })
}

