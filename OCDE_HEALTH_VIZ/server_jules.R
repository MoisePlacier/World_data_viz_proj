server <- function(input, output, session) {
  
  # Stockage des données chargées depuis une URL
  custom_data <- reactiveVal(NULL)
  custom_columns <- reactiveVal(NULL)
  
  # Charger les données depuis l'URL personnalisée
  observeEvent(input$load_url, {
    req(input$custom_url)
    
    showNotification("Chargement...", id = "loading", duration = NULL)
    
    tryCatch({
      # Lire les données SDMX
      sdmx_data <- readSDMX(input$custom_url)
      df <- as.data.frame(sdmx_data)
      # Sauvegarder dans le fichier des datasets préchargés
      saveRDS(df, file = paste0("data/", input$file_name, ".rds"))
      
      
      # Identifier et valider les colonnes
      cols <- identify_columns(df)
      validation <- validate_columns(cols, df)
      
      if (!validation$valid) {
        stop(paste("Erreurs de structure:", paste(validation$errors, collapse = ", ")))
      }
      
      # Stocker les données
      custom_data(df)
      custom_columns(cols)
      
      removeNotification("loading")
      showNotification("Chargé avec succès!", type = "message", duration = 3)
      
    }, error = function(e) {
      removeNotification("loading")
      showNotification(paste("Erreur:", e$message), type = "error", duration = 5)
      custom_data(NULL)
      custom_columns(NULL)
    })
  })
  
  # Renvoie les données selon le mode sélectionné
  data <- reactive({
    if (input$mode == "preloaded") {
      req(input$dataset_preloaded)
      readRDS(file.path("data", input$dataset_preloaded))
    } else {
      req(custom_data())
      custom_data()
    }
  })
  
  # Colonnes avec validation intégrée
  columns <- reactive({
    df <- data()
    req(df)
    
    cols <- if (input$mode == "preloaded") {
      identify_columns(df)
    } else {
      req(custom_columns())
      custom_columns()
    }
    
    # Valider les colonnes
    validation <- validate_columns(cols, df)
    if (!validation$valid) {
      showNotification(
        paste("Données invalides:", paste(validation$errors, collapse = ", ")),
        type = "error",
        duration = NULL
      )
      return(NULL)
    }
    
    cols
  })
  
  # Créé dynamiquement si la colonne indicateur existe
  output$indicator_ui <- renderUI({
    df <- data()
    cols <- columns()
    req(df, cols)
    
    indicators <- unique(df[[cols$indicators]])
    
    selectInput("indicateur",
                "Indicateur:",
                choices = indicators,
                selected = indicators[1])
    
  })
  
  output$year_ui <- renderUI({
    df <- data_filtered()
    cols <- columns()
    req(df, cols)
    
    # Plus de test : cols$time est garanti d'exister
    annees <- sort(unique(as.numeric(df[[cols$time]])))
    annees <- annees[!is.na(annees)]
    
    if (length(annees) == 0) {
      return(p("Aucune année disponible", style = "color: red;"))
    }
    
    sliderInput("annee", "Année:",
                min = min(annees),
                max = max(annees),
                value = max(annees),
                step = 1,
                sep = "")
  })
  
  
  data_filtered <- reactive({
    df <- data()
    cols <- columns()
    req(df, cols, cols$indicators, input$indicateur)
    
    df <- df %>% filter(.data[[cols$indicators]] == input$indicateur)
  })
  
  # Récupère l'unité de mesure pour les graphiques
  get_unit <- reactive({
    df <- data_filtered()
    cols <- columns()
    req(df, cols)
    
    if (!is.null(cols$unit)) {
      unit <- unique(df[[cols$unit]])[1]
      if (!is.na(unit) && unit != "") {
        return(paste0(" (", unit, ")"))
      }
    }
    return("")
  })
  
  # Calcule min/max sur toutes les années pour garder la même échelle
  global_range <- reactive({
    df <- data_filtered()
    cols <- columns()
    req(df, cols)
    
    valeurs <- as.numeric(df[[cols$value]])
    valeurs <- valeurs[is.finite(valeurs)]
    
    if (length(valeurs) > 0) {
      c(min(valeurs), max(valeurs))
    } else {
      c(0, 1)
    }
  })
  
  # Liste simple de tous les pays disponibles
  output$country_selector <- renderUI({
    df <- data_filtered()
    cols <- columns()
    req(df, cols)
    
    countries <- sort(unique(df[[cols$country]]))
    
    if (length(countries) == 0) {
      return(p("Aucun pays disponible", style = "color: red;"))
    }
    
    selectInput("countries",
                "Pays à afficher:",
                choices = countries,
                selected = c("FRA", "USA"),
                multiple = TRUE)
  })
  
  output$titre_carte <- renderText({
    req(input$annee)
    paste("Indicateur :", input$indicateur, "en", input$annee)
  })
  
  output$titre_graph <- renderText({
    cols <- columns()
    paste("Indicateur :", input$indicateur)
  })
  
  donnees_carte <- reactive({
    df <- data_filtered()
    cols <- columns()
    req(df, cols, input$annee)
    
    df_annee <- df %>%
      filter(.data[[cols$time]] == as.character(input$annee)) %>%
      group_by(.data[[cols$country]]) %>%
      summarise(valeur = mean(.data[[cols$value]], na.rm = TRUE), .groups = "drop")
    
    names(df_annee)[1] <- "country_code"
    
    world %>%
      left_join(df_annee, by = c("wb_a3" = "country_code"))
  })
  
  output$carte <- renderLeaflet({
    map_data <- donnees_carte()
    req(map_data)
    
    range_global <- global_range()
    valeurs_valides <- map_data$valeur[is.finite(map_data$valeur)]
    
    if(length(valeurs_valides) == 0) {
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lng = 0, lat = 20, zoom = 2)
    } else {
      pal <- colorNumeric(
        palette = "RdYlGn",
        domain = range_global,
        na.color = "grey"
      )
      
      leaflet(map_data) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lng = 0, lat = 20, zoom = 2) %>%
        addPolygons(
          fillColor = ~pal(valeur),
          fillOpacity = 0.9,
          color = "white",
          weight = 1,
          label = ~ifelse(is.finite(valeur),
                          paste0(name, ": ", round(valeur, 2)),
                          paste0(name, ": Pas de données")),
          
          highlightOptions = highlightOptions( # pour encadrer les pays quand on passe dessus
            weight = 2,
            color = "black",
            bringToFront = TRUE
          )
        ) %>%
        addLegend(
          pal = pal,
          values = range_global,
          title = paste0(get_unit()),
          position = "bottomright"
        )
    }
  })
  
  output$graphique_evolution <- renderPlot({
    df <- data_filtered()
    cols <- columns()
    req(df, cols, input$countries)
    
    plot_data <- df %>%
      filter(.data[[cols$country]] %in% input$countries) %>%
      mutate(year = as.numeric(.data[[cols$time]])) %>%
      group_by(.data[[cols$country]], year) %>%
      summarise(value = mean(as.numeric(.data[[cols$value]]), na.rm = TRUE), .groups = "drop")
    
    if (nrow(plot_data) == 0) {
      plot.new()
      text(0.5, 0.5, "Pas de données disponibles", cex = 1.5)
      return()
    }
    
    ggplot(plot_data, aes(
      x = year,
      y = value,
      color = .data[[cols$country]]
    )) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 3, shape = 21, stroke = 0.8, fill = "white") +  # petits points avec bord
      labs(
        x = "Année",
        y = paste0(get_unit()),
        color = "Pays"
      ) +
      labs(title = "Evolution temporelle de l'indicateur") +
      scale_color_brewer(palette = "Set2") + 
      theme_minimal(base_size = 14) +
      theme(
        axis.title = element_text(face = "bold"),           # titres en gras
        axis.line = element_line(color = "grey40"),         # lignes axes 
        panel.grid.minor = element_blank(),                # grille mineure 
        panel.grid.major = element_line(color = "grey80"), # grille majeure 
        legend.position = "top",                            # légende en haut
        legend.title = element_text(face = "bold"),         # titre légende en gras
        legend.key = element_rect(fill = "transparent"),   # fond légende transparent
        plot.title = element_text(face = "bold", hjust = 0.5)
      )
    
  })
  
  output$n_lignes <- renderText({
    nrow(data_filtered())
  })
  
  output$n_colonnes <- renderText({
    ncol(data_filtered())
  })
  
  output$n_pays <- renderText({
    df <- data_filtered()
    cols <- columns()
    req(df, cols)
    length(unique(df[[cols$country]]))
  })
  
  output$n_annees <- renderText({
    df <- data_filtered()
    cols <- columns()
    req(df, cols)
    length(unique(df[[cols$time]]))
  })
  
  output$n_indicateurs <- renderText({
    df <- data_filtered()
    cols <- columns()
    req(df, cols)
    length(unique(df[[cols$indicators]]))
  })
  
  output$plage_temporelle <- renderText({
    df <- data_filtered()
    cols <- columns()
    req(df, cols)
    annees <- as.numeric(df[[cols$time]])
    annees <- annees[!is.na(annees)]
    if (length(annees) > 0) {
      paste0(min(annees), " - ", max(annees))
    } else {
      "-"
    }
  })
  
  output$data_preview <- renderTable({
    head(data_filtered(), 10)
  })
  
  # Préparer les données wide
  data_wide <- reactive({
    req(data(), columns())
    prepare_wide_data(data(), columns())
  })
  
  # Graphique principal
  output$missing_plot <- renderPlot({
    req(data_wide())
    aggr(data_wide(), only.miss = TRUE, sortVar = TRUE)
  })
  
  # Résumé détaillé
  output$missing_summary <- renderPrint({
    req(data_wide())
    res <- summary(aggr(data_wide(), prop = TRUE, combined = TRUE))$combinations
    res[rev(order(res[,2])),]
  })
}