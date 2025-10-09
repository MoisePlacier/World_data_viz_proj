library(shiny)
library(ggplot2)
library(dplyr)
library(leaflet)
library(rnaturalearth)
library(sf)
library(countrycode)
library(rsdmx)
library(bslib)
library(bsicons)

# Charger la carte du monde pour la visualisation
world <- ne_countries(scale = "medium", returnclass = "sf")

# Trouve automatiquement les colonnes clés dans le jeu de données -- en grande majorité les mêmes colonnes mais possible d'avori d'autres choses
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
  unit_candidates <- c("UNIT_MEASURE", "UNIT", "UNITS")
  cols$unit <- intersect(names(df), unit_candidates)[1]
  
  # Trouver les colonnes indicateurs (pour filtrer les données)
  indicator_candidates <- c("MEASURE", "INDICATOR", "VARIABLE", "SUBJECT")
  cols$indicators <- intersect(names(df), indicator_candidates)
  
  return(cols)
}

# Liste des jeux de données disponibles par défaut
#preloaded_datasets <- list(
  #"Croissance PIB" = "gdp_growth",
  #"Qualité de l'eau" = "water_quality",
  #"Outlook Agriculture" = "outlook_agr"
#)

preloaded_datasets <- list.files("data/", pattern = "rds", full.names = FALSE)

ui <- fluidPage(
  br(),
  titlePanel("Visualisation des données OCDE"),
  br(),
  theme = bs_theme(
    bootswatch = "darkly",
    base_font = font_google("Inter"),
    navbar_bg = "#25443B"
  ),
  sidebarLayout( # séparé en deux : le sidebar panel avec les selecteurs et le main panel avec la carte et les graphs
    sidebarPanel(
      width = 3,
      
      # Pré chargés ou issus d'URL : --> on traite ça avec les conditionnal pannel
      radioButtons("mode", "Source des données:",
                   choices = c("Datasets pré-chargés" = "preloaded",
                               "URL personnalisée" = "custom")),
      
      # Si dataset pré-chargé : menu déroulant
      conditionalPanel(
        condition = "input.mode == 'preloaded'",
        selectInput("dataset_preloaded", "Jeu de données:",
                    choices = preloaded_datasets)
      ),
      
      # Si URL personnalisée : zone de texte + bouton
      conditionalPanel(
        condition = "input.mode == 'custom'",
        textAreaInput("custom_url", "URL SDMX OCDE:",
                      value = "", rows = 3,
                      placeholder = "Collez l'URL ici..."),
        textInput("file_name", "Nom du fichier", value = "fichier"),
        actionButton("load_url", "Charger", class = "btn-success") # comme dans la doc
      ),
      
      hr(),
      
      # Sélecteur d'indicateur dynamique
      uiOutput("indicator_ui"),
      
      hr(),
      helpText("Source: API SDMX OCDE")
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Carte",
                 # Curseur dynamique pour l'année 
                 uiOutput("year_ui"),
                 h3(textOutput("titre_carte")),
                 leafletOutput("carte", height = 450),
                 br()
        ),
        
        tabPanel("Évolution temporelle",
                 br(),
                 h4("Sélectionner les pays:"),
                 uiOutput("country_selector"),
                 br(),
                 plotOutput("graphique_evolution", height = 450)
        ),
        
        tabPanel("Structure",
                 card(
                   card_header("Structure du jeu de données"),
                   card_body(
                     layout_columns(
                       fill = FALSE,
                     value_box(
                       title = "Nombre de lignes",
                       value = textOutput("n_lignes"),
                       showcase = bsicons::bs_icon("list-ol")
                     ),
                     value_box(
                       title = "Nombre de colonnes",
                       value = textOutput("n_colonnes"),
                       showcase = bsicons::bs_icon("columns-gap")
                     ),
                     value_box(
                       title = "Nombre de pays",
                       value = textOutput("n_pays"),
                       showcase = bsicons::bs_icon("globe")
                     ),
                   value_box(
                     title = "Nombre d'années",
                     value = textOutput("n_annees"),
                     showcase = bsicons::bs_icon("calendar3")
                   ),
                   value_box(
                     title = "Nombre d'indicateurs",
                     value = textOutput("n_indicateurs"),
                     showcase = bsicons::bs_icon("bar-chart")
                   ),
                   value_box(
                     title = "Plage temporelle",
                     value = textOutput("plage_temporelle"),
                     showcase = bsicons::bs_icon("clock")
                   )
                 )),
                 br(),
                 h4("Aperçu des données"),
                 card(
                   full_screen = TRUE,
                   card_header("10 premières lignes :"),
                   card_body(
                     tableOutput("data_preview")
                   )
                 ))
        )
      )
      )
    )
  )



# Enable thematic
thematic::thematic_shiny(font = "auto")

# Change ggplot2's default "gray" theme
theme_set(theme_bw(base_size = 16))

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
      
      # on sauvegarde dans le fichier des datasets préchargés 
      saveRDS(df, file = paste("data/", input$file_name, ".rds"))
      
      cols <- identify_columns(df)
      
      # Stocker les données
      custom_data(df)
      custom_columns(cols)
      
      removeNotification("loading")
      showNotification("Chargé avec succès!", type = "message", duration = 3)
      
    }, error = function(e) {
      removeNotification("loading")
      showNotification(paste("Erreur:", e$message), type = "error")
      custom_data(NULL)
      custom_columns(NULL)
    })
  })
  
  # Renvoie les données selon le mode sélectionné
  data <- reactive({
    if (input$mode == "preloaded") {
      # Charger depuis un fichier RDS
      dataset_file <- preloaded_datasets[input$dataset_preloaded]
      readRDS(paste0("data/", dataset_file, ".rds"))
    } else {
      # Utiliser les données personnalisées
      req(custom_data())
      custom_data()
    }
  })
  
  columns <- reactive({
    if (input$mode == "preloaded") {
      identify_columns(data())
    } else {
      req(custom_columns())
      custom_columns()
    }
  })
  
  # Créé dynamiquement si la colonne indicateur existe
  output$indicator_ui <- renderUI({
    df <- data()
    cols <- columns()
    req(df, cols)
    
    # Vérifier s'il y a une colonne indicateur
    if (length(cols$indicators) > 0) {
      indicator_col <- cols$indicators[1]
      indicators <- unique(df[[indicator_col]])
      
      selectInput("indicateur",
                  paste("Indicateur:"),
                  choices = indicators,
                  selected = indicators[1])
    }
  })
  
  output$year_ui <- renderUI({
    df <- data_filtered()
    cols <- columns()
    req(df, cols, cols$time)
    
    # Extraire les années disponibles
    annees <- sort(unique(as.numeric(df[[cols$time]])))
    annees <- annees[!is.na(annees)]
    
    if (length(annees) == 0) {
      return(p("Aucune année disponible", style = "color: red;"))
    }
    
    # Curseur pour sélectionner l'année
    sliderInput("annee", "Année:",
                min = min(annees),
                max = max(annees),
                value = max(annees),  # Par défaut: année la plus récente
                step = 1,
                sep = "")
  })
  
  data_filtered <- reactive({
    df <- data()
    cols <- columns()
    req(df, cols)
    
    # Filtrer par l'indicateur sélectionné si applicable
    if (!is.null(input$indicateur) && length(cols$indicators) > 0) {
      indicator_col <- cols$indicators[1]
      df <- df %>% filter(.data[[indicator_col]] == input$indicateur)
    }
    
    df
  })
  
  # Récupère l'unité de mesure pour les graphiques
  get_unit <- reactive({
    df <- data_filtered()
    cols <- columns()
    req(df, cols)
    
    # Si la colonne unité existe, récupérer la première valeur
    if (!is.null(cols$unit) && cols$unit %in% names(df)) {
      unit <- unique(df[[cols$unit]])[1]
      if (!is.na(unit) && unit != "") {
        return(paste0(" (", unit, ")"))
      }
    }
    return("")  # Retourne vide si pas d'unité
  })
  
  # Calcule min/max sur toutes les années pour garder la même échelle
  global_range <- reactive({
    df <- data_filtered()
    cols <- columns()
    req(df, cols)
    
    valeurs <- as.numeric(df[[cols$value]])
    valeurs <- valeurs[is.finite(valeurs)]
    
    if (length(valeurs) > 0) {
      return(c(min(valeurs), max(valeurs)))
    } else {
      return(c(0, 1))
    }
  })
  
  # Liste simple de tous les pays disponibles
  output$country_selector <- renderUI({
    df <- data_filtered()
    cols <- columns()
    req(df, cols)
    
    # Obtenir la liste de tous les pays
    countries <- sort(unique(df[[cols$country]]))
    
    if (length(countries) == 0) {
      return(p("Aucun pays disponible", style = "color: red;"))
    }
    
    # Sélection multiple avec les 5 premiers pays par défaut
    selectInput("countries",
                "Pays à afficher:",
                choices = countries,
                selected = head(countries, 5),
                multiple = TRUE)
  })
  
  output$titre_carte <- renderText({
    req(input$annee)
    if (!is.null(input$indicateur)) {
      paste(input$indicateur, "en", input$annee)
    } else {
      paste("Données en", input$annee)
    }
  })
  
  donnees_carte <- reactive({
    df <- data_filtered()
    cols <- columns()
    req(df, cols, input$annee)
    
    # Filtrer pour l'année sélectionnée
    df_annee <- df %>%
      filter(.data[[cols$time]] == as.character(input$annee)) %>%
      group_by(.data[[cols$country]]) %>%
      summarise(valeur = mean(.data[[cols$value]], na.rm = TRUE), .groups = "drop") %>%
      filter(is.finite(valeur))
    
    names(df_annee)[1] <- "country_code"
    
    # Joindre avec la carte du monde
    world %>%
      left_join(df_annee, by = c("wb_a3" = "country_code"))
  })
  
  output$carte <- renderLeaflet({
    map_data <- donnees_carte()
    req(map_data)
    
    # Utiliser la plage globale pour toutes les années
    range_global <- global_range()
    
    # Vérifier qu'il y a des données à afficher
    valeurs_valides <- map_data$valeur[is.finite(map_data$valeur)]
    
    if(length(valeurs_valides) == 0) {
      # Carte vide si pas de données (mais garde la vue)
      leaflet() %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lng = 0, lat = 20, zoom = 2)
    } else {
      # Palette de couleurs avec plage fixe
      pal <- colorNumeric(
        palette = "RdYlGn",
        domain = range_global,  # Utilise la plage globale
        na.color = "#d3d3d3"
      )
      
      # Créer la carte
      leaflet(map_data) %>%
        addProviderTiles(providers$CartoDB.Positron) %>%
        setView(lng = 0, lat = 20, zoom = 2) %>%  # Vue fixe
        addPolygons(
          fillColor = ~pal(valeur),
          fillOpacity = 0.7,
          color = "white",
          weight = 1,
          # Étiquette au survol
          label = ~ifelse(is.finite(valeur),
                          paste0(name, ": ", round(valeur, 2)),
                          paste0(name, ": Pas de données")),
          highlightOptions = highlightOptions(
            weight = 2,
            color = "black",
            bringToFront = TRUE
          )
        ) %>%
        addLegend(
          pal = pal,
          values = range_global,  # Légende basée sur la plage globale
          title = paste0("Valeur", get_unit()),
          position = "bottomright"
        )
    }
  })
  
  output$graphique_evolution <- renderPlot({
    df <- data_filtered()
    cols <- columns()
    req(df, cols, input$countries)
    
    # Créer les données du graphique
    plot_data <- df %>%
      filter(.data[[cols$country]] %in% input$countries) %>%
      mutate(year = as.numeric(.data[[cols$time]])) %>%
      group_by(.data[[cols$country]], year) %>%
      summarise(value = mean(as.numeric(.data[[cols$value]]), na.rm = TRUE), .groups = "drop")
    
    if (nrow(plot_data) == 0) {
      # Message si pas de données
      plot.new()
      text(0.5, 0.5, "Pas de données disponibles", cex = 1.5)
      return()
    }
    
    # Titre du graphique
    titre <- if (!is.null(input$indicateur)) {
      paste("Évolution de", input$indicateur)
    } else {
      "Évolution temporelle"
    }
    
    # Graphique en lignes
    ggplot(plot_data, aes(x = year, y = value, color = .data[[cols$country]])) +
      geom_line(linewidth = 1.2) +
      geom_point(size = 2) +
      labs(x = "Année", y = paste0("Valeur", get_unit()), color = "Pays", title = titre) +
      theme_minimal(base_size = 14)
  })
  
  output$n_lignes <- renderText({
    df <- data_filtered()
    req(df)
    nrow(df)
  })
  
  output$n_colonnes <- renderText({
    df <- data_filtered()
    req(df)
    ncol(df)
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
    if (length(cols$indicators) > 0) {
      length(unique(df[[cols$indicators[1]]]))
    } else {
      "-"
    }
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
    df <- data_filtered()
    req(df)
    head(df, 10)
  })
}

shinyApp(ui = ui, server = server)
