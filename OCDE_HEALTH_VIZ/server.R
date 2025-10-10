library(ggplot2)
library(RColorBrewer)
library(shiny)
library(leaflet)
library(data.table)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(dplyr)
source("Utilis.R")
source("Plots.R")
source("fonctions_jules.R")
source("fonctions_tom.R")
library(DT)
library(shinyBS) 
library(plotly)


library(countrycode)
library(rsdmx)
library(bslib)
library(tidyverse)
library(VIM)
library(bsicons)

# Enable thematic
thematic::thematic_shiny(font = "auto")

# Change ggplot2's default "gray" theme
theme_set(theme_bw(base_size = 16))



# Charger le fond de carte
geo_sf <- ne_countries(scale = "medium", returnclass = "sf")

#### data
dt <- readRDS("../Data/merged_imputed.rds")
dt <- as.data.table(dt)
dt[, REF_AREA := as.factor(REF_AREA)]


#### dict 
dict <- readRDS("../Data/Labels_dict.rds")
dict <- as.data.table(dict)
setnames(dt, old = dict$var_full, new = dict$var_label_fr)

###################################
# Filtrer le fond de carte
filtre <- dt$REF_AREA
geo_sf <- geo_sf[geo_sf$gu_a3 %in% filtre,]
# Ajouter une colonne REF_AREA 
geo_sf$REF_AREA <- geo_sf$gu_a3
########################################

########## scores 
scores_global <- readRDS("../Data/scores_globaux.rds")
scores_global <- as.data.table(scores_global)

####### groupes de variables pour les scores 
vars_groupes_scores <- readRDS("../Data/vars_groupes_scores.rds")
vars_groupes_scores <- as.data.table(vars_groupes_scores)






############## Datas Jules 
world <- ne_countries(scale = "medium", returnclass = "sf")

preloaded_datasets <- list.files("../Data/", pattern = "\\.rds$", full.names = FALSE)


###########################@
# Partie de Tom

# app.R
library(shiny)
library(ggplot2)
library(plotly)
library(dplyr)
library(tidyr)
library(purrr)
library(igraph)
library(ggraph)
library(reshape2)
library(ggrepel)
library(data.table)
library(scales)
library(broom)
library(viridis)
library(FactoMineR)
library(factoextra)

#########################


function(input, output, session){
  
####################################################################
  ######################tableau des variables #########################
  
  observeEvent({
    input$viz_mode
    input$var_group
    input$score_var
  }, {
    req(input$viz_mode)
    
    if (input$viz_mode == "Visualiser les similarités entre pays") {
      vars_for_sim <- dict[var_group == input$var_group, var_label_fr]
      render_data_table(
        output,
        "my_table",
        vars_for_sim,
        titre = paste("Détail des variables utilisées pour le calcul de la similarité entre les pays")
      )
    } 
    
    else if (input$viz_mode == "Visualiser les scores de performances de santé des pays") {
      if (input$score_var != "score_global") {
        vars_for_scores <- dict[var_group == input$score_var, var_label_fr]
        #vars_for_scores <- unique(vars_groupes_scores[[input$score_var]])
      } else {
        # Toutes les variables de toutes les colonnes
        vars_for_scores <- unique(unlist(vars_groupes_scores))
      }
      
      render_data_table(
        output,
        "my_table",
        vars_for_scores,
        titre = paste("Détail des variables utilisées pour le calcul du score")
      )
    }
  })
  
  

  
  ############## Carte initiale ###################
  # output$map <- renderLeaflet({
  #   leaflet(geo_sf) %>%
  #     addTiles() %>%
  #     addPolygons(
  #       layerId = ~REF_AREA,
  #       fillColor = "lightgrey",
  #       fillOpacity = 0.7,
  #       color = "white",
  #       weight = 0.5,
  #       label = ~sovereignt
  #     )
  # })
  output$map <- renderLeaflet({
    leaflet(geo_sf) %>%
      addTiles() %>%
      addPolygons(
        layerId = ~REF_AREA,
        fillColor = "lightgrey",
        fillOpacity = 0.7,
        color = "white",
        weight = 0.5,
        label = ~sovereignt,
        labelOptions = labelOptions(
          direction = "auto",
          sticky = TRUE,    # <--- fait que le label reste affiché
          textsize = "12px"
        )
      )
  })
  
  ###############################################################
  ###### Mise à jour de la carte ##############################
  
  observeEvent(
    list(input$viz_mode, input$ref_country_S, input$var_group, input$score_var),
    {
      req(input$viz_mode)
      
      if (input$viz_mode == "Visualiser les similarités entre pays") {
        req(input$ref_country_S, input$var_group)
        vars_for_sim <- dict[var_group == input$var_group, var_label_fr]
        
        sims <-render_map_similarity(geo_sf, dt, input$ref_country_S, vars_for_sim)
        Top_Pays <- sims[order(-sims$sim), ]           # tri par similarité décroissante
        Top_Pays <- merge(Top_Pays, geo_sf[, c("REF_AREA", "sovereignt")], 
                          by = "REF_AREA", all.x = TRUE)
        Top_Pays <- Top_Pays[, c("sovereignt", "sim")]
        render_data_table(output,"T_top_pays",Top_Pays,titre = paste("Scores de similarité avec ", input$ref_country_S))
        

      }
      
      else if (input$viz_mode == "Visualiser les scores de performances de santé des pays") {
        req(input$score_var)
        render_map_scores(geo_sf, scores_global, var = input$score_var)
        
        Top_Pays <- scores_global[order(-get(input$score_var))][, .(REF_AREA,Score = get(input$score_var))]
        Top_Pays <- merge(Top_Pays, geo_sf[, c("REF_AREA", "sovereignt")], 
                          by = "REF_AREA", all.x = TRUE)
        Top_Pays <- Top_Pays[, c("sovereignt", "Score")]
        render_data_table(output,"T_top_pays",Top_Pays,titre = paste("Scores des pays par rapport à :", input$score_var))
      }
    }
  )
  

  
  observeEvent(input$help_score, {
    showModal(modalDialog(
      title = "Détails du calcul des scores",
      size = "l", 
      easyClose = TRUE,
      withMathJax(
      HTML('
      <p>Normalisation des variables :</p>
      $$\\tilde{x}_{ij} =
      \\begin{cases}
      \\frac{x_{ij} - \\min(\\mathbf{x}_j)}{\\max(\\mathbf{x}_j) - \\min(\\mathbf{x}_j)} & \\text{si $x_j$ est à maximiser}, \\\\
      \\frac{\\max(\\mathbf{x}_j) - x_{ij}}{\\max(\\mathbf{x}_j) - \\min(\\mathbf{x}_j)} & \\text{si $x_j$ est à minimiser.}
      \\end{cases}$$
      
      <p>Score par groupe :</p>
      $$S_i(g) = \\frac{1}{p_g} \\sum_{j \\in g} \\tilde{x}_{ij}$$
      
      <p>Score global :</p>
      $$S_{i}^{\\text{global}} = \\frac{1}{G} \\sum_{g=1}^G S_i(g)$$
      ')),
      footer = modalButton("Fermer")
    ))
  })
  
  observeEvent(input$help_similarity, {
    showModal(modalDialog(
      title = "Calcul du score de similarité",
      withMathJax(
        helpText(
          "La similarité entre le pays de référence et le pays i est définie par la distance euclidienne inversée :",
          "$$ S_i = \\frac{1}{1 + \\sqrt{\\sum_{j \\in V} (x_{ref,j} - x_{i,j})^2}} $$",
          "où :",
          "$$V$$ est l'ensemble des variables sélectionnées pour la comparaison,",
          "$$x_{ref,j}$$ est la valeur de la variable $$j$$ pour le pays de référence,",
          "$$x_{i,j}$$ est la valeur de la variable $$j$$ pour le pays $$i$$.",
          "Cette formule garantit que $$S_i \\in (0,1]$$, avec $$S_i = 1$$ si le pays i est identique au pays de référence pour toutes les variables."
        )
      ),
      easyClose = TRUE,
      footer = modalButton("Fermer")
    ))
  })
  
  
  
  
  
  ############## Barplot des contributions sur clic ##########################
  observeEvent(input$map_shape_click, {
    clicked <- input$map_shape_click$id
    req(clicked, input$ref_country_S, input$var_group)
    
    ref_country_S <- geo_sf %>%
      filter(sovereignt == input$ref_country_S) %>%
      pull(REF_AREA)
    
    vars_for_sim <- dict[var_group == input$var_group, var_label_fr]
    
    contrib_dt <- calc_contrib(dt, ref_country_S, clicked, vars_for_sim)
    
    output$barplot_contrib <- renderPlot({
      ggplot(contrib_dt, aes(x = reorder(variable, difference), y = difference)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        coord_flip() +
        labs(
          title = paste0("Différence par variable : ", clicked, " - ", ref_country_S),
          subtitle = "Barres positives : valeurs plus fortes que le pays de référence\nBarres négatives : valeurs plus faibles que le pays de référence",
          x = "Variables",
          y = "Différence (dans les unités d'origine)"
        ) +
        theme_minimal(base_size = 13) +
        theme(
          plot.title = element_text(face = "bold"),
          plot.subtitle = element_text(size = 11, color = "gray40")
        )
    })
  })
  
  
  
  
  
  ##########################
  # ---- Super plot ----
  ##########################
  observeEvent(
    list(input$ref_country_S, input$map_shape_click, input$var_group, input$choix_affichage),
    {
      clicked <-input$map_shape_click$id
      req(input$ref_country_S,clicked, input$var_group)
      
      # Pays de comparaison au clic (ISO3)
      ref_country <- input$ref_country_S
      
      
      clicked <- geo_sf %>%
        filter(REF_AREA == clicked ) %>%
        pull(sovereignt)
      
      vars_for_comp <- dict[var_group == input$var_group, var_label_fr]

      render_super_plot(
        output,
        output_id = "super_plot",
        dt = dt,
        dict = dict,
        scores_global = scores_global,
        geo_sf = geo_sf,
        ref_country = ref_country,
        comp_country = clicked,
        vars_groupe = vars_for_comp,
        user_choice = input$choix_affichage
      )
    }
  )
  
  
  #############################################################################
  ##############################################################################
  
  
  
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
      saveRDS(df, file = paste0("../Data/", input$file_name, ".rds"))
      
      
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
      readRDS(file.path("../Data/", input$dataset_preloaded))
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
  
  output$year_map_ui <- renderUI({
    df <- data_filtered()
    cols <- columns()
    req(df, cols)
    
    # Plus de test : cols$time est garanti d'exister
    annees <- sort(unique(as.numeric(df[[cols$time]])))
    annees <- annees[!is.na(annees)]
    
    if (length(annees) == 0) {
      return(p("Aucune année disponible", style = "color: red;"))
    }
    
    sliderInput("annee_map", "Sélectionner une année :",
                min = min(annees),
                max = max(annees),
                value = max(annees),
                step = 1,
                sep = "")
  })
  
  
  output$year_na_ui <- renderUI({
    df <- data_filtered()
    cols <- columns()
    req(df, cols)
    annees <- sort(unique(as.numeric(df[[cols$time]])))
    annees <- annees[!is.na(annees)]
    
    if (length(annees) == 0) {
      return(p("Aucune année disponible", style = "color: red;"))
    }
    
    sliderInput("annee_na", "Sélectionner une année :",
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
        return(paste0(unit))
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
    req(input$annee_map)
    paste("Indicateur :", input$indicateur, "en", input$annee_map)
  })
  
  output$titre_graph <- renderText({
    cols <- columns()
    paste("Indicateur :", input$indicateur)
  })
  
  donnees_carte <- reactive({
    df <- data_filtered()
    cols <- columns()
    req(df, cols, input$annee_map)
    
    df_annee <- df %>%
      filter(.data[[cols$time]] == as.character(input$annee_map)) %>%
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
      geom_line(linewidth = 1.5) +
      geom_point(size = 3.5, shape = 21, stroke = 1, fill = "white") +
      labs(
        x = "Année",
        y = paste0(get_unit()),
        color = "Pays",
        title = "Dynamique temporelle par pays"
      ) +
      scale_color_brewer(palette = "Set2") + 
      theme_minimal(base_size = 16) +
      theme(
        
        axis.title.x = element_text(face = "bold", size = 18, margin = margin(t = 15)),
        axis.title.y = element_text(face = "bold", size = 18, margin = margin(r = 15)),
        axis.text = element_text(size = 14),
        
        
        plot.title = element_text(face = "bold", hjust = 0.5, size = 22, margin = margin(b = 20)),
        
        legend.position = "top",
        legend.title = element_text(face = "bold", size = 16),
        legend.text = element_text(size = 14),
        legend.key.width = unit(2, "cm"),
        legend.key = element_rect(fill = "transparent"),
        legend.margin = margin(b = 15),
        
        # Grilles et axes
        axis.line = element_line(color = "grey40", linewidth = 0.8),
        panel.grid.minor = element_blank(),
        panel.grid.major = element_line(color = "grey85", linewidth = 0.5),
        
        # Marges générales
        plot.margin = margin(20, 20, 20, 20)
      )
    
  })
  
  output$n_lignes <- renderText({
    nrow(data())
  })
  
  output$n_colonnes <- renderText({
    ncol(data())
  })
  
  output$n_pays <- renderText({
    df <- data()
    cols <- columns()
    req(df, cols)
    length(unique(df[[cols$country]]))
  })
  
  output$n_annees <- renderText({
    df <- data()
    cols <- columns()
    req(df, cols)
    length(unique(df[[cols$time]]))
  })
  
  output$n_indicateurs <- renderText({
    df <- data()
    cols <- columns()
    req(df, cols)
    length(unique(df[[cols$indicators]]))
  })
  
  output$plage_temporelle <- renderText({
    df <- data()
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
    head(data(), 10)
  })
  
  # Préparer les données wide
  data_wide <- reactive({
    req(data(), columns(),input$annee_na)
    prepare_wide_data(data(), columns(), input$annee_na)
  })
  
  # Graphique principal
  output$missing_plot <- renderPlot({
    req(data_wide())
    aggr(data_wide(), only.miss = TRUE, sortVar = TRUE)
  })
  
  output$missing_summary <- renderPrint({
    req(data_wide())
    invisible_res <- invisible(summary(aggr(data_wide(), prop = TRUE, combined = TRUE)))
    res <- invisible_res$combinations
    res[rev(order(res[,1])),]
  })
  
  
  ###########################
  # Partie de Tom
  ###########################
  
  # reactive year
  year_sel <- reactive({
    as.numeric(req(input$year))
  })
  
  #### 1) Graph 2D Esperance de vie X Air Quality X PIB_Sante (plotly)
  output$plot_air_health <- renderPlotly({
    yr <- year_sel()
    # prepare data (copié / adapté)
    pib_sante_clean <- dta_pib_sante %>%
      group_by(Reference.area, TIME_PERIOD) %>%       
      slice_max(order_by = OBS_VALUE, n = 1, with_ties = FALSE) %>%  
      ungroup()
    pib_sante_clean <- as.data.table(pib_sante_clean)
    
    air <- extract_indicator(dta_air_quality, "Population share exposed", yr)
    health <- extract_indicator(dta_health, "Life expectancy", yr)
    pib_sante <- extract_indicator(pib_sante_clean, "Expenditure", yr)
    
    data_merged <- Reduce(function(x, y) merge(x, y, by = "Reference.area", all = TRUE),
                          list(air, pib_sante, health))
    
    setnames(data_merged, c("Reference.area", "AirQuality", "HealthGDP", "LifeExpectancy"))
    data_merged <- as.data.frame(data_merged)
    
    data_merged <- data_merged %>%
      filter(!is.na(AirQuality),
             !is.na(LifeExpectancy), !is.na(HealthGDP),
             Reference.area %in% input$country)
    
    sizes <- rescale(data_merged$HealthGDP, to = c(5, 20))
    
    p <- plot_ly(
      data = data_merged,
      x = ~AirQuality,
      y = ~LifeExpectancy,
      color = ~Reference.area,
      text = ~paste(Reference.area,
                    "<br>% de la population exposée aux particules fines:", round(AirQuality, 1),
                    "<br>Espérence de Vie:", round(LifeExpectancy, 1),
                    "<br>% PIB investit dans la Santé:", round(HealthGDP, 1)),
      hoverinfo = "text",
      type = "scatter",
      mode = "markers",
      marker = list(size = sizes)
    ) %>%
      layout(
        title = list(
          text = paste("Santé et qualité de l'air au sein de l'OCDE (", yr, ")", sep = ""),
          font = list(size = 20),
          x = 0.5,
          xanchor = "center",
          y = 0.95,
          yanchor = "top"
        ),
        xaxis = list(title = "% de la Population exposée à plus de 15µg/m³ de PM2.5", zeroline = FALSE),
        yaxis = list(title = "Espérance de vie (ans)")
      )
    p
  })
  
  #### 2) Matrice des corrélations des indicateurs (ggplot heatmap)
  output$plot_corr <- renderPlot({
    yr <- year_sel()
    # Préprocessing & préparation wide (utilise liste_tableaux)
    prepare_data_year <- function(tab, year) {
      tab %>%
        filter(TIME_PERIOD == year) %>%
        select(Reference.area, Measure, OBS_VALUE) %>%
        pivot_wider(names_from = Measure, values_from = OBS_VALUE)
    }
    
    data_wide <- liste_tableaux %>%
      map(~prepare_data_year(.x, yr)) %>%
      reduce(full_join, by = "Reference.area")
    
    data_wide <- data_wide %>%
      filter(rowSums(is.na(.)) < ncol(.)/2)
    
    data_corr <- data_wide %>% select(-Reference.area)
    corr_matrix <- cor(data_corr, use = "pairwise.complete.obs")
    corr_long <- reshape2::melt(corr_matrix)
    
    labels_vars <- c(
      "Hospital employment" = "Emploi secteur de la Santé",
      "LifeExpectancy" = "Espérance de vie",
      "Expenditure" = "% PIB investit dans la Santé",
      "Pharmaceutical consumption" = "Consommation de médicaments \n par habitant",
      "Gross domestic product" = "PIB",
      "Gross domestic product per capita" = "PIB par habitant",
      "Population share exposed" = "% de la population exposée \n à la pollution de l'air"
    )
    
    ggplot(corr_long, aes(Var1, Var2, fill = value)) +
      geom_tile(color = "white", linewidth = 0.5) + 
      geom_text(aes(label = round(value, 2)), size = 6, color = "black") +  
      scale_fill_gradient2(
        low = "#B2182B", mid = "white", high = "#2166AC", midpoint = 0,
        limits = c(-1, 1),
        name = "Corrélation"
      ) +
      scale_x_discrete(labels = labels_vars) +
      scale_y_discrete(labels = labels_vars) +  
      labs(
        title = paste("Heatmap des corrélations entre indicateurs des données OCDE (", yr, ")", sep = ""),
        x = NULL,
        y = NULL
      ) +
      theme_minimal(base_size = 14) +
      theme(
        plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(b = 15)),
        axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1, size = 14, face = "bold"),
        axis.text.y = element_text(size = 14, face = "bold"),
        panel.grid = element_blank(),
        legend.title = element_text(size = 11, face = "bold"),
        legend.text = element_text(size = 14)
      ) +
      coord_fixed()
  })
  
  #### 3) Scatter Plot Medicament X PIB Hab X Esperance de vie
  output$plot_med_pib <- renderPlot({
    yr <- year_sel()
    # data extraction adaptée
    pib_hab <- extract_indicator(dta_pib_hab, "Gross domestic product per capita", yr)
    medicaments <- extract_indicator(dta_medicament, "Pharmaceutical consumption", yr)
    health <- extract_indicator(dta_health, "Life expectancy", yr)
    rh_sante <- extract_indicator(dta_RH_sante %>%
                                    filter(Health.profession == "Total", Unit.of.measure == "Per 1 000 inhabitants") %>% select(-Health.profession),
                                  "Hospital employment",
                                  yr)
    pib <- extract_indicator(dta_pib, "Gross domestic product", yr)
    
    data_merged2 <- Reduce(function(x, y) merge(x, y, by = "Reference.area", all = TRUE),
                           list(pib, medicaments, health, rh_sante))
    
    setnames(data_merged2, c("Reference.area", "PIB", "Consommation de médicaments", "LifeExpectancy", "Ressources humaines sante"))
    data_merged2 <- as.data.frame(data_merged2)
    
    data_model <- data_merged2 %>%
      filter(!is.na(`Consommation de médicaments`), !is.na(PIB),
             Reference.area %in% input$country)
    
    if(nrow(data_model) < 3) {
      plot.new()
      title(main = paste("Pas assez de données pour l'année", yr))
      return()
    }
    
    mod <- lm(`Consommation de médicaments` ~ PIB, data = data_model)
    data_model <- augment(mod, data = data_model)
    data_model <- data_model %>%
      mutate(atypique = abs(.resid) > sd(.resid, na.rm = TRUE) * 1.5)
    pred <- data.frame(
      PIB = seq(min(data_model$PIB, na.rm = TRUE), max(data_model$PIB, na.rm = TRUE), length.out = 100)
    )
    pred$y <- predict(mod, newdata = pred)
    
    data_labels <- data_model %>%
      mutate(
        fontface = ifelse(atypique, "bold", "plain"),
        segment_color = ifelse(atypique, "black", "grey50"),
        segment_size = ifelse(atypique, 0.7, 0.5)
      )
    
    ggplot(data_model, aes(
      x = PIB,
      y = `Consommation de médicaments`,
      color = LifeExpectancy,
      size = `Ressources humaines sante`
    )) +
      geom_point(alpha = 0.8) +
      geom_point(
        data = subset(data_model, atypique),
        shape = 21,
        color = "black",
        stroke = 1.2,
        fill = NA,
        size = subset(data_model, atypique)$`Ressources humaines sante`,
        show.legend = FALSE
      ) +
      geom_text_repel(
        data = data_labels,
        aes(
          label = Reference.area,
          fontface = fontface,
          segment.color = segment_color,
          segment.size = segment_size
        ),
        size = 7,
        max.overlaps = 50,
        min.segment.length = 0,
        segment.alpha = 0.8,
        point.padding = 0.8,
        box.padding = 1
      ) +
      scale_size_continuous(
        range = c(3, 12),
        name = "Personnel de santé pour 1000 hab",
        guide = "none"
      ) +
      scale_color_viridis_c(option = "plasma", name = "Espérance de vie") +
      scale_x_log10(labels = scales::label_number(suffix = " Md$", accuracy = 1)) +
      scale_y_continuous(labels = scales::label_number(big.mark = " ")) +
      geom_smooth(method = "lm", se = FALSE, color = "gray40", linetype = "dashed") +
      theme_minimal(base_size = 14) +
      labs(
        title = paste("Consommation de médicaments vs PIB (pays OCDE) -", yr),
        subtitle = "Les pays atypiques sont entourés et leurs noms sont en gras.\nLa taille des cercles est proportionnelle avec le nombre d'emploi dans la santé pour 1000 hab",
        x = "PIB (log10, en milliards de $US)",
        y = "Médicaments consommés par habitant (Moyenne annuelle)"
      )
  })
  
  #### 4) ACP (PCA) - deux graphiques
  output$pca_indiv <- renderPlot({
    yr <- year_sel()
    
    RH_sante_clean <- dta_RH_sante %>%
      filter(Health.profession == "Total",
             Unit.of.measure == "Per 1 000 inhabitants") %>%
      select(-Health.profession)
    
    air <- extract_indicator(dta_air_quality, "Population share exposed", yr)
    health <- extract_indicator(dta_health, "Life expectancy", yr)
    pib_sante <- extract_indicator(dta_pib_sante %>%
                                     group_by(Reference.area, TIME_PERIOD) %>%
                                     slice_max(order_by = OBS_VALUE, n = 1, with_ties = FALSE) %>% ungroup() %>% as.data.table(),
                                   "Expenditure", yr)
    pib_hab <- extract_indicator(dta_pib_hab, "Gross domestic product per capita", yr)
    medicaments <- extract_indicator(dta_medicament, "Pharmaceutical consumption", yr)
    rh_sante <- extract_indicator(RH_sante_clean, "Hospital employment", yr)
    pib <- extract_indicator(dta_pib, "Gross domestic product", yr)
    
    # rename OBS_VALUE
    air <- rename(air, AirQuality = OBS_VALUE)
    health <- rename(health, LifeExpectancy = OBS_VALUE)
    pib_sante <- rename(pib_sante, HealthGDP = OBS_VALUE)
    pib_hab <- rename(pib_hab, GDP_per_capita = OBS_VALUE)
    medicaments <- rename(medicaments, PharmaceuticalConsumption = OBS_VALUE)
    rh_sante <- rename(rh_sante, HealthStaff = OBS_VALUE)
    pib <- rename(pib, GDP = OBS_VALUE)
    
    data_merged <- Reduce(function(x, y) merge(x, y, by = "Reference.area", all = TRUE),
                          list(air, pib_sante, health, pib_hab, medicaments, rh_sante, pib))
    data_merged <- as.data.frame(data_merged)
    
    data_merged <- data_merged %>%
      filter(!is.na(AirQuality),
             !is.na(LifeExpectancy),
             !is.na(HealthGDP),
             !is.na(GDP_per_capita),
             !is.na(PharmaceuticalConsumption),
             !is.na(HealthStaff),
             !is.na(GDP))
    
    if(nrow(data_merged) < 3) {
      plot.new()
      title(main = paste("Pas assez de données pour l'ACP en", yr))
      return()
    }
    
    rownames(data_merged) <- data_merged$Reference.area
    data_merged <- data_merged %>% select(-Reference.area)
    
    res.pca <- PCA(data_merged, scale.unit = TRUE, graph = FALSE)
    
    # Graphique des individus (biplot sans variables)
    fviz_pca_biplot(res.pca,
                    pointsize = 6,
                    repel = TRUE,
                    col.ind = "cos2",
                    gradient.cols = viridis(100, option = "plasma"),
                    labelsize = 7,
                    axes = c(1, 2),
                    invisible = "var",
                    title = paste("PCA des pays de l'OCDE - Qualité de représentation (", yr, ")", sep = ""))
  })
  
  output$pca_var <- renderPlot({
    yr <- year_sel()
    
    RH_sante_clean <- dta_RH_sante %>%
      filter(Health.profession == "Total",
             Unit.of.measure == "Per 1 000 inhabitants") %>%
      select(-Health.profession)
    
    air <- extract_indicator(dta_air_quality, "Population share exposed", yr)
    health <- extract_indicator(dta_health, "Life expectancy", yr)
    pib_sante <- extract_indicator(dta_pib_sante %>%
                                     group_by(Reference.area, TIME_PERIOD) %>%
                                     slice_max(order_by = OBS_VALUE, n = 1, with_ties = FALSE) %>% ungroup() %>% as.data.table(),
                                   "Expenditure", yr)
    pib_hab <- extract_indicator(dta_pib_hab, "Gross domestic product per capita", yr)
    medicaments <- extract_indicator(dta_medicament, "Pharmaceutical consumption", yr)
    rh_sante <- extract_indicator(RH_sante_clean, "Hospital employment", yr)
    pib <- extract_indicator(dta_pib, "Gross domestic product", yr)
    
    air <- rename(air, AirQuality = OBS_VALUE)
    health <- rename(health, LifeExpectancy = OBS_VALUE)
    pib_sante <- rename(pib_sante, HealthGDP = OBS_VALUE)
    pib_hab <- rename(pib_hab, GDP_per_capita = OBS_VALUE)
    medicaments <- rename(medicaments, PharmaceuticalConsumption = OBS_VALUE)
    rh_sante <- rename(rh_sante, HealthStaff = OBS_VALUE)
    pib <- rename(pib, GDP = OBS_VALUE)
    
    data_merged <- Reduce(function(x, y) merge(x, y, by = "Reference.area", all = TRUE),
                          list(air, pib_sante, health, pib_hab, medicaments, rh_sante, pib))
    data_merged <- as.data.frame(data_merged)
    
    data_merged <- data_merged %>%
      filter(!is.na(AirQuality),
             !is.na(LifeExpectancy),
             !is.na(HealthGDP),
             !is.na(GDP_per_capita),
             !is.na(PharmaceuticalConsumption),
             !is.na(HealthStaff),
             !is.na(GDP))
    
    if(nrow(data_merged) < 3) {
      plot.new()
      title(main = paste("Pas assez de données pour l'ACP en", yr))
      return()
    }
    
    rownames(data_merged) <- data_merged$Reference.area
    data_merged <- data_merged %>% select(-Reference.area)
    
    res.pca <- PCA(data_merged, scale.unit = TRUE, graph = FALSE)
    
    # Graphique des variables
    fviz_pca_var(
      res.pca,
      col.var = "cos2",
      gradient.cols = viridis(100, option = "plasma"),
      repel = TRUE,
      labelsize = 6.5       # <--- augmente la taille du texte des variables
    ) +
      ggtitle(paste("ACP - variables (", yr, ")", sep = "")) +
      theme(
        plot.title = element_text(size = 18, face = "bold"),  # titre
        axis.title = element_text(size = 14),                 # titres des axes
        axis.text = element_text(size = 12)                   # texte des axes
      )
  })
}
    

    
    
    

