#
library(ggplot2)
library(RColorBrewer)
library(shiny)
library(leaflet)
library(data.table)
library(dplyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(dplyr)
source("Utilis.R")
source("Plots.R")
source("server_jules.R")
source("fonctions_jules.R")
library(DT)
library(shinyBS) 
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




library(shiny)
library(ggplot2)
library(dplyr)
library(leaflet)
library(rnaturalearth)
library(sf)
library(countrycode)
library(rsdmx)
library(bslib)

library(tidyverse)
library(VIM)

library(bsicons)


############## Datas Jules 
world <- ne_countries(scale = "medium", returnclass = "sf")

preloaded_datasets <- list.files("../Data/", pattern = "\\.rds$", full.names = FALSE)





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
        render_data_table(output,"T_top_pays",Top_Pays,titre = paste("Classement des pays les plus similaires à ", input$ref_country_S))
        

      }
      
      else if (input$viz_mode == "Visualiser les scores de performances de santé des pays") {
        req(input$score_var)
        render_map_scores(geo_sf, scores_global, var = input$score_var)
        
        Top_Pays <- scores_global[order(-get(input$score_var))][, .(REF_AREA,Score = get(input$score_var))]
        Top_Pays <- merge(Top_Pays, geo_sf[, c("REF_AREA", "sovereignt")], 
                          by = "REF_AREA", all.x = TRUE)
        Top_Pays <- Top_Pays[, c("sovereignt", "Score")]
        render_data_table(output,"T_top_pays",Top_Pays,titre = paste("Classement des pays pour le score :", input$score_var))
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
    

    
    
    

