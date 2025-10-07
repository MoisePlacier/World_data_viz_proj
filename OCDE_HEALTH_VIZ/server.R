#
library(here)

## Notes à faire demain 

# Régler le prblm dimport des données dans lenvironement global ... 
# 
# Homogénéiser les grps de variables ! (séparer les % des nbr absolus etcs)
# Recalculer les scores avec ces nouvaux groups
# 
# sur la carte interactive : enlever le pays de référence pour qualibrer l échelle de couleurs (le mettre d une autre couleur)
# 
# Ajouter la possibilité d'afficher les variables directement ou d'afficher les scores. 
#
#

# Concevoir un plot de comparaison d un pay références pour un groupe de variable donné
# 
# modalités de comparaisons : 
#   1) VS la moyenne pour tous les pays de ces variables 
#   2) VS les valeurs d un autre pays choisi
#   3) vs les valeurs des variables pour le pays qui a le meilleur score global 
#   4) vs les valeurs des variables pour le pays qui a le meilleur score selon un groupe de variable évalué 
# 
# Mapper le noms des pays avec l ISO ...

####
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


library(ggplot2)
library(RColorBrewer)
library(shiny)
library(leaflet)
library(data.table)
library(dplyr)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)

# Charger le fond de carte
geo_sf <- ne_countries(scale = "medium", returnclass = "sf")

#### data
load("../Data/merged_inputed.RData")
dt <- as.data.table(dt_excl)
dt[, REF_AREA := as.factor(REF_AREA)]
#dt[, dep_sante_Totale_Depenses_sante := NULL]

#### dict 

load("../Data/Labels_dict_wide.RData")  # suppose que l'objet dedans s'appelle dict
# maintenant dict existe dans l'environnement
dict <- as.data.table(Labels_dict_wide)

colnames(dict)
setnames(dt, old = dict$var_full, new = dict$var_label_fr)


#### scores 

load("../Data/scores_global.RData")  # suppose que l'objet dedans s'appelle dict
# maintenant dict existe dans l'environnement
scores_global <- as.data.table(scores)

####### groupes de variables pour les scores 

load("../data/vars_groupes_scores.RData")

vars_groupes_scores <-as.data.table(vars_groupes)

# Filtrer le fond de carte
filtre <- dt$REF_AREA
geo_sf <- geo_sf[geo_sf$gu_a3 %in% filtre, ]

# Ajouter une colonne REF_AREA 
geo_sf$REF_AREA <- geo_sf$gu_a3





function(input, output, session){
  
  # Mettre à jour la sélection des variables selon le groupe choisi
  output$vars_ui <- renderUI({
    vars_in_group <- dict[var_group == input$var_group, var_label_fr]
    selectInput("vars", "Variables :", choices = vars_in_group,
                selected = vars_in_group, multiple = TRUE)
  })
  
  ############## Carte initiale ###################@
  output$map <- renderLeaflet({
    leaflet(geo_sf) %>%
      addTiles() %>%
      addPolygons(layerId = ~REF_AREA,
                  fillColor = "lightgray",
                  fillOpacity = 0.7,
                  color = "white",
                  weight = 0.5,
                  label = ~REF_AREA)
  })
  
  ###### Mise à jour de la carte selon pays de référence et variables choisies##############
  observeEvent(c(input$ref_country_S, input$vars), {
    req(input$ref_country_S, input$vars)
    sims <- calc_similarity(dt, input$ref_country_S, input$vars)
    
    map_data <- merge(geo_sf, sims, by="REF_AREA", all.x=TRUE)
    

    #pal <- colorNumeric(palette = "RdBu",domain = map_data$sim,reverse = TRUE)
    pal <- colorNumeric("Blues", domain=map_data$sim, na.color="white")
    
    
    leafletProxy("map") %>%
      clearShapes() %>%
      clearControls() %>%
      addPolygons(data=map_data,
                  layerId = ~REF_AREA,
                  fillColor = ~pal(sim),
                  fillOpacity = 0.8,
                  color = "white",
                  weight = 0.5,
                  label = ~paste0(REF_AREA, ": ", round(sim,3)),
                  highlightOptions = highlightOptions(
                    weight = 1,
                    color = 'black',
                    bringToFront = TRUE)
                  ) %>%
      addLegend("bottomright", pal=pal, values=map_data$sim,
                title=paste("Similarité à", input$ref_country_S))
  })
  
  ############## Barplot des contributions sur clic ##########################
  observeEvent(input$map_shape_click, {
    clicked <- input$map_shape_click$id
    req(clicked, input$ref_country_S, input$vars)
    
    contrib_dt <- calc_contrib(dt, input$ref_country_S, clicked, input$vars)
    
    
    output$barplot_contrib <- renderPlot({
      ggplot(contrib_dt, aes(x = reorder(variable, difference), y = difference)) +
        geom_bar(stat = "identity", fill = "steelblue") +
        coord_flip() +
        labs(
          title = paste0(
            "Écarts moyens par variable entre ", clicked, " et ", input$ref_country_S
          ),
          subtitle = "Barres positives : valeurs plus élevées dans le pays sélectionné\nBarres négatives : valeurs plus faibles que le pays de référence",
          x = "Variables",
          y = "Différence (dans les unités d'origine)"
        ) +
        theme_minimal(base_size = 13) +
        theme(
          plot.title = element_text(face = "bold"),
          plot.subtitle = element_text(size = 11, color = "gray40")
        )
    })
    ###############################################################################
    ########## Graphique de comparaison des valeurs par variable################### unique(dict$var_group)
    
    output$comparison_plot <- renderPlot({
      req(input$ref_country_C, input$vars_G, input$compare_mode)
      vars_C <-unique(vars_groupes_scores$vars_G)
      dt_ref <- dt[REF_AREA == input$ref_country_C, ..vars_C]
      
      # Calcul des valeurs comparatives selon le mode choisi
      
      comp_values <- switch(input$compare_mode,
                            
                            # 1) Moyenne
                            "Moyenne" = colMeans(dt[, ..vars_C], na.rm = TRUE),
                            
                            # 2) Meilleur pays selon le score du groupe
                            "Meilleur pays" = {
                              # On suppose que chaque variable appartient à un groupe
                              # On prend le meilleur pays selon le score correspondant au groupe
                              group_name <- input$vars_G
                              best_country <- scores[which.max(get(group_name)), REF_AREA]
                              dt[REF_AREA == best_country, ..vars_C]
                            },
                            
                            # 3) Pays spécifique
                            "Pays spécifique" = dt[REF_AREA == input$specific_country, ..vars_C]
      )
      
      # Préparer un dataframe pour ggplot
      plot_df <- data.frame(
        variable = vars_C,
        ref_value = as.numeric(dt_ref),
        comp_value = as.numeric(comp_values)
      )

      # Tracer
      ggplot(plot_df, aes(x = reorder(variable, ref_value))) +
        geom_bar(aes(y = ref_value), stat = "identity", fill = "steelblue", alpha = 0.7) +
        geom_point(aes(y = comp_value), color = "red", size = 3) +
        coord_flip() +
        labs(
          title = paste("Comparaison des variables pour", input$ref_country_C),
          subtitle = paste("Points rouges :", input$compare_mode),
          x = "Variable",
          y = "Valeur"
        ) +
        theme_minimal(base_size = 13)
        

    })
    
  })
}

