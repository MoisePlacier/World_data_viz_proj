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
library(DT)


# Concevoir un plot de comparaison d un pay références pour un groupe de variable donné
# 
# modalités de comparaisons : 
#   1) VS la moyenne pour tous les pays de ces variables 
#   2) VS les valeurs d un autre pays choisi
#   3) vs les valeurs des variables pour le pays qui a le meilleur score global 
#   4) vs les valeurs des variables pour le pays qui a le meilleur score selon un groupe de variable évalué 
# 
# Mapper le noms des pays avec l ISO ...

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
geo_sf <- geo_sf[geo_sf$gu_a3 %in% filtre, ]
# Ajouter une colonne REF_AREA 
geo_sf$REF_AREA <- geo_sf$gu_a3
########################################

########## scores 
scores_global <- readRDS("../Data/scores_globaux.rds")
scores_global <- as.data.table(scores_global)

####### groupes de variables pour les scores 
vars_groupes_scores <- readRDS("../Data/vars_groupes_scores.rds")
vars_groupes_scores <- as.data.table(vars_groupes_scores)



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
        vars_for_scores <- unique(vars_groupes_scores[[input$score_var]])
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
  output$map <- renderLeaflet({
    leaflet(geo_sf) %>%
      addTiles() %>%
      addPolygons(
        layerId = ~REF_AREA,
        fillColor = "lightgrey",
        fillOpacity = 0.7,
        color = "white",
        weight = 0.5,
        label = ~sovereignt
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
}
    

    
    
    

