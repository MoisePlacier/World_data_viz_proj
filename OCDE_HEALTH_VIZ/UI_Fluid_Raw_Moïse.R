




### === UI === ###
FR_Condi <- fluidRow(
  # Colonne gauche : sélection
  column(
    width = 4,
    style = "background-color:#f9f9f9; padding:15px; border-radius:10px; height:100%;",
    
    h4("🔧 Paramètres de visualisation"),
    
    
    # Choix du mode de visualisation
    selectInput(
      "viz_mode",
      "Type d’analyse :",
      choices = c("Visualiser les similarités entre pays","Visualiser les scores de performances de santé des pays"," "),
      selected = " "
    ),
    #######################################################
    # Panneau conditionnel  menu du groupe de variables
    conditionalPanel(
      condition = "input.viz_mode == 'Visualiser les similarités entre pays'",
      selectInput(
        "var_group",
        "Groupe de variables :",
        choices = unique(dict$var_group[-1]),
        selected = "Dépenses en % du total des dépenses de santé"
      ),
      # Choix du pays de référence
      selectInput(
        "ref_country_S",
        "Pays de référence :",
        choices = sort(unique(geo_sf$sovereignt)),
        selected = "France"
      )
    ),
    ############## 2 
    conditionalPanel(
        condition = "input.viz_mode == 'Visualiser les scores de performances de santé des pays'",
        selectInput(
          "score_var",
          "Score à visualiser :",
          choices = c(
            "Score global" = "score_global",
            "Mortalité évitable" = "mortalite_evitable",
            "Ressources humaines de santé" = "ressources_humaines_sante",
            "Qualité des soins" = "qualite_soins",
            "Espérance de vie" = "esperance_de_vie"),
          selected = "score_global"),
        h5("Carte interactive des scores de performance")
        )
  ),
  
  # Colonne droite : tableau défilant
  column(
    width = 8,
    div(
      style = "height:400px; overflow-y:auto; border:1px solid #ddd; padding:10px; border-radius:10px;",
      uiOutput("my_table")
    )
  )
)


######################################################################
Condi_MAP <- conditionalPanel(
  condition = "true",  # toujours visible
  h4("Carte interactive"),
  leafletOutput("map", height = 400)
)

top_10_Table <- conditionalPanel(
  condition = "input.viz_mode != ' '",      
  div( style = "height:400px; overflow-y:auto; border:1px solid #ddd; padding:10px; border-radius:10px;",
       uiOutput("T_top_pays"))  
)

FR_Map <- fluidRow(
  column(4,top_10_Table),
  column(8,Condi_MAP)
  
  )
######################################################################

FR_C_Sim_barplot <-fluidRow(
  conditionalPanel(
    condition = "input.viz_mode == 'Visualiser les similarités entre pays'",
    column(12,h4("Écarts par variable (pays cliqué vs pays de référence)"),plotOutput("barplot_contrib", height = 400))))












