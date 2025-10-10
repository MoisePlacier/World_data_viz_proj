




### === UI === ###
FR_Condi <- fluidRow(
  # Colonne gauche : sélection
  column(
    width = 4,
    style = "background-color:#f9f9f9; padding:15px; border-radius:10px; height:100%;",
    
    h4("Exploration des données agrégées sur la période 2015–2020, à partir des moyennes observées."),
    
    
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
      fluidRow(
        # 2/3 pour le menu déroulant
        column(
          width = 8,
          style = "display: flex; align-items: center;",
          selectInput(
            "var_group",
            "Groupe de variables :",
            choices = unique(dict$var_group[-1]),
            selected = "Dépenses en % du total des dépenses de santé"
          )
        ),
        
        # 1/3 pour le bouton d'aide
        column(
          width = 4,
          style = "display: flex; align-items: center;",
          actionButton(
            "help_similarity",
            label = "",
            icon = icon("question-circle")
          )
        )
      )
    ),
    ############## 2 
    conditionalPanel(
      condition = "input.viz_mode == 'Visualiser les scores de performances de santé des pays'",
      
      fluidRow(
        # Colonne 2/3 pour le selectInput
        column(
          width = 8,  # 8/12 ≈ 2/3
          #style = "display: flex; align-items: center;",
          selectInput(
            "score_var",
            "Score à visualiser :",
            choices = c(
              "Score global" = "score_global",
              "Mortalité évitable" = "Mortalité évitable pour 100 000 habitants",
              "Ressources humaines de santé" = "Emploi en santé pour 1000 habitants",
              "Qualité des soins" = "Hospitalisations/mortalité évitables pour 1000 patients",
              "Espérance de vie" = "esperance de vie"
            ),
            selected = "score_global"
          )
        ),
        
        # Colonne 1/3 pour le bouton
        column(
          width = 4,  # 4/12 ≈ 1/3
          style = "display: flex; align-items: center;",
          actionButton("help_score", label = "", icon = icon("question-circle"))
        )
      ),
      
      h5("choix des variables à comparer"),
      
      selectInput(
        "var_group",
        "Groupe de variables :",
        choices = unique(dict$var_group[-1]),
        selected = "Dépenses en % du total des dépenses de santé"
      )
    ),
    # Choix du pays de référence
    selectInput(
      "ref_country_S",
      "Pays de référence :",
      choices = sort(unique(geo_sf$sovereignt)),
      selected = "France"
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
  leafletOutput("map", height = "600px", width = "100%")
)

top_10_Table <- conditionalPanel(
  condition = "input.viz_mode != ' '",      
  div( style = "height:400px; overflow-y:auto; border:1px solid #ddd; padding:10px; border-radius:10px;",
       uiOutput("T_top_pays"))  
)

FR_Map <- fluidRow(
  column(3,top_10_Table),
  column(9,Condi_MAP)
  
  )
######################################################################

FR_C_Sim_barplot <-fluidRow(
  conditionalPanel(
    condition = "input.viz_mode == 'Visualiser les similarités entre pays'",
    column(12,h4("Écarts par variable (pays cliqué vs pays de référence)"),plotOutput("barplot_contrib", height = 400))))


Check <- checkboxGroupInput(
  "choix_affichage",
  label = "Afficher :",
  choices = c(
    "Moyenne globale" = "moyenne_globale",
    "Valeur minimale" = "valeur_min",
    "Valeur maximale" = "valeur_max"
  ),
  selected = c("moyenne_globale")
)

beau_plot <- plotlyOutput("super_plot", height = "800px")

FR_C_Comparaison_barplot <-fluidRow(
  column(12,Check,beau_plot)
  
)









