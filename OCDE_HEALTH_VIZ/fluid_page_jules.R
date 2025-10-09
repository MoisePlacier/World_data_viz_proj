

fluid_page_accueil <- fluidPage(
  br(),
  titlePanel("Explorateur de données de l'OCDE"),
  br(),
  theme = bs_theme(
    bootswatch = "minty",
    base_font = font_google("Inter"),
    navbar_bg = "#25443B"
  ),
  sidebarLayout( # séparé en deux : le sidebar panel avec les selecteurs et le main panel avec la carte et les graphs
    sidebarPanel(
      width = 3,
      card(
        card_header("Données et indicateurs"),
        card_body(
          # Pré chargés ou issus d'URL : --> on traite ça avec les conditionnal pannel
          radioButtons("mode", "Source des données:",
                       choices = c("Datasets pré-chargés" = "preloaded",
                                   "URL personnalisée" = "custom")),
          
          # Si dataset pré-chargé : menu déroulant
          conditionalPanel(
            condition = "input.mode == 'preloaded'",  # voir doc conditional pannel
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
            actionButton("load_url", "Charger", class = "btn-success"), # comme dans la doc
          ),
          
          hr(),
          
          # Sélecteur d'indicateur dynamique
          uiOutput("indicator_ui"),
          
          hr(),
          helpText("Source: API SDMX OCDE")))
    ),
    
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Carte",
                 # Curseur dynamique pour l'année
                 card(
                   card_header(textOutput("titre_carte")),
                   card_body(uiOutput("year_map_ui")),
                   
                   leafletOutput("carte", height = 450),
                   br()
                 )),
        
        tabPanel(
          "Évolution temporelle",
          br(),
          fluidRow(
            column(
              width = 10,
              card(
                card_header(textOutput("titre_graph")),
                plotOutput("graphique_evolution", height = 450)
              )
            ),
            column(
              width = 2,
              hr(),
              uiOutput("country_selector")
            )
          )
        ),
        
        tabPanel("Aperçu",
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
                   h4("Jeu de données brutes"),
                   card(
                     full_screen = TRUE,
                     card_header("10 premières lignes :"),
                     card_body(
                       tableOutput("data_preview")
                     )
                   ))
        ),
        tabPanel("Données manquantes",
                 br(),
                 uiOutput("year_na_ui"),
                 card(
                   full_screen = TRUE,
                   card_header("Structure des données manquantes : fréquence et combinaisons"),
                   card_body(
                     plotOutput("missing_plot", height = "600px"))),
                 br(),
                 card(
                   full_screen = TRUE,
                   card_header("Résumé"),
                   card_body(verbatimTextOutput("missing_summary")))
        )
        
        
      )
    )
  )
)