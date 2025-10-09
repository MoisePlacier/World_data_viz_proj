fluid_tempo <- fluidPage(
  titlePanel("Visualisations OCDE - Santé, Air, PIB"),
  sidebarLayout(
    sidebarPanel(
      width = 3,
      helpText("Choisir l'année à afficher pour tous les graphiques :"),
      selectInput(
        inputId = "year",
        label = "Année",
        choices = years_all,
        selected = max(years_all)
      ),
      selectInput(
        inputId = "country",
        label = "Pays à afficher",
        choices = countries_all,    
        selected = countries_all,   # par défaut, tous les pays sont sélectionnés
        multiple = TRUE
      ),
      br(),
      helpText("Les graphiques sont affichés dans des onglets. Les tailles ont été augmentées pour lisibilité.")
    ),
    mainPanel(
      width = 9,
      tabsetPanel(
        tabPanel("Scatter Air vs Espérance (Plotly)", 
                 br(),
                 plotlyOutput("plot_air_health", height = "700px")
        ),
        tabPanel("Heatmap Corrélations",
                 br(),
                 plotOutput("plot_corr", height = "700px")
        ),
        tabPanel("Scatter Médicaments vs PIB",
                 br(),
                 plotOutput("plot_med_pib", height = "700px")
        ),
        tabPanel("ACP (PCA) - individus & variables",
                 br(),
                 fluidRow(
                   column(6, plotOutput("pca_indiv", height = "650px")),
                   column(6, plotOutput("pca_var", height = "650px"))
                 )
        )
      )
    )
  )
)