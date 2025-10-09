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

####----
### Data importation (global)
dta_pib_sante <- readRDS("./Data/dta_pib_sante.rds")
dta_air_quality <- readRDS("./Data/dta_air_quality.rds")
dta_agri <- readRDS("./Data/dta_agri.rds")
dta_health <- readRDS("./Data/dta_health.rds")
dta_pib_hab <- readRDS("./Data/dta_pib_hab.rds")
dta_pib <- readRDS("./Data/dta_pib.rds")
dta_medicament <- readRDS("./Data/dta_medicament.rds")
dta_RH_sante <- readRDS("./Data/dta_RH_sante.rds")

# helper: liste de tables utilisées pour les heatmaps / corrélations
liste_tableaux <- list(
  dta_pib_sante %>% group_by(Reference.area, TIME_PERIOD) %>%
    slice_max(order_by = OBS_VALUE, n = 1, with_ties = FALSE) %>% ungroup(),
  dta_air_quality, dta_health, dta_pib_hab, dta_pib, dta_medicament,
  dta_RH_sante %>% filter(Health.profession == "Total", Unit.of.measure == "Per 1 000 inhabitants") %>% select(-Health.profession)
)

# calculer années disponibles dans les jeux de données (pour le slider)
years_all <- sort(unique(na.omit(c(
  dta_pib_sante$TIME_PERIOD,
  dta_air_quality$TIME_PERIOD,
  dta_agri$TIME_PERIOD,
  dta_health$TIME_PERIOD,
  dta_pib_hab$TIME_PERIOD,
  dta_pib$TIME_PERIOD,
  dta_medicament$TIME_PERIOD,
  dta_RH_sante$TIME_PERIOD
))))

# ---- Déterminer la liste des pays disponibles ----
countries_all <- sort(unique(c(
  dta_pib_sante$Reference.area,
  dta_air_quality$Reference.area,
  dta_agri$Reference.area,
  dta_health$Reference.area,
  dta_pib_hab$Reference.area,
  dta_pib$Reference.area,
  dta_medicament$Reference.area,
  dta_RH_sante$Reference.area
)))

years_all <- years_all[years_all >= 2015]

min_year <- min(years_all, na.rm = TRUE)
max_year <- max(years_all, na.rm = TRUE)

# fonction existante (inchangée)
extract_indicator <- function(data, measure_name, year) {
  data[
    Measure == measure_name & TIME_PERIOD == year, .(Reference.area, OBS_VALUE)
  ]
}

# UI
ui <- fluidPage(
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

# Server
server <- function(input, output, session) {
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

shinyApp(ui = ui, server = server)
