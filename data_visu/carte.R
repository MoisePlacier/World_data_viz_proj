library(ggplot2)
library(dplyr)
library(rnaturalearth)
library(rnaturalearthdata)
library(data.table)
library(plotly)
library(shiny)


load(url("https://github.com/MoisePlacier/World_data_viz_proj/raw/refs/heads/main/Data/data_cleaned.RData"))
# 1. Charger la carte du monde
world <- ne_countries(scale = "medium", returnclass = "sf")

# 2. Exemple de données PIB (à remplacer par tes données réelles)
# Important : avoir une colonne avec le code ISO3 du pays (ex: FRA pour France)
pib <- data[Tableau == "PIB"]
setnames(pib,"REF_AREA","wb_a3") 

# 3. Fusionner données avec la carte
world_pib <- left_join(world, pib, by = "wb_a3" )

ui <- fluidPage(
  sliderInput("year", "Choisir une année :", min = 2015, max = 2024, value = 2020, step = 1, sep = ""),
  plotOutput("map")
)

server <- function(input, output) {
  output$map <- renderPlot({
    data_year <- pib %>% filter(TIME_PERIOD == input$year)
    world_pib <- left_join(world, data_year, by = "wb_a3")
    
    ggplot(world_pib) +
      geom_sf(aes(fill = OBS_VALUE)) +
      scale_fill_viridis_c(option = "plasma", na.value = "grey80") +
      theme_minimal() +
      labs(title = paste("PIB par pays -", input$year), fill = "PIB")
  })
}

shinyApp(ui, server)

