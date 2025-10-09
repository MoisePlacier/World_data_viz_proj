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
### Data importation

dta_pib_sante <- readRDS("./Data/dta_pib_sante.rds")
dta_air_quality <- readRDS("./Data/dta_air_quality.rds")
dta_agri <- readRDS("./Data/dta_agri.rds")
dta_health <- readRDS("./Data/dta_health.rds")
dta_pib_hab <- readRDS("./Data/dta_pib_hab.rds")
dta_pib <- readRDS("./Data/dta_pib.rds")
dta_medicament <- readRDS("./Data/dta_medicament.rds")
dta_RH_sante <- readRDS("./Data/dta_RH_sante.rds")


#####----
### Premier graphique : Graph 2D Esperance de vie X Air Quality X PIB_Sante


#Extraire les indiacteurs pour une année dans un tableau de données
extract_indicator <- function(data, measure_name, year) {
  data[
    Measure == measure_name & TIME_PERIOD == year, .(Reference.area, OBS_VALUE)
  ]
}

pib_sante_clean <- dta_pib_sante %>%
  group_by(Reference.area, TIME_PERIOD) %>%       # regrouper par pays et année
  slice_max(order_by = OBS_VALUE, n = 1, with_ties = FALSE) %>%  # garder la ligne avec OBS_VALUE max
  ungroup()

pib_sante_clean <- as.data.table(pib_sante_clean)

# Exemple pour l’année 2019
year_to_plot <- 2019

#pesticides <- extract_indicator(dta_agri, "Total sales of agricultural pesticides", year_to_plot)
air <- extract_indicator(dta_air_quality, "Population share exposed", year_to_plot)
health <- extract_indicator(dta_health, "Life expectancy", year_to_plot)
pib_sante <- extract_indicator(pib_sante_clean, "Expenditure", year_to_plot)

#Fusion des données
data_merged <- Reduce(function(x, y) merge(x, y, by = "Reference.area", all = TRUE),
                      list(air, pib_sante, health))

#Format
setnames(data_merged, c("Reference.area", "AirQuality", "HealthGDP", "LifeExpectancy"))
data_merged <- as.data.frame(data_merged)

data_merged <- data_merged %>%
  filter(!is.na(AirQuality),
         !is.na(LifeExpectancy), !is.na(HealthGDP))

sizes <- rescale(data_merged$HealthGDP, to = c(5, 20))

#Graph
plot_ly(
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
      text = paste("Santé et qualité de l'air au sein de l'OCDE (", year_to_plot, ")", sep = ""),
      font = list(size = 20),
      x = 0.5,          
      xanchor = "center",
      y = 0.95,         
      yanchor = "top"
    ),
    xaxis = list(title = "% de la Population exposée à plus de 15µg/m³ de PM2.5", zeroline = FALSE),
    yaxis = list(title = "Espérance de vie (ans)")
    )

####----
### Deuxième Graphique : Matrice des corrélations des indicateurs

## Preprocessing ds tableaux pour qu'ils soient compatibles entre eux
RH_sante_clean <- dta_RH_sante %>%
  # Filtrer uniquement les lignes qui nous intéressent
  filter(Health.profession == "Total",
         Unit.of.measure == "Per 1 000 inhabitants") %>%
  select(-Health.profession)

pib_sante_clean <- dta_pib_sante %>%
  group_by(Reference.area, TIME_PERIOD) %>%       # regrouper par pays et année
  slice_max(order_by = OBS_VALUE, n = 1, with_ties = FALSE) %>%  # garder la ligne avec OBS_VALUE max
  ungroup()

# Exemple : liste de tous tes tableaux
liste_tableaux <- list(pib_sante_clean, dta_air_quality, dta_health, dta_pib_hab, dta_pib, dta_medicament, RH_sante_clean)


# Fonction pour extraire un tableau "large" par année
prepare_data_year <- function(tab, year) {
  tab %>%
    filter(TIME_PERIOD == year) %>%
    select(Reference.area, Measure, OBS_VALUE) %>%
    pivot_wider(names_from = Measure, values_from = OBS_VALUE)
}

year_to_plot2 <- 2019

# Fusionner tous les tableaux pour l'année donnée
data_wide <- liste_tableaux %>%
  map(~prepare_data_year(.x, year_to_plot2)) %>%
  reduce(full_join, by = "Reference.area")

# Retirer les pays avec trop de NA
data_wide <- data_wide %>%
  filter(rowSums(is.na(.)) < ncol(.)/2)  # garde pays avec au moins 50% des données

# Supprimer la colonne Reference.area pour corrélation
data_corr <- data_wide %>% select(-Reference.area)

# Corrélation pairwise (Pearson)
corr_matrix <- cor(data_corr, use = "pairwise.complete.obs")

# Calcul des corrélations
corr_matrix <- cor(data_corr, use = "pairwise.complete.obs")

# Mettre en format long
corr_long <- reshape2::melt(corr_matrix)

labels_vars <- c(
  "Hospital employment" = "Emploi secteur de la Santé",
  "LifeExpectancy" = "Espérance de vie",
  "Expenditure" = "% PIB investit dans la Santé",
  "Pharmaceutical consumption" = "Consommation de médicaments par habitant",
  "Gross domestic product" = "PIB",
  "Gross domestic product per capita" = "PIB par habitant",
  "Population share exposed" = "% de la population exposée à la pollution de l'air"
)

ggplot(corr_long, aes(Var1, Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "red", mid = "white", high = "green", midpoint = 0) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  ggtitle(paste("Heatmap des corrélations OCDE (", year_to_plot, ")", sep = ""))

ggplot(corr_long, aes(Var1, Var2, fill = value)) +
  geom_tile(color = "white", linewidth = 0.5) + 
  geom_text(aes(label = round(value, 2)), size = 3.5, color = "black") +  
  scale_fill_gradient2(
    low = "#B2182B", mid = "white", high = "#2166AC", midpoint = 0,
    limits = c(-1, 1),
    name = "Corrélation"
  ) +
  scale_x_discrete(labels = labels_vars) +
  scale_y_discrete(labels = labels_vars) +  
  labs(
    title = paste("Heatmap des corrélations entre indicateurs des données OCDE (", year_to_plot, ")", sep = ""),
    x = NULL,
    y = NULL
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(
      size = 14,
      face = "bold",
      hjust = 0.5,
      margin = margin(b = 15)
    ),
    axis.text.x = element_text(
      angle = 45,
      vjust = 1,
      hjust = 1,
      size = 9,
      face = "bold"
    ),
    axis.text.y = element_text(
      size = 9,
      face = "bold"
    ),
    panel.grid = element_blank(),
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 8)
  ) +
  coord_fixed() 


####----
### 3eme graphe : Scatter PLot Medicament X PIB Hab X Esperance de vie

# Exemple pour l’année 2019
year_to_plot3 <- 2019

#data extraction
pib_hab <- extract_indicator(dta_pib_hab, "Gross domestic product per capita", year_to_plot3)
medicaments <- extract_indicator(dta_medicament, "Pharmaceutical consumption", year_to_plot3)
health <- extract_indicator(dta_health, "Life expectancy", year_to_plot3)
rh_sante <- extract_indicator(RH_sante_clean, "Hospital employment", year_to_plot3)
pib <- extract_indicator(dta_pib, "Gross domestic product", year_to_plot3)
#pib_sante <- extract_indicator(pib_sante_clean, "Expenditure", year_to_plot3)

#Data frame wide pour l'année choisie
data_merged2 <- Reduce(function(x, y) merge(x, y, by = "Reference.area", all = TRUE),
                      list(pib, medicaments, health, rh_sante))

setnames(data_merged2, c("Reference.area", "PIB", "Consommation de médicaments", "LifeExpectancy", "Ressources humaines sante"))
data_merged2 <- as.data.frame(data_merged2)


#Modèle de reg linéaire
data_model <- data_merged2 %>%
  filter(!is.na(`Consommation de médicaments`), !is.na(PIB))

mod <- lm(`Consommation de médicaments` ~ PIB, data = data_model)
data_model <- augment(mod, data = data_model)
data_model <- data_model %>%
  mutate(atypique = abs(.resid) > sd(.resid, na.rm = TRUE) * 1.5)
pred <- data.frame(
  PIB = seq(min(data_model$PIB), max(data_model$PIB), length.out = 100)
)
pred$y <- predict(mod, newdata = pred)

# Fusionner les données pour que les labels atypiques et cercles noirs utilisent les mêmes points
data_labels <- data_model %>%
  mutate(
    # on peut identifier les labels des atypiques et normaux
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
  
  # Cercles noirs autour des atypiques
  geom_point(
    data = subset(data_model, atypique),
    shape = 21,
    color = "black",
    stroke = 1.2,
    fill = NA,
    size = subset(data_model, atypique)$`Ressources humaines sante`,
    show.legend = FALSE
  ) +
  
  # Labels pour tous les pays
  geom_text_repel(
    data = data_labels,
    aes(
      label = Reference.area,
      fontface = fontface,
      segment.color = segment_color,
      segment.size = segment_size
    ),
    size = 3.5,
    max.overlaps = 50,
    min.segment.length = 0,
    segment.alpha = 0.8,
    point.padding = 0.8,       # ← prend en compte la taille du point
    box.padding = 1          # ← espace minimal autour du texte
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
    title = "Consommation de médicaments vs PIB (pays OCDE)",
    subtitle = "Les pays atypiques sont entourés et leurs noms est en gras, \nLa taille des cercles est proportionnelle avec le nombre d'emploi dans la santé pour 1000 hab",
    x = "PIB (log10, en milliards de $US)",
    y = "Médicaments consommés par habitant (Moyenne annuelle)"
  )


####---- 
### 4eme Graphique : ACP

year_to_plot4 <- 2019

RH_sante_clean <- dta_RH_sante %>%
  # Filtrer uniquement les lignes qui nous intéressent
  filter(Health.profession == "Total",
         Unit.of.measure == "Per 1 000 inhabitants") %>%
  select(-Health.profession)

air <- extract_indicator(dta_air_quality, "Population share exposed", year_to_plot4)
health <- extract_indicator(dta_health, "Life expectancy", year_to_plot4)
pib_sante <- extract_indicator(pib_sante_clean, "Expenditure", year_to_plot4)
pib_hab <- extract_indicator(dta_pib_hab, "Gross domestic product per capita", year_to_plot4)
medicaments <- extract_indicator(dta_medicament, "Pharmaceutical consumption", year_to_plot4)
rh_sante <- extract_indicator(RH_sante_clean, "Hospital employment", year_to_plot4)
pib <- extract_indicator(dta_pib, "Gross domestic product", year_to_plot4)

air <- rename(air, AirQuality = OBS_VALUE)
health <- rename(health, LifeExpectancy = OBS_VALUE)
pib_sante <- rename(pib_sante, HealthGDP = OBS_VALUE)
pib_hab <- rename(pib_hab, GDP_per_capita = OBS_VALUE)
medicaments <- rename(medicaments, PharmaceuticalConsumption = OBS_VALUE)
rh_sante <- rename(rh_sante, HealthStaff = OBS_VALUE)
pib <- rename(pib, GDP = OBS_VALUE)


#Fusion des données
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

rownames(data_merged) <- data_merged$Reference.area
data_merged <- data_merged %>% select(-Reference.area)

res.pca <- PCA(data_merged, scale.unit = TRUE, graph = FALSE)

# Graphique des individus
fviz_pca_biplot(res.pca,        
             pointsize = 3,
             repel = TRUE,
             col.ind = "cos2",    
             gradient.cols = viridis(100, option = "plasma"),
             labelsize = 4,
             axes = c(1, 2),     
             invisible = "var",
             title = "PCA des pays de l'OCDE - Qualité de représentation")

# Graphique des variables
fviz_pca_var(res.pca,
             col.var = "cos2",        # couleur selon qualité de représentation
             gradient.cols = viridis(100, option = "plasma"),
             repel = TRUE)
