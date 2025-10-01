library(ggplot2)
library(plotly)
library(data.table)
library(patchwork)

<<<<<<< HEAD
data <- readRDS("../Data/data_cleaned.RData")
=======
load(url("https://github.com/MoisePlacier/World_data_viz_proj/raw/refs/heads/main/Data/data_cleaned.RData"))
>>>>>>> 95715398d489567f4264a51597ada3faef3ffb78

levels(data$Tableau)
health <- data[Tableau == "Health" & REF_AREA %in% c("FRA", "USA", "ZAF", "JPN")]
pib_health <- data[Tableau == "PIB sante" & REF_AREA %in% c("FRA", "USA", "ZAF", "JPN") & TIME_PERIOD != "2024" ]

h <- ggplot(data = health) + 
      geom_line(data = health, aes(x= TIME_PERIOD, y= OBS_VALUE, color = REF_AREA)) +
      labs(x = "ANNEE", y = "Esperance de vie moyenne", color = "Pays")

                  

p <- ggplot(data = pib_health, aes(x = TIME_PERIOD, y = OBS_VALUE, fill = REF_AREA)) +
  geom_col(position = "dodge") +
  labs(x = "ANNEE", y = "Pourcentage du PIB investit dans la sante", fill = "Pays")

h/p





