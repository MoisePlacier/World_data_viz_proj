library(ggplot2)
library(plotly)
library(data.table)
library(patchwork)

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



