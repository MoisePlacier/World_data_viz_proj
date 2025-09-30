library(ggplot2)
library(plotly)
library(data.table)

levels(data$Tableau)
health <- data[Tableau == "Health" & REF_AREA %in% c("FRA", "USA", "ZAF", "JPN")]
pib_health <- data[Tableau == "PIB sante" & REF_AREA %in% c("FRA", "USA", "ZAF", "JPN")]

ggplot(data = health)+
         geom_line(data = health ,aes(x= TIME_PERIOD, y= OBS_VALUE, color = REF_AREA))

ggplot(data = pib_health, aes(x = TIME_PERIOD, y = OBS_VALUE, fill = REF_AREA)) +
  geom_col(position = "dodge")
