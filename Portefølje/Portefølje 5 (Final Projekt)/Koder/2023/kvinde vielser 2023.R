library(ggplot2)
library(tidyverse)
library(readr)
library(dplyr)
library(readxl)

kvinde_vielser_2023 <- read_excel("~/Desktop/Digitale arkiver og metoder/Portefølje/PRTF 5/data/kvinde vielse 2023.xlsx")

#Tjekker de første par rækker
head(kvinde_vielser_2023)

#Tjekker hvad kolonnerne hedder
colnames(kvinde_vielser_2023)

#Fjerner eventuelle tomme rækker
kvinde_vielser_2023 <- na.omit(kvinde_vielser_2023)

#Konverterer til numeriske værdier
kvinde_vielser_2023$Alder <- as.numeric(gsub(" år", "", kvinde_vielser_2023$Alder))
kvinde_vielser_2023$Antal <- as.numeric(kvinde_vielser_2023$Antal)

#Laver graf
ggplot(kvinde_vielser_2023, aes(x = Alder, y = Antal)) +
  geom_bar(stat = "identity", fill = "pink") +
  theme_minimal() +
  labs(title = "Antal Vielser efter Kvindens Alder (2023)",
       x = "Alder",
       y = "Antal Vielser") +
  scale_x_continuous(breaks = seq(min(kvinde_vielser_2023$Alder), max(kvinde_vielser_2023$Alder), by = 1))
