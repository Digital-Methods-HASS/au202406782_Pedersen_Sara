library(ggplot2)
library(tidyverse)
library(readr)
library(dplyr)
library(readxl)

Mænd_beskæftigelse <- read_excel("~/Desktop/Digitale arkiver og metoder/Portefølje/PRTF 5/data/Mænd beskæftigelse filtreret.xlsx")

#Tjekker de første par rækker
head(Mænd_beskæftigelse)

#Tjekker hvad kolonnerne hedder
colnames(Mænd_beskæftigelse)

#Fjerner eventuelle tomme rækker
Mænd_beskæftigelse <- na.omit(Mænd_beskæftigelse)

#Gør det numeric
Mænd_beskæftigelse$Antal <- as.numeric(Mænd_beskæftigelse$Antal)

#Laver grafen
ggplot(Mænd_beskæftigelse, aes(x = Branche, y = Antal)) +
  geom_bar(stat = "identity", fill = "green") +
  theme_minimal() +
  labs(title = "Mænds beskæftigelse efter branche i alderen 20-29 (2023)",
       x = "Branche",
       y = "Antal beskæftigede")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 4))
