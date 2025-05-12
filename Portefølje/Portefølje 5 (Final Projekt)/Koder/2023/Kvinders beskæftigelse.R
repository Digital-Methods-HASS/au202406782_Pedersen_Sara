library(ggplot2)
library(tidyverse)
library(readr)
library(dplyr)
library(readxl)

Kvinder_beskæftigelse <- read_excel("~/Desktop/Digitale arkiver og metoder/Portefølje/PRTF 5/data/Kvinder beskæftigelse filtreret.xlsx")

#Tjekker de første par rækker
head(Kvinder_beskæftigelse)

#Tjekker hvad kolonnerne hedder
colnames(Kvinder_beskæftigelse)

#Fjerner eventuelle tomme rækker
Kvinder_beskæftigelse <- na.omit(Kvinder_beskæftigelse)

#Gør det numeric
Kvinder_beskæftigelse$Antal <- as.numeric(Kvinder_beskæftigelse$Antal)

#Laver grafen
ggplot(Kvinder_beskæftigelse, aes(x = Branche, y = Antal)) +
  geom_bar(stat = "identity", fill = "red") +
  theme_minimal() +
  labs(title = "Kvinders beskæftigelse efter branche i alderen 20-29 (2023)",
       x = "Branche",
       y = "Antal beskæftigede")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 4))
