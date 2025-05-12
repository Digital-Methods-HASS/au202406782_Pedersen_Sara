install.packages("ggplot2")
install.packages("tidyverse")
install.packages("readr")
install.packages("dplyr")
install.packages("readxl")

library(ggplot2)
library(tidyverse)
library(readr)
library(dplyr)
library(readxl)

#Indlæser Excel-filen
mand_vielser_2023 <- read_excel("~/Desktop/Digitale arkiver og metoder/Portefølje/PRTF 5/data/mand vielser 2023.xlsx")

#Tjekker de første par rækker
head(mand_vielser_2023)

#Tjekker hvad kolonnerne hedder
colnames(mand_vielser_2023)

#Fjerner eventuelle tomme rækker
mand_vielser_2023 <- na.omit(mand_vielser_2023)

#Konverterer til numeriske værdier
mand_vielser_2023$Alder <- as.numeric(gsub(" år", "", mand_vielser_2023$Alder))
mand_vielser_2023$Antal <- as.numeric(mand_vielser_2023$Antal)

#Laver graf
ggplot(mand_vielser_2023, aes(x = Alder, y = Antal)) +
  geom_bar(stat = "identity", fill = "blue") +
  theme_minimal() +
  labs(title = "Antal Vielser efter Mandens Alder (2023)",
       x = "Alder",
       y = "Antal Vielser") +
  scale_x_continuous(breaks = seq(min(mand_vielser_2023$Alder), max(mand_vielser_2023$Alder), by = 1))
