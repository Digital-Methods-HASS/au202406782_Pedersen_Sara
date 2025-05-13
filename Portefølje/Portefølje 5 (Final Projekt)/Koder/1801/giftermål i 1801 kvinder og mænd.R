# loading the libraries
library(tidyverse)
library(ggplot2)
library(readr)
library(readxl)
library(dplyr)
library(stringr)

#Loading the document
data_1801 <- read_excel("1801.renset-erhverv.xlsx")

#Filters women aged 20–30 and converts age to numeric.
kvinder_20_30 <- data_1801 %>%
  filter(alder >= 20, alder <= 30, koen == "kvinde")

kvinder_20_30 <- data_1801 %>%
  filter(as.numeric(alder) >= 20 & as.numeric(alder) <= 30,
         koen == "kvinde")

#Groups women aged 20–30 by age and marital status, and counts the number in each group.
summary_kvinde <- kvinder_20_30 %>%
  group_by(alder, civilstand) %>%
  summarise(antal = n(), .groups = "drop")


# Creates a bar chart showing the number of women by age and marital status in 1801.
ggplot(summary_kvinde, aes(x = factor(alder), y = antal, fill = civilstand)) +
  geom_col(position = "dodge") +
  labs(title = "Civilstand for kvinder i Århus i 1801 i alderen 20-30 år",
       x = "alder", y = "Antal Kvinder") +
  theme_minimal()


#We would like to take a closer look at men in the ages 20 to 30 years, and their marital status 
# Filters the dataset data_1801 to include only men aged 20 to 30.
mand_20_30 <- data_1801 %>%
  filter(as.numeric(alder) >= 20, as.numeric(alder) <= 30, koen == "mand")

#Groups the filtered data by age and marital status and counts the number of men in each group.
summary_mand <- mand_20_30 %>%
  group_by(alder, civilstand) %>%
  summarise(antal = n(), .groups = "drop")

#This creates a bar plot showing the number of men by age and marital status
ggplot(summary_mand, aes(x = factor(alder), y = antal, fill = civilstand)) +
  geom_col(position = "dodge") +
  labs(title = "Civilstand for mænd i Århus i 1801 i alderen 20-30 år",
       x = "Alder", y = "Antal Mænd") +
  theme_minimal()




#Taking a closer look at occupation for women in th ages 20 til 30 years, and their martial status. 
#The 10 Most Common Occupations for Married Women Aged 20-30 in 1801
data_1801 <- data_1801 %>%
  mutate(erhverv = na_if(Erhverv_rensen, "NA"))

Arbejde_giftkvinde1801 <- data_1801 %>%
  filter(
    koen == "kvinde",
    alder >= 20, alder <= 30,
    civilstand == "gift",
    !is.na(erhverv)
  ) %>%
  mutate(
    erhverv = case_when(
      erhverv %in% c("Inderste", "Indsidder", "Inderste, Mand tjener") ~ "Insidder",
      erhverv %in% c("Ejer hus uden jord", "Jordløs Huuskone") ~ "Jordløs huskone",
      erhverv == "Disse Unge Folk Tjener Den Gamle Fader" ~ "Tjener hos faderen",
      erhverv == "Hun ernærer sig af Haand Arbeide" ~ "Lever af Håndarbejde",
      erhverv == "Lever af Hænderne" ~ "Lever af Håndarbejde",
      erhverv == "Husmand med jord" ~ "Mand ejer jord",
      erhverv == "Jordløs Husmand" ~ "Manden ejer ingen jord",
      TRUE ~ erhverv
    )
  ) %>%
  group_by(erhverv) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(desc(n)) %>%
  slice_head(n = 10)
Arbejde_giftkvinde1801


# The chart based on occupation and count (married)
ggplot(Arbejde_giftkvinde1801, aes(x = reorder(erhverv, n), y = n)) +
  geom_bar(stat = "identity", fill = "salmon") +
  coord_flip() +
  labs(
    x = "Erhverv",
    y = "Antal",
    title = "De 10 hyppigste erhverv for gifte kvinder i Aarhus i alderen 20-30 år"
  ) +
  theme_minimal()


#unmarried women and their occupations 
# #The 10 Most Common Occupations for Unmarried Women Aged 20-30 in 1801
data_1801 <- data_1801 %>%
  mutate(erhverv = na_if(Erhverv_rensen, "NA"))

Arbejde_ugiftkvinder1801 <- data_1801 %>%
  filter(
    koen == "kvinde",
    alder >= 20, alder <= 30,
    civilstand == "ugift",
    !is.na(erhverv)
  ) %>%
  mutate(
    erhverv = case_when(
      erhverv %in% c("Inderste", "Indsidder") ~ "Insidder",
      TRUE ~ erhverv
    ),
    erhverv = str_to_title(erhverv)  # Sikrer, at erhvervet starter med stort bogstav
  ) %>%
  group_by(erhverv) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(desc(n)) %>%
  slice_head(n = 10)

Arbejde_ugiftkvinder1801


#The chart based on occupation and count (unmarried)
ggplot(Arbejde_ugiftkvinder1801, aes(x = erhverv, y = n, fill = "ugift")) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    x = "Erhverv",
    y = "Antal",
    title = "De 10 hyppigste erhverv for ugifte kvinder i Aarhus i alderen 20-30 år"
  ) +
  theme_minimal()



#Taking a closer look at men's occupations based on their marital status, for ages between 20 and 30 years.
#top 10 Married men's occupation 
Arbejde_giftmand1801 <- data_1801 %>%
  filter(
    koen == "mand",  # Filtrer for gifte mænd
    alder >= 20, alder <= 30,
    civilstand == "gift",  
    !is.na(erhverv)
  ) %>%
  mutate(
    erhverv = case_when(
      erhverv %in% c("Inderste", "Indsidder") ~ "Indsidder",  
      erhverv %in% c("national soldat", "soldat ved 1. Jyske Inf. Reg.", "soldat") ~ "Soldat",  
      erhverv %in% c("huusmand uden jord", "jordløs husmand") ~ "Husmand uden jord",  
      TRUE ~ erhverv  
    ),
    erhverv = str_to_title(erhverv)  
  ) %>%
  group_by(erhverv) %>%
  summarise(n = n(), .groups = "drop") %>%
  arrange(desc(n)) %>%
  slice_head(n = 10)
Arbejde_giftmand1801


#The chart based on occupation and martial status 
ggplot(Arbejde_giftmand1801, aes(x = erhverv, y = n, fill = "gift")) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    x = "erhverv",
    y = "antal",
    title = "De 10 hyppigste erhverv for gifte mænd i Aarhus i alderen 20-30 år"
  ) +
  theme_minimal()


#unmarried men's occuptaion based on martial status, for ages between 20 til 30 years 
Arbejde_ugiftemænd <- data_1801 %>%
  filter(koen == "mand", alder >= 20, alder <= 30, civilstand == "ugift") %>%
  filter(!is.na(erhverv)) %>% 
  mutate(
    erhverv = case_when(
      erhverv %in% c("national soldat", "soldat ved 1. Jyske Inf. Reg.", "landsoldat") ~ "Soldat",  # Saml soldatkategorier til "Soldat"
      TRUE ~ erhverv  # Behold øvrige erhverv som de er
    ),
    erhverv = str_to_title(erhverv)  
  ) %>%
  group_by(erhverv) %>%
  tally() %>%
  arrange(desc(n)) %>%
  slice_head(n = 10)
Arbejde_ugiftemænd

#The chart based on occuptation and martial status 
ggplot(Arbejde_ugiftemænd, aes(x = erhverv, y = n, fill = "ugift")) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    x = "erhverv",
    y = "antal",
    title = "De 10 hyppigste erhverv for ugifte mænd i Aarhus i alderen 20-30 år"
  ) +
  theme_minimal()

