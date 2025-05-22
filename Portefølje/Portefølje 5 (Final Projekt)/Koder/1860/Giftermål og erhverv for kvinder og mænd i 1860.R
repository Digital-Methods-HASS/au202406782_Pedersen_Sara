# loading the libraries
library(tidyverse)
library(ggplot2)
library(readr)
library(readxl)
library(dplyr)

#Loading the document
data_1860 <- read_excel("1860_renset.Stilling.xlsx")

#Removing NA from the Tabel
data_1860 <- data_1860 %>%
  mutate(erhverv = na_if(Stilling_renset, "NA"))


#We would like to take a closer look at womans in the ages 20 to 30 years, and their marital status 
#Filters women aged 20–30 and converts age to numeric.
kvinder_20til30 <- data_1860 %>%
  mutate(alder = as.numeric(alder)) %>%
  filter(koen == "kvinde", alder >= 20, alder <= 30)


#Groups women aged 20–30 by age and marital status, and counts the number in each groupsummary_kvinde <- kvinder_20til30 %>%
summary_kvinde <- kvinder_20til30 %>%
  group_by(alder, civilstand) %>%
  summarise(antal = n(), .groups = "drop")
kvinder_20til30


#  Creates a bar chart showing the number of women by age and marital status in 1801.
ggplot(summary_kvinde, aes(x = factor(alder), y = antal, fill = civilstand)) +
  geom_col(position = "dodge") +
  labs(title = "Civilstand for kvinder i Aarhus i 1860 i alderen 20-30 år",
       x = "Alder", y = "Antal Kvinder") +
  theme_minimal()


#We would like to take a closer look at men in the ages 20 to 30 years, and their marital status 
# Filters the dataset data_1860to include only men aged 20 to 30.
mand_20til30 <- data_1860 %>%
  filter(as.numeric(alder) >= 20 & as.numeric(alder) <= 30,
         koen == "mand")
mand_20til30

#Groups the filtered data by age and marital status and counts the number of men in each group.
summary_mand <- mand_20til30 %>%
  group_by(alder, civilstand) %>%
  summarise(antal = n(), .groups = "drop")
summary_mand

#This creates a bar plot showing the number of men by age and marital status
ggplot(summary_mand, aes(x = factor(alder), y = antal, fill = civilstand)) +
  geom_col(position = "dodge") +
  labs(title = "Civilstand for mænd i Århus i 1860 i alderen 20-30 år",
       x = "Alder", y = "Antal Mænd") +
  theme_minimal()



#The 10 Most Common Occupations for Married Women Aged 20-30 in 1801
Arbejde_giftekvinder <- data_1860 %>%
  filter(koen == "kvinde", alder >= 20, alder <= 30, civilstand == "gift") %>%
  mutate(erhverv = tolower(Stilling_renset),
         erhverv = case_when(
           erhverv %in% c("konen", "mandens kone", "dennes hustru", "hans kone", "hustru") ~ "kone",
           erhverv == "hans kone, væverske" ~ "væverske",
           erhverv == "hans kone, fabrikarb." ~ "fabriksarbejder",
           TRUE ~ erhverv
         )) %>%
  filter(!is.na(erhverv)) %>%
  group_by(erhverv) %>%
  summarise(antal = n()) %>%
  arrange(desc(antal)) %>%
  slice_head(n = 10)



# The chart based on occupation and count (married)
ggplot(Arbejde_giftekvinder, aes(x = reorder(erhverv, antal), y = antal, fill = "gift")) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    x = "Erhverv",
    y = "Antal",
    title = "De 10 hyppigste erhverv for gifte kvinder i alderen 20-30 år i 1860"
  ) +
  theme_minimal() +
  guides(fill = "none")


#unmarried women and their occupations 
# #The 10 Most Common Occupations for Unmarried Women Aged 20-30 in 1860
Arbejde_ugift1860 <- data_1860 %>%
  filter(koen == "kvinde", alder >= 20, alder <= 30, civilstand == "ugift") %>%
  filter(!is.na(erhverv)) %>%
  group_by(erhverv) %>%
  tally() %>%
  arrange(desc(n)) %>%
  slice_head(n = 10)
Arbejde_ugift1860

#The chart based on occupation and count (unmarried)
ggplot(Arbejde_ugift1860, aes(x = erhverv, y = n, fill = "ugift")) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    x = "Erhverv",
    y = "Antal",
    title = "De 10 Hyppigste erhverv for ugifte Kvinder i alderen 20-30 år i 1860"
  ) +
  theme_minimal()




#Taking a closer look at men's occupations based on their marital status, for ages between 20 and 30 years.
#top 10 Married men's occupation 
Arbejde_giftemænd1860<- data_1860 %>%
  filter(koen == "mand", alder >= 20, alder <= 30, civilstand == "gift") %>%
  filter(!is.na(erhverv)) %>%
  group_by(erhverv) %>%
  tally() %>%
  arrange(desc(n)) %>%
  slice_head(n = 10)
Arbejde_giftemænd1860

#The chart based on occupation and martial status (Married men)
ggplot(Arbejde_giftemænd1860, aes(x = erhverv, y = n, fill = "gift")) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    x = "Erhverv",
    y = "Antal",
    title = "De 10 hyppigste erhverv for gifte mænd i alderen 20-30 år i 1860"
  ) +
  theme_minimal()



#unmarried men's occuptaion based on martial status, for ages between 20 til 30 years 
Arbejde_ugiftemænd1860<- data_1860 %>%
  filter(koen == "mand", alder >= 20, alder <= 30, civilstand == "ugift") %>%
  filter(!is.na(erhverv)) %>%
  group_by(erhverv) %>%
  filter(erhverv != "Barn") %>% 
  tally() %>%
  arrange(desc(n)) %>%
  slice_head(n = 10)
Arbejde_ugiftemænd1860

#The chart based on occuptation and martial status 
ggplot(Arbejde_ugiftemænd1860, aes(x = erhverv, y = n, fill = "uift")) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(
    x = "stilling",
    y = "Antal",
    title = "De 10 hyppigste erhverv for ugifte mænd i alderen 20-30 år"
  ) +
  theme_minimal()
