# Andrzej Kocielski, 24.05.2024

# Zadanie: 4
# Pytanie: Wzorując się na materiale z regresji wielowymiarowej (sekcja Materiały), 
#          wybrać z "cars_dataset" wszystkie (rozważyć też te, których nie ma w 
#          przykładowej analizie) zmienne istotnie wpływające na cenę samochodu 
#          i oszacować siłę tego wpływu. Przeprowadzić diagnostykę modelu.


library(readr)
library(tidyverse)
library(ggplot2)
library(ggcorrplot)
library(car)

##### Wczytanie danych
#setwd <- "/home/ak/Desktop/_moje/_coding/WSB/04-wielowymiarowa-analiza/zadanie4/"
dane <- read_csv("cars_dataset.csv")

# ograniczamy wielkość zbioru danych
cars <- sample_n(dane, 10000)

# szybki podgląd danych
#sample_n(cars, 1)
#summary(cars)

# odfiltrowanie złych danych (mocno odstające lub pomijalne - arbirtralnie)
cars <- cars %>%
  filter(engineSize > 0.5, engineSize <= 5, 
         mileage >= 2, mileage <= 250000, 
         mpg >= 10, mpg <= 100, 
         fuelType != "Other", 
         transmission != "Other")

# Zamiana zmiennej 'year' na 'age'
cars <- cars %>% mutate(year = 2024 - year) %>% rename(age = year)

# Dodawanie nowych syntetycznych zmiennych
set.seed(123)

# Generowanie nowej zmiennej Accident_History (przyjęto, że średnio 80% nie ma historii wypadku) 
cars2 <- cars %>%
  mutate(Accident_History = runif(nrow(cars), min = 0, max = 1)) %>%
  mutate(Accident_History = ifelse(Accident_History > 0.8, 1, 0))
#summary(cars2$Accident_History)

# Generowanie nowej zmiennej Average_Maintenance_Cost, przy użyciu rozkładu beta 
cars2 <- cars2 %>%
  mutate(Average_Maintenance_Cost = rbeta(nrow(cars), shape1 = 2, shape2 = 25)) %>%
  mutate(Average_Maintenance_Cost = 100 + (Average_Maintenance_Cost * (3000 - 100)))
#summary(cars2$Average_Maintenance_Cost)

# Histogram dla Average_Maintenance_Cost
ggplot(cars2, aes(x = Average_Maintenance_Cost)) + 
  geom_histogram(binwidth = 100, fill = "blue", color = "black") + 
  labs(title = "Histogram of Average Maintenance Cost", x = "Average Maintenance Cost", y = "Frequency") +
  theme_minimal()



# Analiza korelacji oryginalnych zmiennych liczbowych
numeric_columns <- cars %>% select(age, price, mileage, tax, mpg, engineSize)
cor(numeric_columns)
# mapa cieplna korelacji
ggcorrplot(cor(numeric_columns), type='lower') # oryginalny zbiór danych


# Aktualizacja macierzy korelacji z nowymi zmiennymi
numeric_columns_all <- cars2 %>% 
  select(age, price, mileage, tax, mpg, engineSize, Accident_History, Average_Maintenance_Cost)
round(cor(numeric_columns_all), 2)
# mapa cieplna korelacji
ggcorrplot(cor(numeric_columns_all), type='lower') # dodatkowe dane syntetyczne



##### Modele regresji liniowej

# age
summary(lm(price ~ log(age), data=cars2))

# age + Average_Maintenance_Cost
summary(lm(price ~ log(age)+Average_Maintenance_Cost, data=cars2))

# age + mpg + engineSize + mileage + Accident_History + Average_Maintenance_Cost
lm_model <- lm(price ~ log(age)+mpg+engineSize+mileage+Accident_History+Average_Maintenance_Cost, data=cars2)
summary(lm_model)


###### Diagnostyka modelu

# Współczynnik wariancji (VIF); VIF większa niż np. 10 sugeruje występowanie współliniowości.
vif(lm_model) 

# Ocena istotności poszczególnych zmiennych (jednoczynnikowa ANOVA) 
anova(lm_model)

# Interpretacja graficzna
plot(lm_model, which=1:5) 

