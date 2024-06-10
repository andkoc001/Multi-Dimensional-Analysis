# Andrzej Kocielski, 19.05.2024

# Zadanie 6 (19.05.2024):
# Analiza wieloczynnikowa - dostarczonych danych
# Do zrobienia (dla zarządu tylko; rada nadzorcza zrobiona na zajęciach):
# 0. sprawdzenie założeń
# 1. EFA: czy w zbirze danych (albo poszczególnych grupach - kolory pól w execelu - w zadaniu wystarczy tylko jedna grupa lub dla całości) można zidentyfikować zmienne latentne
# 2. CFA: ocena jak te zmienne latentne między sobą oddziałują (miara dopasowania modelu, np. nfi (powinno być blisko 1))
# 3. SEM: analiza strukturalna

# Ocenić które z zadań zarządu zajmują najwięcej czasu, wraz z interpretacją graficzną


library(tidyverse)
library(readr)
library(openxlsx)
library(psych)
library(lavaan)


# Wczytanie i przygotowanie danych 
setwd("/home/ak/Desktop/_moje/_coding/WSB/04-wielowymiarowa-analiza/zadanie6/")
sciezka <- "/home/ak/Desktop/_moje/_coding/WSB/04-wielowymiarowa-analiza/zadanie6/Dane.xlsx" #moja własna ścieżka


dane <- read.xlsx(sciezka, sheet=2, rows = 1:31)#, cols = 1:11, range="A1:AZ30") 
#summary(dane)




# dobór liczby czynników metodą równoległą (są też inne metody, np. metoda demokratyczna (głosowanie większościowe))
fa.parallel(frnt)

#########################


library(lavaan)
myData <- read.csv(dane.csv)
myModel <- '
    f1 =~ item1 + item2 + item3
    f2 =~ item4 + item5 + item6
    f3 =~ item7 + item8 + item9
    '


