# Andrzej Kocielski, 22.05.2024

# Zadanie: 3
# Pytanie: Czy istnieją istotne różnice (i dla których) grup w przypadku pomiaru 
#          parametruruchu (MOT) i podziale względem dni przechowywania, warunków 
#          tlenowych i dodanej substancji. Wykonać też to zadanie dla parametru VCL. 
#          Grupy czynników: a) dni przechowywania, b)warunki tlenowe lub beztlenowe,
#          c) obecność antyoksydantów (w tym próba kontrolna).
#          Plik z danymi jest w materiałach (dane.xlsx). 


library(tidyverse)
library(readr)
library(openxlsx)

# Wczytanie i przygotowanie danych 
#setwd("/home/ak/Desktop/_moje/_coding/WSB/04-wielowymiarowa-analiza/zadanie3/")
sciezka <- "/home/ak/Desktop/_moje/_coding/WSB/04-wielowymiarowa-analiza/zadanie3/dane.xlsx" #moja własna ścieżka

###############################
##### Analiza dla parametru MOT

# warunki tlenowe
`Control 1` <- read.xlsx(sciezka, 1, rows = 2:8, cols = 1:11) %>% 
  gather(osobnik, value="MOT", -dni.przechowywania, na.rm = FALSE) %>% 
  mutate(osobnik = ifelse(osobnik == "Control", "1", osobnik),
         antyoksydant = case_when(
           osobnik %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10") ~ "Control"),
         warunki_tlenowe = case_when(
           osobnik %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10") ~ "With oxygen (MOT)"),
         dni.przechowywania = factor(dni.przechowywania, levels = c("1", "2", "4", "6", "8","10")))

`dilluted excretion (DS) 1` <- read.xlsx(sciezka, 1, rows = 11:17, cols = 1:11) %>%
  gather(osobnik, value="MOT", -dni.przechowywania, na.rm = FALSE) %>%
  mutate(osobnik = ifelse(osobnik == "Control", "1", osobnik),
         antyoksydant = case_when(
           osobnik %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10") ~ "dilluted excretion (DS)"),
         warunki_tlenowe = case_when(
           osobnik %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10") ~ "With oxygen (MOT)"),
         dni.przechowywania = factor(dni.przechowywania, levels = c("1", "2", "4", "6", "8","10")))

`diluted excretion +glutatione (DS+G) 1` <- read.xlsx(sciezka, 1, rows = 20:26, cols = 1:11) %>% 
  gather(osobnik, value="MOT", -dni.przechowywania, na.rm = FALSE) %>% 
  mutate(osobnik = ifelse(osobnik == "Control", "1", osobnik),
         antyoksydant = case_when(
           osobnik %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10") ~ "diluted excretion +glutatione (DS+G)"),
         warunki_tlenowe = case_when(
           osobnik %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10") ~ "With oxygen (MOT)"),
         dni.przechowywania = factor(dni.przechowywania, levels = c("1", "2", "4", "6", "8","10")))

`diluted excretion +vit C (DS+VC) 1` <- read.xlsx(sciezka, 1, rows = 29:35, cols = 1:11) %>%
  gather(osobnik, value="MOT", -dni.przechowywania, na.rm = FALSE) %>%
  mutate(osobnik = ifelse(osobnik == "Control", "1", osobnik),
         antyoksydant = case_when(
           osobnik %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10") ~ "diluted excretion + vit C (DS+VC)"),
         warunki_tlenowe = case_when(
           osobnik %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10") ~ "With oxygen (MOT)"),
         dni.przechowywania = factor(dni.przechowywania, levels = c("1", "2", "4", "6", "8","10")))

`diluted excretion +vitamin E (DS+VE) 1` <- read.xlsx(sciezka, 1, rows = 38:44, cols = 1:11) %>% 
  gather(osobnik, value="MOT", -dni.przechowywania, na.rm = FALSE) %>% 
  mutate(osobnik = ifelse(osobnik == "Control", "1", osobnik),
         antyoksydant = case_when(
           osobnik %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10") ~ "diluted excretion + vit E (DS+VE)"),
         warunki_tlenowe = case_when(
           osobnik %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10") ~ "With oxygen (MOT)"),
         dni.przechowywania = factor(dni.przechowywania, levels = c("1", "2", "4", "6", "8","10")))


# warunki beztlenowe
`Control 2` <- read.xlsx(sciezka, 2, rows = 2:8, cols = 1:11) %>% 
  gather(osobnik, value="MOT", -dni.przechowywania, na.rm = FALSE) %>% 
  mutate(osobnik = ifelse(osobnik == "Control", "1", osobnik),
         antyoksydant = case_when(
           osobnik %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10") ~ "Control"),
         warunki_tlenowe = case_when(
           osobnik %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10") ~ "Without oxygen (MOT)"),
         dni.przechowywania = factor(dni.przechowywania, levels = c("1", "2", "4", "6", "8","10")))

`dilluted excretion (DS) 2` <- read.xlsx(sciezka, 2, rows = 11:17, cols = 1:11) %>%
  gather(osobnik, value="MOT", -dni.przechowywania, na.rm = FALSE) %>%
  mutate(osobnik = ifelse(osobnik == "Control", "1", osobnik),
         antyoksydant = case_when(
           osobnik %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10") ~ "dilluted excretion (DS)"),
         warunki_tlenowe = case_when(
           osobnik %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10") ~ "Without oxygen (MOT)"),
         dni.przechowywania = factor(dni.przechowywania, levels = c("1", "2", "4", "6", "8","10")))

`diluted excretion +glutatione (DS+G) 2` <- read.xlsx(sciezka, 2, rows = 20:26, cols = 1:11) %>% 
  gather(osobnik, value="MOT", -dni.przechowywania, na.rm = FALSE) %>% 
  mutate(osobnik = ifelse(osobnik == "Control", "1", osobnik),
         antyoksydant = case_when(
           osobnik %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10") ~ "diluted excretion +glutatione (DS+G)"),
         warunki_tlenowe = case_when(
           osobnik %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10") ~ "Without oxygen (MOT)"),
         dni.przechowywania = factor(dni.przechowywania, levels = c("1", "2", "4", "6", "8","10")))

`diluted excretion +vit C (DS+VC) 2` <- read.xlsx(sciezka, 2, rows = 29:35, cols = 1:11) %>%
  gather(osobnik, value="MOT", -dni.przechowywania, na.rm = FALSE) %>%
  mutate(osobnik = ifelse(osobnik == "Control", "1", osobnik),
         antyoksydant = case_when(
           osobnik %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10") ~ "diluted excretion + vit C (DS+VC)"),
         warunki_tlenowe = case_when(
           osobnik %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10") ~ "Without oxygen (MOT)"),
         dni.przechowywania = factor(dni.przechowywania, levels = c("1", "2", "4", "6", "8","10")))

`diluted excretion +vitamin E (DS+VE) 2` <- read.xlsx(sciezka, 2, rows = 38:44, cols = 1:11) %>% 
  gather(osobnik, value="MOT", -dni.przechowywania, na.rm = FALSE) %>% 
  mutate(osobnik = ifelse(osobnik == "Control", "1", osobnik),
         antyoksydant = case_when(
           osobnik %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10") ~ "diluted excretion + vit E (DS+VE)"),
         warunki_tlenowe = case_when(
           osobnik %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10") ~ "Without oxygen (MOT)"),
         dni.przechowywania = factor(dni.przechowywania, levels = c("1", "2", "4", "6", "8","10")))


# Połączenie danych w jeden zbiór
dane_mot <- rbind(`Control 1`, `dilluted excretion (DS) 1`, `diluted excretion +glutatione (DS+G) 1`,
              `diluted excretion +vit C (DS+VC) 1`, `diluted excretion +vitamin E (DS+VE) 1`,
              `Control 2`, `dilluted excretion (DS) 2`, `diluted excretion +glutatione (DS+G) 2`,
              `diluted excretion +vit C (DS+VC) 2`, `diluted excretion +vitamin E (DS+VE) 2`)


##### Jednoczynnikowa analiza wariancji
# H0: nie ma istotnych różnic w ocenach MOT w zależności od pojedynczego czynnika (wszystkie grupy mają tę samą średnią); 
# H1: istnieją istotne różnice (przynajmniej jedna z grup ma średnią różniącą się od pozostałych)
summary(aov(MOT ~ antyoksydant, data = dane_mot)) # H1
summary(aov(MOT ~ warunki_tlenowe, data = dane_mot)) # H1
summary(aov(MOT ~ dni.przechowywania, data = dane_mot)) # H1
# Wniosek: Każdy z czynników niezależnie jest istotny na parametr MOT

# Analiza Tukeya-Kramera (która z grup rózni się od pozostałych), np. dla antyoksydantów:
TukeyHSD(aov(MOT ~ antyoksydant, data = dane_mot))
plot(TukeyHSD(aov(MOT ~ antyoksydant, data = dane_mot)))


##### Wieloczynnikowa analiza wariancji
# Założenia: a) próby wewnątrz grup mają rozkład normalny, b) wszystkie próby wewnątrz grup mają stałą i jednakową wariancję, c) próby są niezależne


#H0: Nie ma istotnej interakcji między czynnikami; H1: istnieją takie interakcje.
summary(aov_MOT <- aov(MOT ~ antyoksydant * warunki_tlenowe * dni.przechowywania, data = dane_mot))
# Wniosek: Wszystkie główne interakcje (zarówno dwu- jak i trójstopniowe) są istotne (p < 0.05). 
# Oznacza to, że efekt jednego czynnika zależy od poziomów innych czynników.


# Przeprowadzenie testu Tukeya
tukey_MOT <- TukeyHSD(aov_MOT)

# Wyświetlenie szczegółowych wyników dla poszczególnych czynników i ich interakcji (interakcje są istotnte dla p-value < 0.05)
#print(tukey_MOT$antyoksydant)
#print(tukey_MOT$`warunki_tlenowe`)
#print(tukey_MOT$`dni.przechowywania`)
#print(tukey_MOT$`antyoksydant:warunki_tlenowe`)
#print(tukey_MOT$`antyoksydant:dni.przechowywania`)
#print(tukey_MOT$`warunki_tlenowe:dni.przechowywania`)
#print(tukey_MOT$`antyoksydant:warunki_tlenowe:dni.przechowywania`)



###############################
##### Analiza dla parametru VCL 

# warunki tlenowe
`Control 1` <- read.xlsx(sciezka, 3, rows = 2:8, cols = 1:11) %>% 
  gather(osobnik, value="VCL", -dni.przechowywania, na.rm = FALSE) %>% 
  mutate(osobnik = ifelse(osobnik == "Control", "1", osobnik),
         antyoksydant = case_when(
           osobnik %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10") ~ "Control"),
         warunki_tlenowe = case_when(
           osobnik %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10") ~ "With oxygen (VCL)"),
         dni.przechowywania = factor(dni.przechowywania, levels = c("1", "2", "4", "6", "8","10")))

`dilluted excretion (DS) 1` <- read.xlsx(sciezka, 3, rows = 11:17, cols = 1:11) %>%
  gather(osobnik, value="VCL", -dni.przechowywania, na.rm = FALSE) %>%
  mutate(osobnik = ifelse(osobnik == "Control", "1", osobnik),
         antyoksydant = case_when(
           osobnik %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10") ~ "dilluted excretion (DS)"),
         warunki_tlenowe = case_when(
           osobnik %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10") ~ "With oxygen (VCL)"),
         dni.przechowywania = factor(dni.przechowywania, levels = c("1", "2", "4", "6", "8","10")))

`diluted excretion +glutatione (DS+G) 1` <- read.xlsx(sciezka, 3, rows = 20:26, cols = 1:11) %>% 
  gather(osobnik, value="VCL", -dni.przechowywania, na.rm = FALSE) %>% 
  mutate(osobnik = ifelse(osobnik == "Control", "1", osobnik),
         antyoksydant = case_when(
           osobnik %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10") ~ "diluted excretion +glutatione (DS+G)"),
         warunki_tlenowe = case_when(
           osobnik %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10") ~ "With oxygen (VCL)"),
         dni.przechowywania = factor(dni.przechowywania, levels = c("1", "2", "4", "6", "8","10")))

`diluted excretion +vit C (DS+VC) 1` <- read.xlsx(sciezka, 3, rows = 29:35, cols = 1:11) %>%
  gather(osobnik, value="VCL", -dni.przechowywania, na.rm = FALSE) %>%
  mutate(osobnik = ifelse(osobnik == "Control", "1", osobnik),
         antyoksydant = case_when(
           osobnik %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10") ~ "diluted excretion + vit C (DS+VC)"),
         warunki_tlenowe = case_when(
           osobnik %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10") ~ "With oxygen (VCL)"),
         dni.przechowywania = factor(dni.przechowywania, levels = c("1", "2", "4", "6", "8","10")))

`diluted excretion +vitamin E (DS+VE) 1` <- read.xlsx(sciezka, 3, rows = 38:44, cols = 1:11) %>% 
  gather(osobnik, value="VCL", -dni.przechowywania, na.rm = FALSE) %>% 
  mutate(osobnik = ifelse(osobnik == "Control", "1", osobnik),
         antyoksydant = case_when(
           osobnik %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10") ~ "diluted excretion + vit E (DS+VE)"),
         warunki_tlenowe = case_when(
           osobnik %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10") ~ "With oxygen (VCL)"),
         dni.przechowywania = factor(dni.przechowywania, levels = c("1", "2", "4", "6", "8","10")))


# warunki beztlenowe
`Control 2` <- read.xlsx(sciezka, 4, rows = 2:8, cols = 1:11) %>% 
  gather(osobnik, value="VCL", -dni.przechowywania, na.rm = FALSE) %>% 
  mutate(osobnik = ifelse(osobnik == "Control", "1", osobnik),
         antyoksydant = case_when(
           osobnik %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10") ~ "Control"),
         warunki_tlenowe = case_when(
           osobnik %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10") ~ "Without oxygen (VCL)"),
         dni.przechowywania = factor(dni.przechowywania, levels = c("1", "2", "4", "6", "8","10")))

`dilluted excretion (DS) 2` <- read.xlsx(sciezka, 4, rows = 11:17, cols = 1:11) %>%
  gather(osobnik, value="VCL", -dni.przechowywania, na.rm = FALSE) %>%
  mutate(osobnik = ifelse(osobnik == "Control", "1", osobnik),
         antyoksydant = case_when(
           osobnik %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10") ~ "dilluted excretion (DS)"),
         warunki_tlenowe = case_when(
           osobnik %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10") ~ "Without oxygen (VCL)"),
         dni.przechowywania = factor(dni.przechowywania, levels = c("1", "2", "4", "6", "8","10")))

`diluted excretion +glutatione (DS+G) 2` <- read.xlsx(sciezka, 4, rows = 20:26, cols = 1:11) %>% 
  gather(osobnik, value="VCL", -dni.przechowywania, na.rm = FALSE) %>% 
  mutate(osobnik = ifelse(osobnik == "Control", "1", osobnik),
         antyoksydant = case_when(
           osobnik %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10") ~ "diluted excretion +glutatione (DS+G)"),
         warunki_tlenowe = case_when(
           osobnik %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10") ~ "Without oxygen (VCL)"),
         dni.przechowywania = factor(dni.przechowywania, levels = c("1", "2", "4", "6", "8","10")))

`diluted excretion +vit C (DS+VC) 2` <- read.xlsx(sciezka, 4, rows = 29:35, cols = 1:11) %>%
  gather(osobnik, value="VCL", -dni.przechowywania, na.rm = FALSE) %>%
  mutate(osobnik = ifelse(osobnik == "Control", "1", osobnik),
         antyoksydant = case_when(
           osobnik %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10") ~ "diluted excretion + vit C (DS+VC)"),
         warunki_tlenowe = case_when(
           osobnik %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10") ~ "Without oxygen (VCL)"),
         dni.przechowywania = factor(dni.przechowywania, levels = c("1", "2", "4", "6", "8","10")))

`diluted excretion +vitamin E (DS+VE) 2` <- read.xlsx(sciezka, 4, rows = 38:44, cols = 1:11) %>% 
  gather(osobnik, value="VCL", -dni.przechowywania, na.rm = FALSE) %>% 
  mutate(osobnik = ifelse(osobnik == "Control", "1", osobnik),
         antyoksydant = case_when(
           osobnik %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10") ~ "diluted excretion + vit E (DS+VE)"),
         warunki_tlenowe = case_when(
           osobnik %in% c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10") ~ "Without oxygen (VCL)"),
         dni.przechowywania = factor(dni.przechowywania, levels = c("1", "2", "4", "6", "8","10")))


# Połączenie danych w jeden zbiór
dane_vcl <- rbind(`Control 1`, `dilluted excretion (DS) 1`, `diluted excretion +glutatione (DS+G) 1`,
                  `diluted excretion +vit C (DS+VC) 1`, `diluted excretion +vitamin E (DS+VE) 1`,
                  `Control 2`, `dilluted excretion (DS) 2`, `diluted excretion +glutatione (DS+G) 2`,
                  `diluted excretion +vit C (DS+VC) 2`, `diluted excretion +vitamin E (DS+VE) 2`)


##### Jednoczynnikowa analiza wariancji
# H0: nie ma istotnych różnic w ocenach MOT w zależności od pojedynczego czynnika (wszystkie grupy mają tę samą średnią); 
# H1: istnieją istotne różnice (przynajmniej jedna z grup ma średnią różniącą się od pozostałych)
summary(aov(VCL ~ antyoksydant, data = dane_vcl)) # H1
summary(aov(VCL ~ warunki_tlenowe, data = dane_vcl)) # H1
summary(aov(VCL ~ dni.przechowywania, data = dane_vcl)) # H1
# Wniosek: Każdy z czynników niezależnie jest istotny na parametr MOT

# Analiza Tukeya-Kramera (która z grup rózni się od pozostałych), np. dla antyoksydantów:
TukeyHSD(aov(VCL ~ antyoksydant, data = dane_vcl))
plot(TukeyHSD(aov(VCL ~ antyoksydant, data = dane_vcl)))


##### Wieloczynnikowa analiza wariancji
# Założenia: a) próby wewnątrz grup mają rozkład normalny, b) wszystkie próby wewnątrz grup mają stałą i jednakową wariancję, c) próby są niezależne

#H0: Nie ma istotnej interakcji między czynnikami; H1: istnieją takie interakcje.
summary(aov_VCL <- aov(VCL ~ antyoksydant * warunki_tlenowe * dni.przechowywania, data = dane_vcl))
# Wniosek: Wszystkie główne interakcje (zarówno dwu- jak i trójstopniowe) są istotne (p < 0.05). 
# Oznacza to, że efekt jednego czynnika zależy od poziomów innych czynników.


# Przeprowadzenie testu Tukeya
tukey_VCL <- TukeyHSD(aov_VCL)

# Wyświetlenie szczegółowych wyników dla poszczególnych czynników i ich interakcji (interakcje są istotnte dla p-value < 0.05)
#print(tukey_VCL$antyoksydant)
#print(tukey_VCL$`warunki_tlenowe`)
#print(tukey_VCL$`dni.przechowywania`)
#print(tukey_VCL$`antyoksydant:warunki_tlenowe`)
#print(tukey_VCL$`antyoksydant:dni.przechowywania`)
#print(tukey_VCL$`warunki_tlenowe:dni.przechowywania`)
#print(tukey_VCL$`antyoksydant:warunki_tlenowe:dni.przechowywania`)

