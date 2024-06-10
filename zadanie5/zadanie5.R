# Andrzej Kocielski, 31.05.2024

# Zadanie 5 (z dnia 18.05.2024):
# Trzeba mając dane z cz.1 i cz.2 odpowiedzieć na pytania:
# 1. Jak wartości z tabeli w części II-kolumny: a, b, c, d, e wpływają na wartości w tabeli w I części zawarte w kolumnach: A, B, C, D, E, F?
# 2. (*) Jak wartości z tabeli I części-kolumny A, B, D, E wpływają na wartości w kolumnach: G, H, K, L, M, N, P, R?


# Załaduj wymagane pakiety
library(tidyverse)
library(readr)
library(broom)
library(dplyr)

# Wczytanie i przygotowanie danych 
#setwd("/home/ak/Desktop/_moje/_coding/WSB/04-wielowymiarowa-analiza/zadanie5/")
cz1 <- read_csv2("czesc_I.csv")
cz2 <- read_csv2("czesc_II.csv")

# zostawiamy tylko potrzebne kolumny 
cz1 <- cz1[, c(1, 2, 3, 4, 5, 6)] # kolumny A-F
cz2 <- cz2[, c(1, 2, 3, 4, 5)] # kolumny A-E

# Łączenie zbiorów danych na podstawie kolumny `Numer Próbki`
dane <- merge(cz1, cz2, by = "Numer próbki")


# weryfikacja założeń (kod funkcji podany na zajęciach)
zalozenia_regresja <- function(model){
  if(length(model$coefficients)==1){
    print("Funkcja to stała")
    return(NULL)
  }
  
  zalozenia <- data.frame()
  zalozenia[1,"Rozkład normalny reszt\n(Shapiro-Wilk p-value)"]<-c(round(shapiro.test(model$residuals)$p.value,4))
  zalozenia[1,"Brak heteroskadastyczności\n(Breusch-Pagan p-value)"]<-c(round(lmtest::bptest(model)$p.value,4)) #H0: homoskedastycznosc, H1: heteroskedastycznosc
  
  if(length(model$coefficients) > 2){
    vif_tab <- as.data.frame(t(car::vif(model)))
    colnames(vif_tab) <- paste("VIF:",colnames(vif_tab))
    zalozenia <- bind_cols(zalozenia, vif_tab)
  }
  
  # Interpretacja wyników testów:
  # Wartość p > 0,05 dla testu Shapiro-Wilka nie pozwala odrzucić hipotezy o normalnym rozkładzie reszt.
  # Wartość p > 0,05 dla testu Breuscha-Pagana nie pozwala odrzucić hipotezy o homoskedastyczności.
  # Wartości VIF > 10 mogą wskazywać na multikolinearność, która może negatywnie wpływać na stabilność i dokładność modelu.
  
  return(zalozenia)
}

model <- lm(dane)
zalozenia <- zalozenia_regresja(model)
zalozenia # Wnioski: rozkład normalny, homoskedastyczny; VIF > 10 (multikolinearność) dla: Rm, R0.2


### Analiza regresji

# Predyktory: kolumny B, C, D, E, F ze zbioru cz2
predyktory <- dane[, 7:10]
names(predyktory)

# Zmienne zależne: kolumny B, C, D, E, F ze zbioru cz1
zmienne_zalezne <- dane[, 2:6]
names(zmienne_zalezne)

# Lista do przechowywania wyników regresji
wyniki_regresji <- list()

# Przeprowadzanie regresji dla każdej zmiennej zależnej
for (col in 1:length(zmienne_zalezne)) {
  formulka <- as.formula(paste0("`", names(zmienne_zalezne)[col], "` ~ `C [%]` + `Si [%]` + `Mn [%]` + `Cu [%]`"))
  model_lm <- lm(formulka, data = dane)
  wyniki_regresji[[names(zmienne_zalezne)[col]]] <- summary(model_lm)
}
summary(model_lm)
#plot(model_lm)

# Wyświetlanie wyników regresji
for (nazwa in names(wyniki_regresji)) {
  cat("------ \nWyniki regresji dla zmiennej zależnej", nazwa, ":\n")
  print(wyniki_regresji[[nazwa]])
  cat("\n")
}

# Przekształcenie wyników regresji do formy przystępnej do analizy
tidy_regresje <- bind_rows(lapply(wyniki_regresji, tidy), .id = "zmienna_zalezna")

# Filtrowanie tylko istotnych predyktorów (p < 0.05)
istotne_predyktory <- tidy_regresje %>% filter(p.value < 0.05) %>%
  mutate(zmienna_zalezna = factor(zmienna_zalezna))

istotne_predyktory

# Wnioski: Istotne (p-value < 0.05) są pierwiastki Si,  Mn dla parametru wydłużenia A5 a także wyraz wolny rówania regresji dla tej zmiennej zależnej.
