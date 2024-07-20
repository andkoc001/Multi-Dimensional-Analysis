# Data Science in Business - case studies
# WSB-NLU, 2023-2024
# Lecturer: Dr. Andrzej Tomski
# Author: Andrzej Kocielski, 07.06.2024
#########################################

# Projekt - prezentacja (grupy max 2-os.) na zajeciach (max 20 min.). Wymagania:
# - opis przypadku (5 pkt)
# - wybór jednej z metod wielowymiarowych z zajęć: ANOVA, regresja, analiza czynnikowa  (5 pkt)
# - przygotowanie danych do analizy (5 pkt)
# - implementacja rozwiązania (10 pkt)
# - dyskusja co do wyboru modelu (5 pkt)
# - dyskusja co do jakości modelu (5 pkt)
# - interpretacja wyników (5 pkt)
# - prezentacja na zajęciach (5 pkt)
# - wybór analizy czynnikowej (5 pkt)

# Mój plan:
# 0. sprawdzenie założeń
# 1. EFA: czy w zbiorze danych można zidentyfikować zmienne latentne
# 2. CFA: ocena jak te zmienne latentne między sobą oddziałują (miara dopasowania modelu, np. nfi (powinno być blisko 1))
# Ekstra 3. SEM: analiza strukturalna oraz analiza ścieżek
# Ocenić które z czynników mają największy efekt


library(gdata)
library(tidyverse)
library(ggplot2)
library(reshape2)
library(psych)
library(car)
library(nFactors)
library(lavaan)
library(lavaanPlot)
#install.packages("semPlot")
#devtools::install_github('SachaEpskamp/semPlot')
#library(semPlot)



##### - opis przypadku 

# Zbiór danych "Nature Relatedness Scale" zawiera wyniki ankiet psychometrycznych. Ankieta mierzy
# stopień, w jakim jednostka czuje się emocjonalnie związana z naturą. # Skala ta jest wykorzystywana
# w badaniach psychologicznych do oceny, jak relacja człowieka z naturą wpływa na jego dobrostan psychiczny.
# Zbiór danych pobrany ze strony https://openpsychometrics.org/_rawdata/ zawiera 1522 obserwacje i 63 zmienne.


##### - Pobranie, załadowanie oraz przygotowanie danych do analizy 

# Źródło: https://openpsychometrics.org/_rawdata/ - "Nature Relatedness Scale"

dane <- read.csv2("data2/data-final.csv", sep = "\t") 

# Zostawimy wybrane kolumny (wybrane arbitralnie na podstawie dokumentacji)
kols <- c("Q1A", "Q2A", "Q3A", "Q4A", "Q5A", "Q6A", 
          "TIPI1", "TIPI2", "TIPI3", "TIPI4", "TIPI5",
          "TIPI6", "TIPI7", "TIPI8", "TIPI9", "TIPI10",
          "education",
          "age",
          "familysize"
          )
dane <- select(dane, kols)

glimpse(dane)
summary(dane)

# usuwamy nie poprawne obserwacje
dane <- subset(dane, familysize != 0 & education != 0)

# liczność próby jako % całego zbioru
#licznosc_proby <- round(0.1 * nrow(dane), 0) 
#dane <- slice_sample(dane, n = licznosc_proby)


# Histogramy zmiennych
dane %>% 
  gather(key = Variable, value = Value) %>% 
  ggplot() +
  geom_histogram(aes(x = Value), bins = 20, fill = "blue") +
  facet_wrap(~Variable, scales='free') +
  theme_bw() +
  theme(aspect.ratio = 0.5, axis.title = element_blank(), panel.grid = element_blank())



##### - wybór metody analizy wielowymiarowej: analiza czynnikowa  

# Zdecydowałem się zastosować analizę czynnikową danych, ze względu na 
# - charakter danych, t.j. wyniki ankiet psychometrycznych 
# - cechy zbioru danych odpowiadające założeniom analizy czynnikowej (poniżej)


##### - dyskusja co do wyboru modelu 

# Założenia dla anzlzy czynnikowej to:
# - odpowiednia liczba obserwacji dla analizy czynnikowej (powyżej zalecanych ~100)
# - obserwacje są niezależne od siebie 
# - rozkład danych zbliżony jest do normalnego danych (w przeciwnym przypadku: estimator="MLR")
# - brak wartości znacznie odbiegających 
# - zbiór danych zakłada istnienie pewnej strukturalnej zależności teoretycznej (do potwierdzenia)


##### - przygotowanie danych do analizy 

# Sprawdzenie założeń do analizy czynnikowej

# Funkcja do normalizacji wartości kolumn do zakresu [0, 1]
znormalizuj <- function(data, columns) {
  normalize <- function(x) {
    return ((x - min(x)) / (max(x) - min(x)))
  }
  # Zastosowanie funkcji normalizacji do wybranych kolumn
  data[columns] <- lapply(data[columns], normalize)
  return(data)
}

# Normalizacja kolumn
dane <- znormalizuj(dane, c("Q1A", "Q2A", "Q3A", "Q4A", "Q5A", "Q6A",
                          "TIPI1", "TIPI2", "TIPI3", "TIPI4", "TIPI5",
                          "TIPI6", "TIPI7", "TIPI8", "TIPI9", "TIPI10"))

# Macierz korelacji zmiennych
cor_matrix <- cor(dane)
# matryca korelacji
print(cor_matrix)
# Przekształcenie macierz korelacji na format długiego formatu
melted_cor_matrix <- melt(cor_matrix)
# Mapa cieplna
ggplot(data = melted_cor_matrix, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradient2(low = "blue", high = "red", limit = c(-1,1)) +
  theme(axis.text.x = element_text(angle = 25)) +
  coord_fixed()


# Sprawdzenie adekwatności zbioru danych dla analizy czynnikowej 

# Test sferyczności Bartletta - używany do sprawdzania, czy macierz korelacji różni się istotnie od macierzy jednostkowej, co oznacza, że zmienne są wystarczająco skorelowane, aby przeprowadzić analizę czynnikową. 
cortest.bartlett(cor_matrix, nrow(dane))
# p-value < 0.05, co oznacza, że odrzucamy H0, czyli istnieje istotna korelacja między zmiennymi (korzystne dla analizy czynnikowej).


# Test KMO (Kaiser-Meyer-Olkin), zwany też (Measure of Sampling Adequacy, MSA) 
# Test KMO mierzy stosunek sumy kwadratów korelacji do sumy kwadratów korelacji częściowych, gdzie wyższe wartości wskazują na lepszą adekwatność do analizy czynnikowej.
KMO(dane)
# Zmienna z wynikiem KMO poniżej 0.50 może nie być odpowiednia do analizy czynnikowej i można rozważyć jej usunięcie.

# usuwamy nie odpowiednie zmienne, przyjmujemy takie, które < 0.6, oraz education i familysize
kols1 <- c("TIPI4", "TIPI6", "education", "familysize")
dane <- select(dane, -kols1)

# Ocena homogeniczności wariancji
ggplot(dane, aes(x = age, 
                 y = 
                     Q1A +
                     Q2A +
                     Q3A +
                     Q4A +
                     Q5A +
                     Q6A +
                     TIPI1 +
                     TIPI2 +
                     TIPI3 +
                     TIPI5 +
                     TIPI7 +
                     TIPI8 +
                     TIPI9 +
                     TIPI10 
                 )) + 
  geom_point() +
  geom_smooth(method = "lm", se = FALSE) +
  theme_minimal()

# Analiza reszt z modelu regresji liniowej
model <- lm(age ~
                  Q1A +
                  Q2A +
                  Q3A +
                  Q4A +
                  Q5A +
                  Q6A +
                  TIPI1 +
                  TIPI2 +
                  TIPI3 +
                  TIPI5 +
                  TIPI7 +
                  TIPI8 +
                  TIPI9 +
                  TIPI10 
                , data = dane)
plot(model, which = 1)  
# powinny mieć rozkład normalny, t.j. być równomiernie rozłożone wokoło zera (czerwonej linii) oraz bez outlayerów

# Obliczenie VIF dla zmiennych w modelu 
vif(model)
# dla wszystkivh zmiennych VIF < 5, czyli występuje mała współliniowość, ergo model jest OK 


##### Implementacja rozwiązania 

# EFA: czy w zbiorze danych można zidentyfikować zmienne latentne

# Sprawczamy wartości własne
eigenvalue <- eigen(cor(dane))
eigenvalue$values
# wykres osuwiska wartości własnych
plot(nScree(eigenvalue$values))
# wartości mówią ile dany czynnik wyjaśniaj wariancji zmiennych; im więcej tym lepiej; poniżej 1 - odrzucamy (zwykle)

# dobór liczby czynników metodą równoległą (są też inne metody, np. metoda demokratyczna (głosowanie większościowe))
fa.parallel(dane)

# na podstawie wykresów, przyjmuję poniżej podaną liczbę czynników


# analiza czynnikowa - budowa modelu
model1 <- factanal(dane, factors = 4, rotation = "varimax")
print(model1, cutoff=0.45, sort=FALSE)
# Uniquenesses: [0,1]; im bliżej zera tym lepiej czynnik wyjaśnia zmienną
# Loading: wkład zmiennej do budowy czynnika
# SS loading: suma ładunków podniesionych do kwadratu; większe od 1 - warto uwzględniż w modelu
# p-value < 0.05 (odrzucenie H0, że tyle czynników wystarcza) sugeruje dodanie kolejnego czynnika (ale wtedy wcale nie musi być lepiej!)


# CFA: ocena jak te zmienne latentne między sobą oddziałują (miara dopasowania modelu, np. nfi (powinno być blisko 1))

# budowa modelu
myModel <- '
    f1 =~ Q2A + Q3A + Q4A + Q5A + Q6A
    f2 =~ TIPI3 + TIPI8
    f3 =~ TIPI2
    f4 =~ TIPI1 + TIPI5
    '
# Dopasowanie modelu CFA (ML dla normalnego rozkładu; MLR dla nie podobnego do normalnego)
fit_cfa <- cfa(model = myModel, data = dane, estimator="MLR")

# Wyświetlenie wyników
summary(fit_cfa, fit.measures = TRUE, standardized = TRUE)
# p-value dla Chi-square < 0.05, może sugerować, że model słabo dopasowany (ten wskaźnik jest wrażliwy) ???
# CFI: bardzo dobre dopasowanie, gdy wartość > 0.95
# TLI: bardzo dobre dopasowanie, gdy wartość > 0.95
# RMSEA: dobre dopasowanie, gdy wartość < 0.06
# SRMR: dobre dopasowanie, gdy wartość < 0.08
# Współczynniki ładunków są statystycznie istotne, gdy p < 0.05
# Standaryzowane ładunki dla zmiennych STD.all > 0.7 sugerują silne związki z odpowiednimi czynnikami

# Wyszukanie kowariancji 
modindices(fit_cfa, sort=TRUE, maximum.number = 5)
#modindices(fit_cfa, sort=TRUE, minimum.value = 30)

# Poprawa modelu, poprzez uwzględnienie kowariancję oraz dostosowanie czynników
myModel2 <- '
    f1 =~ Q3A + Q4A + Q5A + Q6A
    f2 =~ TIPI3 + TIPI8
    f3 =~ TIPI2
    f4 =~ TIPI5
    Q5A ~~ Q6A
    '

fit_cfa2 <- cfa(model = myModel2, data = dane, estimator="MLR")
summary(fit_cfa2, fit.measures = TRUE, standardized = TRUE)
# ten model jest lepiej dopasowany


##### SEM + analiza ścieżek

# Dopasowanie modelu SEM
fit_sem <- sem(myModel2, data = dane, estimator="MLR")

# Wyświetlenie wyników
summary(fit_sem, fit.measures = TRUE, standardized = TRUE)


# Rysowanie wykresu SEM za pomocą lavaanPlot
lavaanPlot(model = fit_sem, node_options = list(shape = "box", fontname = "Helvetica"),
           edge_options = list(color = "blue"), coefs = T, stand = T)

lavaanPlot(model = fit_sem, node_options = list(shape = "box", fontname = "Helvetica"),
           edge_options = list(color = "blue"), coefs = T, stand = T, covs = T)


##### Interpretacja i wnioski - ocena które z czynników mają największy efekt
# strzałka od zmiennej ukrytej (owal) do zmiennej obserwowanej (prostokąt) sugeruje kierunek wpływu 
# wybrane zmienne dobrze opisują zjawisko (wysokie wsp. regresji przy strzałkach)
# zmienne Q3A, Q5A oraz Q6A wpływają na (są dobrze opisane przez) czynnik f1
# istnieje korelacja międzu Q5A oraz Q6A
# istnieją nieznaczne korelacje między czynnikami f1-f4 oraz znikome między pozostałymi 
# wydaje się, że pytania w ankiecie mogą być tendencyjne, ze względu na korelację między odpowiedziami
#  oraz na rozkład odpowiedzi (nie normalny) 
