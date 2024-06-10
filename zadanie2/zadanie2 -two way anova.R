# Andrzej Kocielski, 08.05.2024

# Zadanie 2 (MANOVA): W pliku ‘halo1.dat’ znajdują się dane pochodzące z badania na temat efektu aureoli. 
# Mamy dwa czynniki - “jakość eseju” i “atrakcyjność autora” i jedną zmienną objaśnianą - “ocenę”.
# Eseje dzielimy na “dobre - 1” i “złe - 2”, atrakcyjność dzielimy na “1 - wysoką, 2 - grupa kontrolna, 3 - niewysoką”.
# Przy pomocy analizy wariancji stwierdzić czy czynniki mają wypływ na wynik. 

library(tidyverse)
library(readr)


halo <- readr::read_table('halo1.dat', col_names = c('jakosc', 'atrakcyjnosc', 'ocena'))
halo <- halo %>% dplyr::mutate(jakosc = as.factor(jakosc)) %>% dplyr::mutate(atrakcyjnosc = as.factor(atrakcyjnosc))
head(halo)

# 1. Zaczniemy od jednoczynnikowej analizy wariancji
# H0: nie ma istotnych różnic w ocenach w zależności od (pojedynczego) czynnika; H1: istnieją różnice.
par(mfrow=c(1,2))
boxplot(ocena ~ jakosc, col=unique(as.numeric(halo$jakosc))+1, main="ocena", data=halo)
boxplot(ocena ~ atrakcyjnosc, col=unique(as.numeric(halo$atrakcyjnosc))+1, main="atrakcyjnosc", data=halo)
# Z wykresów pudełkowych widzimy, że istnieją różnice między średnimi zarówno dla jakości oraz atrakcyjności.
summary(halo_aov1 <- aov(ocena ~ jakosc + atrakcyjnosc, data = halo))
# Wniosek: W obu przypadkach odrzucamy H0 na korzyść H1, t.j. czynniki mają statystycznie istotny wpływ na ocenę eseju (ponieważ p-value < 0.05)


# 2. Wieloczynnikowa analiza wariancji, ANOVA two-way
#H0: Nie ma istotnej interakcji jmiędzy akością eseju i atrakcyjnością autora; H1: istnieją tak interakcja.
summary(halo_aov2 <- aov(ocena ~ jakosc * atrakcyjnosc, data = halo))
# Wniosek: Nie ma podstaw do odrzucenia H0, tj. nie ma istotnej interakcji między jakością eseju a atrakcyjnością autora (ponieważ p-value > 0.05).


# 3. Analiza post-hoc Tukeya - mówi, które czynniki są statystycznie istotne (źródło: https://rpubs.com/aaronsc32/post-hoc-analysis-tukey)
?TukeyHSD
TukeyHSD(halo_aov2, which="jakosc:atrakcyjnosc")
par(mfrow=c(1,1))
plot(TukeyHSD(halo_aov2, which="jakosc:atrakcyjnosc"))

# Podsumowując, jakość eseju i atrakcyjność autora mają istotny wpływ na oceny, ale nie ma istotnej interakcji między nimi.
