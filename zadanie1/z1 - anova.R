library(tidyverse)
library(readr)

# Zadanie: Wczytać dane ze zbioru danych z zestawu 1 i odpowiedzieć na pytanie (ANOVA), czy jest istotna różnica w wynikach G1 między Fjob (praca) i Reason (powód udziału w teście).


#student <- readr::read_csv('student-mat.csv')
student <- read_csv2('student-mat.csv')
head(student)
summary(student$G1)

# Test T studenta dla zmiennej 'Fjob'
# dplyr::sample_n(tbl=student, size=10)$Fjob
# ggplot(student, aes(x=Fjob, y=G3)) + geom_point(aes(color=Fjob), position='jitter') + labs(title = 'Wykres punktowy wyników końcowych z podziałem na szkoły', x='Fjob', y='Wynik końcowy') + theme(legend.position = 'none')
# ggplot(student, aes(x=Fjob, y=G1)) + geom_boxplot() + labs(title = "Wykres pudełkowy wyników końcowych w zależności od szkoły", x='Fjob', y='Wynik końcowy')
# t.test(G1 ~ Fjob, data=student, var.equal=FALSE, alternative='two.sided')

# Jednoczynnikowa Analiza Wariancji - mówi czy są statystyczne różnice (ale nie mówi które grupy są różne statystyczne - do tego analiza post hoc Tukeya-Kramera)
summary(aov(G1 ~ Fjob, data=student))
student <- student %>% dplyr::mutate(Fjob = as.character(Fjob))
ggplot(student, aes(x=Fjob, y=G1)) + geom_point(aes(color=Fjob), position='jitter') + labs(title = 'Wykres punktowy wyników końcowych z podziałem na grupy', x='Fjob', y='Wynik końcowy') + theme(legend.position = 'none')
ggplot(student, aes(x=Fjob, y=G1)) + geom_boxplot() + labs(title = "Wykres pudełkowy wyników końcowych z podziałem na grupy", x='Fjob', y='Wynik końcowy')

summary(aov(G1 ~ reason, data=student))
student <- student %>% dplyr::mutate(reason = as.character(reason))
ggplot(student, aes(x=reason, y=G1)) + geom_point(aes(color=reason), position='jitter') + labs(title = 'Wykres punktowy wyników końcowych z podziałem na grupy', x='reason', y='Wynik końcowy') + theme(legend.position = 'none')
ggplot(student, aes(x=reason, y=G1)) + geom_boxplot() + labs(title = "Wykres pudełkowy wyników końcowych z podziałem na grupy", x='reason', y='Wynik końcowy')

# Test normalności rozkładu reszt (residuals)
# Model regresji liniowej dla zmiennej 'Fjob'
model <- lm(G1 ~ Fjob, data=student)
# Pobieramy reszty z modelu
reszty <- residuals(model)
# Test Shapiro-Wilka; mała p-wartość oznacza odrzucenie hipotezy o normalności (?)
shapiro.test(reszty)
# Histogram
hist(reszty)


# Analiza Tukeya-Kramera - mówi które czynniki są statystycznie rózne
student <- student %>% dplyr::mutate(freetime = as.character(freetime))
summary(aov(G1 ~ Fjob, data = student)) # dla zmiennej 'Fjob'
TukeyHSD(aov(G1 ~ Fjob, data = student))
plot(TukeyHSD(aov(G1 ~ Fjob, data = student)))


