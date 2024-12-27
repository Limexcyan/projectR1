# wczytanie danych 
results <- data.frame(read.csv("results.csv"))
shootouts <- data.frame(read.csv("shootouts.csv"))
goalscorers <- data.frame(read.csv("goalscorers.csv"))

# zobaczenie jak wyglądają dane
print(head(results, 10))
print(head(shootouts, 10))
print(head(goalscorers, 10))

# print(colnames(results))
# print(colnames(shootouts))
print(colnames(goalscorers))

goalscorers$decade <- floor(as.numeric(format(as.Date(goalscorers$date), "%Y")) / 10) * 10

# Filtrowanie tylko prawidłowych goli (bez samobójów)
valid_goals <- goalscorers[goalscorers$own_goal == 0, ]

# Globalny ranking 10 najlepszych strzelców
global_ranking <- aggregate(
  valid_goals$minute, 
  by = list(scorer = valid_goals$scorer), 
  FUN = length
)
colnames(global_ranking) <- c("scorer", "total_goals")
global_ranking <- global_ranking[order(global_ranking$total_goals, decreasing = TRUE), ]
global_ranking <- global_ranking[1:10, ]

# # Rankingi dla każdej dekady
# decade_rankings <- aggregate(
#   valid_goals$minute, 
#   by = list(decade = valid_goals$decade, scorer = valid_goals$scorer), 
#   FUN = length
# )
# colnames(decade_rankings) <- c("decade", "scorer", "total_goals")
# decade_rankings <- decade_rankings[order(decade_rankings$decade, -decade_rankings$total_goals), ]
# 
# # Wyłonienie top 3 dla każdej dekady
# unique_decades <- unique(decade_rankings$decade)
# top_3_per_decade <- data.frame()
# 
# for (dec in unique_decades) {
#   top_3 <- head(decade_rankings[decade_rankings$decade == dec, ], 3)
#   top_3_per_decade <- rbind(top_3_per_decade, top_3)
# }

# Wyświetlanie wyników
cat("Globalny ranking 10 najlepszych strzelców:\n")
print(global_ranking)

# cat("\nTop 3 strzelcy dla każdej dekady:\n")
# print(top_3_per_decade)

# Filtrowanie tylko samobójów
own_goals <- goalscorers[goalscorers$own_goal == 1, ]

# Liczba samobójów na zawodnika
own_goals_ranking <- aggregate(
  own_goals$minute, 
  by = list(scorer = own_goals$scorer), 
  FUN = length
)
colnames(own_goals_ranking) <- c("scorer", "own_goals_count")

# Sortowanie wyników malejąco po liczbie samobójów
own_goals_ranking <- own_goals_ranking[order(own_goals_ranking$own_goals_count, decreasing = TRUE), ]

# Najwięcej samobójów
top_own_goal_scorer <- head(own_goals_ranking, 10)

# Wyświetlenie wyników
cat("Zawodnik, który zdobył najwięcej samobójów:\n")
print(top_own_goal_scorer)


# Filtrowanie prawidłowych goli (bez samobójów)
valid_goals <- goalscorers[goalscorers$minute <= 90, ]

# Instalacja i załadowanie ggplot2, jeśli nie jest dostępne
if (!require(ggplot2)) install.packages("ggplot2", dependencies = TRUE)
library(ggplot2)

# Tworzenie wykresu gęstości
ggplot(valid_goals, aes(x = minute)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(
    title = "Gęstość minut, w których padają bramki",
    x = "Minuta meczu",
    y = "Gęstość"
  ) +
  theme_minimal()





# Filtrowanie prawidłowych goli (bez samobójów) w regulaminowym czasie gry
valid_goals <- goalscorers[goalscorers$minute <= 90, ]

# Dodanie kolumny wskazującej połowę meczu
valid_goals$half <- ifelse(valid_goals$minute <= 45, "Pierwsza połowa", "Druga połowa")

# Liczenie goli w każdej połowie
half_goals <- table(valid_goals$half)
half_goals_df <- as.data.frame(half_goals)
colnames(half_goals_df) <- c("Half", "Goals")

# Instalacja i załadowanie ggplot2, jeśli nie jest dostępne
if (!require(ggplot2)) install.packages("ggplot2", dependencies = TRUE)
library(ggplot2)

# Tworzenie wykresu słupkowego
ggplot(half_goals_df, aes(x = Half, y = Goals, fill = Half)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(
    title = "Porównanie liczby goli w pierwszej i drugiej połowie",
    x = "Połowa meczu",
    y = "Liczba goli"
  ) +
  scale_fill_manual(values = c("Pierwsza połowa" = "blue", "Druga połowa" = "green")) +
  theme_minimal()













# Dodanie kolumny dekady i połowy meczu
goalscorers$decade <- floor(as.numeric(format(as.Date(goalscorers$date), "%Y")) / 10) * 10
goalscorers$half <- ifelse(goalscorers$minute <= 45, "Pierwsza połowa", 
                           ifelse(goalscorers$minute <= 90, "Druga połowa", NA))

# Filtrowanie prawidłowych goli w regulaminowym czasie gry
valid_goals <- goalscorers[goalscorers$minute <= 90, ]

# Liczenie goli dla każdej dekady i połowy
trend_data <- as.data.frame(table(valid_goals$decade, valid_goals$half))
colnames(trend_data) <- c("Decade", "Half", "Goals")

# Obliczanie proporcji goli w ramach każdej dekady
total_goals_per_decade <- aggregate(Goals ~ Decade, data = trend_data, sum)
trend_data <- merge(trend_data, total_goals_per_decade, by = "Decade", suffixes = c("", "_Total"))
trend_data$Proportion <- trend_data$Goals / trend_data$Goals_Total

# Instalacja i załadowanie ggplot2, jeśli nie jest dostępne
if (!require(ggplot2)) install.packages("ggplot2", dependencies = TRUE)
library(ggplot2)

# Tworzenie wykresu proporcji
ggplot(trend_data, aes(x = Decade, y = Proportion, fill = Half)) +
  geom_bar(stat = "identity", position = "fill", width = 0.7) +
  labs(
    title = "Proporcja goli w pierwszej i drugiej połowie na przestrzeni dekad",
    x = "Dekada",
    y = "Proporcja goli",
    fill = "Połowa meczu"
  ) +
  scale_fill_manual(values = c("Pierwsza połowa" = "blue", "Druga połowa" = "green")) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()





# Dodanie kolumny dekady
goalscorers$decade <- floor(as.numeric(format(as.Date(goalscorers$date), "%Y")) / 10) * 10

# Dodanie kolumny określającej kwadrans
goalscorers$quarter <- cut(
  goalscorers$minute,
  breaks = c(0, 15, 30, 45, 60, 75, 90),
  labels = c("1-15", "16-30", "31-45", "46-60", "61-75", "76-90"),
  right = TRUE
)

# Filtrowanie prawidłowych goli w regulaminowym czasie gry
valid_goals <- goalscorers[goalscorers$minute <= 90, ]

# Liczenie goli dla każdego kwadransa
quarter_goals <- as.data.frame(table(valid_goals$quarter))
colnames(quarter_goals) <- c("Quarter", "Goals")

# Liczenie goli dla każdej dekady i kwadransu
trend_data <- as.data.frame(table(valid_goals$decade, valid_goals$quarter))
colnames(trend_data) <- c("Decade", "Quarter", "Goals")

# Obliczanie proporcji goli w każdym kwadransie
total_goals <- sum(quarter_goals$Goals)
quarter_goals$Proportion <- quarter_goals$Goals / total_goals

# Obliczanie proporcji goli w ramach każdej dekady
total_goals_per_decade <- aggregate(Goals ~ Decade, data = trend_data, sum)
trend_data <- merge(trend_data, total_goals_per_decade, by = "Decade", suffixes = c("", "_Total"))
trend_data$Proportion <- trend_data$Goals / trend_data$Goals_Total


# Instalacja i załadowanie ggplot2, jeśli nie jest dostępne
if (!require(ggplot2)) install.packages("ggplot2", dependencies = TRUE)
library(ggplot2)

# Tworzenie wykresu proporcji
ggplot(quarter_goals, aes(x = Quarter, y = Proportion, fill = Quarter)) +
  geom_bar(stat = "identity", width = 0.7) +
  labs(
    title = "Proporcje goli strzelonych w różnych kwadransach meczu",
    x = "Kwadrans meczu",
    y = "Proporcja goli",
    fill = "Kwadrans"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal() +
  theme(legend.position = "none")



ggplot(trend_data, aes(x = Decade, y = Proportion, fill = Quarter)) +
  geom_bar(stat = "identity", position = "fill", width = 0.7) +
  labs(
    title = "Proporcja goli w w danych kwadransach na przestrzeni dekad",
    x = "Dekada",
    y = "Proporcja goli",
    fill = "kwadrans meczu"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()




# Przekształcenie daty na rok
goalscorers$year <- as.numeric(format(as.Date(goalscorers$date), "%Y"))

# Filtrowanie goli w regulaminowym czasie gry (do 90. minuty) i od 2004 roku
valid_goals <- goalscorers[goalscorers$minute <= 90 & goalscorers$year >= 1992, ]

# Tworzenie nowych kolumn: 1 dla każdego gola ogółem
valid_goals$Goal = 1

# Liczenie goli ogółem oraz goli z rzutów karnych w każdym roku
goal_counts <- aggregate(cbind(Goal, penalty) ~ year, data = valid_goals, sum)

# Zmiana nazwy kolumny 'penalty' na 'PenaltyGoal' dla lepszej czytelności
colnames(goal_counts)[colnames(goal_counts) == "penalty"] <- "PenaltyGoal"

# Obliczanie stosunku karnych do wszystkich goli
goal_counts$PenaltyRatio <- goal_counts$PenaltyGoal / goal_counts$Goal

# Tworzenie wykresu
ggplot(goal_counts, aes(x = year, y = PenaltyRatio)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red") +
  labs(
    title = "Stosunek goli z rzutów karnych do wszystkich goli (rok do roku)",
    x = "Rok",
    y = "Stosunek goli z karnych do wszystkich goli"
  ) +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()
