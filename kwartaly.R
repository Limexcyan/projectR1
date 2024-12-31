library(ggplot2)

# wczytanie danych 
results <- data.frame(read.csv("results.csv"))
shootouts <- data.frame(read.csv("shootouts.csv"))
goalscorers <- data.frame(read.csv("goalscorers.csv"))


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

ggsave("images/minuty_gestosc_kwartaly_overall.png", plot = last_plot(), width = 8, height = 6, dpi = 300)

# sacasdcsd
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

ggsave("images/minuty_gestosc_kwartaly_dekady.png", plot = last_plot(), width = 8, height = 6, dpi = 300)
