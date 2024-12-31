library(ggplot2)
goalscorers <- data.frame(read.csv("goalscorers.csv"))
valid_goals <- goalscorers[goalscorers$minute <= 90, ]

ggplot(valid_goals, aes(x = minute)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(
    title = "Gęstość minut, w których padają bramki",
    x = "Minuta meczu",
    y = "Gęstość"
  ) +
  theme_minimal()

ggsave("images/minuty_gestosc_podstawa.png", plot = last_plot(), width = 8, height = 6, dpi = 300)


valid_goals$half <- ifelse(valid_goals$minute <= 45, "Pierwsza połowa", "Druga połowa")

half_goals <- table(valid_goals$half)
half_goals_df <- as.data.frame(half_goals)
colnames(half_goals_df) <- c("Half", "Goals")

ggplot(half_goals_df, aes(x = Half, y = Goals, fill = Half)) +
  geom_bar(stat = "identity", width = 0.5) +
  labs(
    title = "Porównanie liczby goli w pierwszej i drugiej połowie",
    x = "Połowa meczu",
    y = "Liczba goli"
  ) +
  scale_fill_manual(values = c("Pierwsza połowa" = "blue", "Druga połowa" = "green")) +
  theme_minimal()

ggsave("images/minuty_gestosc_polowy.png", plot = last_plot(), width = 8, height = 6, dpi = 300)


goalscorers$decade <- floor(as.numeric(format(as.Date(goalscorers$date), "%Y")) / 10) * 10
goalscorers$half <- ifelse(goalscorers$minute <= 45, "Pierwsza połowa", 
                           ifelse(goalscorers$minute <= 90, "Druga połowa", NA))

valid_goals <- goalscorers[goalscorers$minute <= 90, ]

trend_data <- as.data.frame(table(valid_goals$decade, valid_goals$half))
colnames(trend_data) <- c("Decade", "Half", "Goals")

total_goals_per_decade <- aggregate(Goals ~ Decade, data = trend_data, sum)
trend_data <- merge(trend_data, total_goals_per_decade, by = "Decade", suffixes = c("", "_Total"))
trend_data$Proportion <- trend_data$Goals / trend_data$Goals_Total

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

ggsave("images/minuty_gestosc_dekady_polowy.png", plot = last_plot(), width = 8, height = 6, dpi = 300)
