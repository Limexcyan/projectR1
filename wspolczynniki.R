library(patchwork)
library(dplyr)
library(tidyr)

results <- data.frame(read.csv("results.csv"))
shootouts <- data.frame(read.csv("shootouts.csv"))
goalscorers <- data.frame(read.csv("goalscorers.csv"))

results$goals_dif <- abs(results$home_score - results$away_score)
results$goals_sum <- results$home_score + results$away_score
results$year <- as.numeric(format(as.Date(results$date), "%Y"))
results$goals_quot <- results$goals_dif / results$goals_sum

average_goals_per_year <- aggregate(goals_sum ~ year, data = results, FUN = mean)
average_dif_goals_per_year <- aggregate(goals_dif ~ year, data = results, FUN=mean)
average_quot_goals_per_year <- aggregate(goals_quot ~ year, data = results, FUN=mean)

ggplot(average_goals_per_year, aes(x = year, y = goals_sum)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red") +
  labs(
    title = "Średnia liczba goli na mecz w poszczególnych latach",
    x = "Rok",
    y = "Średnia liczba goli na mecz"
  ) +
  theme_minimal()

ggsave("images/mecze_suma.png", plot = last_plot(), width = 8, height = 6, dpi = 300)

ggplot(average_dif_goals_per_year, aes(x = year, y = goals_dif)) + 
  geom_line(color = "green", size = 1) + 
  geom_point(color = "red") +
  labs(
    title = "asfsdf",
    x = "Rok",
    t = "Średnia różnica bramkowa w meczu"
  ) + 
  theme_minimal()

ggsave("images/mecze_roznica.png", plot = last_plot(), width = 8, height = 6, dpi = 300)

# Średni współczynnik ilorazu różnicy przez sumę
ggplot(average_quot_goals_per_year, aes(x = year, y = goals_quot)) + 
  geom_line(color = "green", size = 1) + 
  geom_point(color = "red") +
  labs(
    title = "asfsdf",
    x = "Rok",
    t = "Średnia różnica bramkowa w meczu w stosunku do sumy bramek"
  ) + 
  theme_minimal()


ggsave("images/mecze_wspolczynnik.png", plot = last_plot(), width = 8, height = 6, dpi = 300)


combined_data <- data.frame(
  year = average_goals_per_year$year[40:152],
  goals_sum = average_goals_per_year$goals_sum[40:152],
  goals_dif = average_dif_goals_per_year$goals_dif[40:152],
  goals_quot = average_quot_goals_per_year$goals_quot[40:152]
)

combined_data_long <- combined_data %>%
  gather(key = "metric", value = "value", -year)

ggplot(combined_data_long, aes(x = year, y = value, fill = metric)) +
  geom_area(alpha = 0.6) +
  scale_fill_manual(values = c("blue", "green", "red")) +
  labs(
    title = "Zmienność wskaźników w czasie",
    x = "Rok",
    y = "Wartość wskaźnika",
    fill = "Wskaźnik"
  ) +
  theme_minimal() +
  theme(legend.position = "top")


ggsave("images/area_chart_wskaźniki.png", width = 10, height = 6, dpi = 300)





results <- data.frame(read.csv("results.csv"))
results$goals_dif <- abs(results$home_score - results$away_score)
results$goals_sum <- results$home_score + results$away_score
results$year <- as.numeric(format(as.Date(results$date), "%Y"))
results$goals_quot <- results$goals_dif / results$goals_sum

average_goals_per_year <- aggregate(goals_sum ~ year, data = results, FUN = mean)
average_dif_goals_per_year <- aggregate(goals_dif ~ year, data = results, FUN = mean)
average_quot_goals_per_year <- aggregate(goals_quot ~ year, data = results, FUN = mean)

plot1 <- ggplot(average_goals_per_year, aes(x = year, y = goals_sum)) +
  geom_line(color = "blue", size = 1) +
  labs(
    title = "Średnia liczba goli na mecz w poszczególnych latach",
    x = "Rok",
    y = "Średnia liczba goli na mecz"
  ) +
  theme_minimal()

plot2 <- ggplot(average_dif_goals_per_year, aes(x = year, y = goals_dif)) +
  geom_line(color = "green", size = 1) +
  labs(
    title = "Średnia różnica bramkowa w meczu",
    x = "Rok",
    y = "Średnia różnica bramkowa"
  ) +
  theme_minimal()

plot3 <- ggplot(average_quot_goals_per_year, aes(x = year, y = goals_quot)) +
  geom_line(color = "purple", size = 1) +
  labs(
    title = "Średni iloraz różnicy do sumy bramek",
    x = "Rok",
    y = "Iloraz różnicy do sumy bramek"
  ) +
  theme_minimal()

combined_plot <- plot1 / plot2 / plot3

print(combined_plot)

ggsave("images/combined_indeces.png", plot = combined_plot, width = 10, height = 12, dpi = 300)







results <- data.frame(read.csv("results.csv"))
results$goals_dif <- abs(results$home_score - results$away_score)
results$goals_sum <- results$home_score + results$away_score
results$year <- as.numeric(format(as.Date(results$date), "%Y"))
results$goals_quot <- results$goals_dif / results$goals_sum

results <- results %>% filter(year >= max(year) - 25)

average_goals_per_year <- aggregate(goals_sum ~ year, data = results, FUN = mean)
average_dif_goals_per_year <- aggregate(goals_dif ~ year, data = results, FUN = mean)
average_quot_goals_per_year <- aggregate(goals_quot ~ year, data = results, FUN = mean)

plot1 <- ggplot(average_goals_per_year, aes(x = year, y = goals_sum)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red") +
  geom_smooth(method = "lm", color = "black", linetype = "dashed") +
  labs(
    title = "Średnia liczba goli na mecz w poszczególnych latach",
    x = "Rok",
    y = "Średnia liczba goli na mecz"
  ) +
  theme_minimal()

plot2 <- ggplot(average_dif_goals_per_year, aes(x = year, y = goals_dif)) +
  geom_line(color = "green", size = 1) +
  geom_point(color = "red") +
  geom_smooth(method = "lm", color = "black", linetype = "dashed") +
  labs(
    title = "Średnia różnica bramkowa w meczu",
    x = "Rok",
    y = "Średnia różnica bramkowa"
  ) +
  theme_minimal()

plot3 <- ggplot(average_quot_goals_per_year, aes(x = year, y = goals_quot)) +
  geom_line(color = "purple", size = 1) +
  geom_point(color = "red") +
  geom_smooth(method = "lm", color = "black", linetype = "dashed") +
  labs(
    title = "Średni iloraz różnicy do sumy bramek",
    x = "Rok",
    y = "Iloraz różnicy do sumy bramek"
  ) +
  theme_minimal()

combined_plot <- plot1 / plot2 / plot3

print(combined_plot)

ggsave("images/combined_recent_indeces.png", plot = combined_plot, width = 10, height = 12, dpi = 300)
