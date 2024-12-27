# wczytanie danych 
results <- data.frame(read.csv("results.csv"))
shootouts <- data.frame(read.csv("shootouts.csv"))
goalscorers <- data.frame(read.csv("goalscorers.csv"))

# zobaczenie jak wyglądają dane
print(head(results, 10))
print(head(shootouts, 10))

print(colnames(results))
print(unique(results$tournament))
print(colnames(shootouts))

results$goals_dif <- abs(results$home_score - results$away_score)
results$goals_sum <- results$home_score + results$away_score
results$year <- as.numeric(format(as.Date(results$date), "%Y"))
results$goals_quot <- results$goals_dif / results$goals_sum

# Obliczamy średnią liczbę goli na mecz w poszczególnych latach
average_goals_per_year <- aggregate(goals_sum ~ year, data = results, FUN = mean)
average_dif_goals_per_year <- aggregate(goals_dif ~ year, data = results, FUN=mean)
average_quot_goals_per_year <- aggregate(goals_quot ~ year, data = results, FUN=mean)

# Tworzenie wykresu
ggplot(average_goals_per_year, aes(x = year, y = goals_sum)) +
  geom_line(color = "blue", size = 1) +
  geom_point(color = "red") +
  labs(
    title = "Średnia liczba goli na mecz w poszczególnych latach",
    x = "Rok",
    y = "Średnia liczba goli na mecz"
  ) +
  theme_minimal()

ggplot(average_dif_goals_per_year, aes(x = year, y = goals_dif)) + 
  geom_line(color = "green", size = 1) + 
  geom_point(color = "red") +
  labs(
    title = "asfsdf",
    x = "Rok",
    t = "Średnia różnica bramkowa w meczu"
  ) + 
  theme_minimal()


ggplot(average_quot_goals_per_year, aes(x = year, y = goals_quot)) + 
  geom_line(color = "green", size = 1) + 
  geom_point(color = "red") +
  labs(
    title = "asfsdf",
    x = "Rok",
    t = "Średnia różnica bramkowa w meczu w stosunku do sumy bramek"
  ) + 
  theme_minimal()


