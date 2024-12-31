results <- data.frame(read.csv("results.csv"))
shootouts <- data.frame(read.csv("shootouts.csv"))
goalscorers <- data.frame(read.csv("goalscorers.csv"))

print(head(results, 10))
print(head(shootouts, 10))
print(head(goalscorers, 10))

print(colnames(results))
print(colnames(shootouts))
print(colnames(goalscorers))

goalscorers$decade <- floor(as.numeric(format(as.Date(goalscorers$date), "%Y")) / 10) * 10

valid_goals <- goalscorers[goalscorers$own_goal == 0, ]

global_ranking <- aggregate(
  valid_goals$minute, 
  by = list(scorer = valid_goals$scorer), 
  FUN = length
)
colnames(global_ranking) <- c("scorer", "total_goals")
global_ranking <- global_ranking[order(global_ranking$total_goals, decreasing = TRUE), ]
global_ranking <- global_ranking[1:10, ]

decade_rankings <- aggregate(
  valid_goals$minute,
  by = list(decade = valid_goals$decade, scorer = valid_goals$scorer),
  FUN = length
)
colnames(decade_rankings) <- c("decade", "scorer", "total_goals")
decade_rankings <- decade_rankings[order(decade_rankings$decade, -decade_rankings$total_goals), ]

unique_decades <- unique(decade_rankings$decade)
top_3_per_decade <- data.frame()

for (dec in unique_decades) {
  top_3 <- head(decade_rankings[decade_rankings$decade == dec, ], 3)
  top_3_per_decade <- rbind(top_3_per_decade, top_3)
}

cat("Globalny ranking 10 najlepszych strzelców:\n")
print(global_ranking)

cat("\nTop 3 strzelcy dla każdej dekady:\n")
print(top_3_per_decade)

own_goals <- goalscorers[goalscorers$own_goal == 1, ]

own_goals_ranking <- aggregate(
  own_goals$minute, 
  by = list(scorer = own_goals$scorer), 
  FUN = length
)
colnames(own_goals_ranking) <- c("scorer", "own_goals_count")

own_goals_ranking <- own_goals_ranking[order(own_goals_ranking$own_goals_count, decreasing = TRUE), ]

top_own_goal_scorer <- head(own_goals_ranking, 10)

cat("Zawodnik, który zdobył najwięcej samobójów:\n")
print(top_own_goal_scorer)


valid_goals <- goalscorers[goalscorers$minute <= 90, ]

library(ggplot2)

ggplot(valid_goals, aes(x = minute)) +
  geom_density(fill = "blue", alpha = 0.5) +
  labs(
    title = "Gęstość minut, w których padają bramki",
    x = "Minuta meczu",
    y = "Gęstość"
  ) +
  theme_minimal()

ggsave("minuty_gestosc.png", plot = plot, width = 8, height = 6, dpi = 300)



