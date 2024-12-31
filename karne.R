library(ggplot2)

goalscorers <- data.frame(read.csv("goalscorers.csv"))

goalscorers$year <- as.numeric(format(as.Date(goalscorers$date), "%Y"))

valid_goals <- goalscorers[goalscorers$minute <= 90 & goalscorers$year >= 1992, ]

valid_goals$Goal = 1

goal_counts <- aggregate(cbind(Goal, penalty) ~ year, data = valid_goals, sum)

colnames(goal_counts)[colnames(goal_counts) == "penalty"] <- "PenaltyGoal"

goal_counts$PenaltyRatio <- goal_counts$PenaltyGoal / goal_counts$Goal

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

ggsave("images/karne_rok_do_roku.png", plot = last_plot(), width = 8, height = 6, dpi = 300)
