library(ggplot2)
library(dplyr)

quality_tournaments <- c(
  "UEFA Euro", "African Cup of Nations qualification",
  "CONCACAF Championship", "CONCACAF Championship qualification", "CONCACAF Nations League", 
  "UEFA Nations League", "Afcica Cup of Nations qualifications", 
  "UEFA Euro Qualification", "Copa América qualification",
  "Copa América", "AFC Asian Cup", "AFC Asian Cup qualification"
)
quality_results <- results[results$tournament %in% quality_tournaments, ]

quality_results$year <- as.numeric(format(as.Date(quality_results$date, format = "%Y-%m-%d"), "%Y"))
quality_results$cycle <- floor((quality_results$year - 1) / 4) * 4 + 1

matches_per_cycle <- quality_results %>%
  group_by(cycle, tournament) %>%
  summarise(matches = n(), .groups = 'drop')

federation_mapping <- list(
  UEFA = c("UEFA Nations League", "UEFA Euro", "UEFA Euro qualification"),
  CONCACAF = c("CONCACAF Nations League", "CONCACAF Championship", "CONCACAF Championship qualification"),
  CAF = c("African Cup of Nations qualification", "Africa Cup of Nations qualification"),
  FIFA = c("FIFA World Cup", "FIFA World Cup qualification"),
  AFC = c("AFC Asian Cup", "AFC Asian Cup qualification"),
  CONMEBOL = c("Copa América qualification", "Copa América")
)

quality_results <- quality_results %>%
  mutate(federation = case_when(
    tournament %in% federation_mapping$UEFA ~ "UEFA",
    tournament %in% federation_mapping$CONCACAF ~ "CONCACAF",
    tournament %in% federation_mapping$CAF ~ "CAF",
    tournament %in% federation_mapping$FIFA ~ "FIFA",
    tournament %in% federation_mapping$AFC ~ "AFC",
    tournament %in% federation_mapping$CONMEBOL ~ "CONMEBOL",
    TRUE ~ "Other"
  ))

matches_per_federation <- quality_results %>%
  group_by(cycle, federation) %>%
  summarise(matches = n(), .groups = 'drop')

ggplot(matches_per_federation, aes(x = cycle, y = matches, color = federation, group = federation)) +
  geom_line(size = 1) +
  labs(
    title = "Liczba spotkań na cykl czteroletni dla poszczególnych federacji",
    x = "Lata (pierwszy rok cyklu)",
    y = "Liczba spotkań",
    color = "Federacja"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 10)
  )

print(federation_plot)

ggsave("images/zmiana_liczby_spotkan_federacje.png", plot = last_plot(), width = 12, height = 8, dpi = 300)



max_matches_per_federation <- matches_per_federation %>%
  group_by(federation) %>%
  summarise(max_matches = max(matches))

matches_per_federation <- matches_per_federation %>%
  left_join(max_matches_per_federation, by = "federation") %>%
  mutate(scaled_matches = (matches / max_matches) * 100)

ggplot(matches_per_federation, aes(x = cycle, y = scaled_matches, color = federation, group = federation)) +
  geom_line(size = 1) +
  labs(
    title = "Zmiana liczby spotkań w cyklach czteroletnich (przeskalowane do 100%) dla poszczególnych federacji",
    x = "Początek cyklu czteroletniego",
    y = "Liczba spotkań (procentowo)",
    color = "Federacja"
  ) +
  theme_minimal() +
  theme(
    text = element_text(size = 12),
    legend.position = "bottom",
    legend.title = element_text(face = "bold"),
    legend.text = element_text(size = 10)
  ) +
  scale_color_brewer(palette = "Set3")


ggsave("images/zmiana_liczby_spotkan_federacje_scaled.png", plot = last_plot(), width = 12, height = 8, dpi = 300)