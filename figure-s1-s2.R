library(tidyverse)
library(sve)

load("simulation_coverage.rda")
load("wald_results.rda")
load("wald_correct_results.rda")

result <- out |>
  mutate(
    sve_hat_wald = wald_results$estimate,
    se_hat_wald = (wald_results$upper - wald_results$estimate) / qnorm(0.975),
    sve_hat_wald_correct = wald_correct_results$estimate,
    se_hat_wald_correct = (wald_correct_results$upper - wald_correct_results$estimate) / qnorm(0.975),
    sve = sve:::sve(p0, p1, n0, n1, correction = FALSE)
  )

result_summary <- result |>
  filter(x0 != 0, x1 != 0) |>
  group_by(p0, p1, n0, n1, sve) |>
  summarise(
    bias = mean(sve_hat_wald - sve),
    bias_corrected = mean(sve_hat_wald_correct - sve),
    empirical_se = sd(sve_hat_wald),
    empirical_se_correct = sd(sve_hat_wald_correct),
    estimated_se = mean(se_hat_wald),
    estimated_se_correct = mean(se_hat_wald_correct),
    ratio = estimated_se / empirical_se,
    ratio_correct = estimated_se_correct / empirical_se_correct,
    .groups = "drop"
  )

result_summary |>
  summarise(min(bias), max(bias))

result_summary |>
  ggplot(aes(x = p0, y = p1, fill = bias)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "cornflower blue", high = "orange", mid = "white",
    midpoint = 0, name = "Bias", limits = c(-0.03, 0.03)
  ) +
  facet_grid(n0 ~ n1, labeller = label_both) +
  labs(
    x = expression(p[0]),
    y = expression(p[1])
  ) +
  theme_minimal(base_size = 12) + 
  theme(
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "gray95", color = NA)
  )

ggsave("fig-s1-a.png", width = 6, height = 4, dpi = 300)

result_summary |>
  ggplot(aes(x = p0, y = p1, fill = bias_corrected)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "cornflower blue", high = "orange", mid = "white",
    midpoint = 0, name = "Bias", limits = c(-0.03, 0.03)
  ) +
  facet_grid(n0 ~ n1, labeller = label_both) +
  labs(
    x = expression(p[0]),
    y = expression(p[1])
  ) +
  theme_minimal(base_size = 12) + 
  theme(
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "gray95", color = NA)
  )

ggsave("fig-s1-b.png", width = 6, height = 4, dpi = 300)

result_summary |>
  ggplot(aes(x = p0, y = p1, fill = estimated_se/empirical_se)) +
  geom_tile() +
  scale_fill_gradient2(
    low = "cornflower blue", high = "orange", mid = "white",
    midpoint = 1, name = "Estimated SE/\nEmpirical SE",
    limits = c(0.93, 1.07)
  ) +
  facet_grid(n0 ~ n1, labeller = label_both) +
  labs(
    x = expression(p[0]),
    y = expression(p[1])
  ) +
  theme_minimal(base_size = 12) + 
  theme(
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "gray95", color = NA)
  )

ggsave("fig-s2.png", width = 7, height = 4, dpi = 300)
