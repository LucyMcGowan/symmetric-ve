library(tidyverse)
library(sve)

load("simulation_coverage.rda")
load("wald_results.rda")
load("tanh_results.rda")
load("profile_results.rda")

z_crit <- qnorm(0.975)

result <- out |>
  mutate(
    sve_hat = wald_results$estimate,
    ci_lower_wald = wald_results$lower,
    ci_upper_wald = wald_results$upper,
    ci_lower_tanh = tanh_results$lower,
    ci_upper_tanh = tanh_results$upper,
    ci_lower_profile = profile_results$lower,
    ci_upper_profile = profile_results$upper,
    
    true_sve_val = sve:::sve(p0, p1),
    
    covered_wald = (true_sve_val >= ci_lower_wald) & (true_sve_val <= ci_upper_wald),
    covered_tanh = (true_sve_val >= ci_lower_tanh) & (true_sve_val <= ci_upper_tanh),
    covered_profile = (true_sve_val >= ci_lower_profile) & (true_sve_val <= ci_upper_profile)
  )

result_summary <- result |>
  group_by(p0, p1, n0, n1) |>
  summarise(
    coverage_wald = mean(covered_wald),
    coverage_tanh = mean(covered_tanh),
    coverage_profile = mean(covered_profile),
    .groups = "drop"
  )


result_summary |>
  mutate(sve = map2_dbl(p0, p1, sve:::sve)) |>
  ggplot(aes(x = sve, y = coverage_wald)) +
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "gray40", linewidth = 0.8) +
  geom_line(aes(color = "Wald"), alpha = 0.7) +
  geom_line(aes(y = coverage_tanh, color = "tanh-Wald"), alpha = 0.7) +
  geom_line(aes(y = coverage_profile, color = "Profile"), alpha = 0.7) +
  geom_point(size = 0.75, aes(color = "Wald"), alpha = 0.7) +
  geom_point(size = 0.75, aes(y = coverage_tanh, color = "tanh-Wald"), alpha = 0.7) +
  geom_point(size = 0.75, aes(y = coverage_profile, color = "Profile"), alpha = 0.7) +
  scale_color_manual(
    name = "Method",
    values = c("Wald" = "#1E88E5", "tanh-Wald" = "#FFC107", "Profile" = "#D81B60")
  ) +
  scale_y_continuous(breaks = c(0.93, 0.95, 0.97, 0.99)) +
  facet_grid(n0 ~ n1, labeller = label_both) +
  labs(
    x = "True SVE",
    y = "Coverage Probability"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "gray95", color = NA)
  )

ggsave("fig2.png", width = 5, height = 4, dpi = 300)
