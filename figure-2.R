load("simulation_coverage.rda")
source("functions.R")
library(tidyverse)
z_crit <- qnorm(0.975)

result <- out |>
  mutate(

    se_hat = sqrt(sve_var(p0_hat, p1_hat, n0, n1)),
    ci_lower_wald = sve - z_crit * se_hat,
    ci_upper_wald = sve + z_crit * se_hat,
    
    z_hat = atanh(sve),
    var_z = sve_var(p0_hat, p1_hat, n0, n1) / (1 - sve^2)^2,
    se_z = sqrt(var_z),
    ci_lower_z_scale = z_hat - z_crit * se_z,
    ci_upper_z_scale = z_hat + z_crit * se_z,
    ci_lower_fisher = tanh(ci_lower_z_scale),
    ci_upper_fisher = tanh(ci_upper_z_scale),
    
    true_sve_val = sve(p0, p1),
    covered_wald = (true_sve_val >= ci_lower_wald) & (true_sve_val <= ci_upper_wald),
    covered_fisher = (true_sve_val >= ci_lower_fisher) & (true_sve_val <= ci_upper_fisher)
  )

result_summary <- result |>
  filter(p0_hat !=0 & p1_hat != 0) |>
  group_by(p0, p1, n0, n1) |>
  summarise(
    coverage_wald = mean(covered_wald),
    coverage_fisher = mean(covered_fisher),
    .groups = "drop"
  )

result_summary |>
  mutate(sve = map2_dbl(p0, p1, sve)) |>
  ggplot(aes(x = sve, y = coverage_wald)) +
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "gray40", linewidth = 0.8) +
  geom_line(aes(color = "Original scale Wald"), alpha = 0.7) +
  geom_line(aes(y = coverage_fisher, color = "tanh-Wald"), alpha = 0.7) +
  geom_point(size = 0.75, aes(color = "Original scale Wald"), alpha = 0.7) +
  geom_point(size = 0.75, aes(y = coverage_fisher, color = "tanh-Wald"), alpha = 0.7) +
  scale_color_manual(
    name = "Method",
    values = c("Original scale Wald" = "cornflower blue", "tanh-Wald" = "orange")
  ) +
  scale_y_continuous(breaks = c(0.93, 0.95, 0.97)) +
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
