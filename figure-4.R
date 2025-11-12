load("simulation_coverage.rda")
library(tidyverse)
library(sve)
z_crit <- qnorm(0.975)

out <- out |>
  filter(p0 == p1) 

wald_results <- with(out, {
  est_sve(x0, x1, n0, n1, method = "wald")
})

tanh_results <- with(out, {
  est_sve(x0, x1, n0, n1, method = "tanh-wald")
})

profile_results <- with(out, {
  est_sve(x0, x1, n0, n1, method = "profile")
})

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
    t1error_wald = 1 - mean(covered_wald),
    t1error_tanh = 1 - mean(covered_tanh),
    t1error_profile = 1 - mean(covered_profile),
    .groups = "drop"
  )

result_summary |>
  ggplot(aes(x = p0, y = t1error_wald)) +
  geom_hline(yintercept = 0.05, linetype = "dashed", color = "gray40", linewidth = 0.8) +
  geom_line(aes(color = "Wald"), linewidth = 0.8) +
  geom_line(aes(y = t1error_tanh, color = "tanh-Wald"), alpha = 0.7) +
  geom_line(aes(y = t1error_profile, color = "Profile"), alpha = 0.7) +
  geom_point(size = 0.75, aes(color = "Wald"), alpha = 0.7) +
  geom_point(size = 0.75, aes(y = t1error_tanh, color = "tanh-Wald"), alpha = 0.7) +
  geom_point(size = 0.75, aes(y = t1error_profile, color = "Profile"), alpha = 0.7) +
  scale_color_manual(
    name = "Method",
    values = c("Wald" = "#1E88E5", "tanh-Wald" = "#FFC107", "Profile" = "#D81B60")
  ) +
  scale_y_continuous(breaks = c(0.0, 0.025, 0.05, 0.075)) +
  scale_x_continuous(breaks = c(0.1, 0.3, 0.5, 0.7, 0.9)) +
  facet_grid(n0 ~ n1, labeller = label_both) +
  labs(
    x = expression(p[0] == p[1]),
    y = "Type I Error"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "gray95", color = NA)
  )

ggsave("fig4.png", width = 5, height = 4, dpi = 300)
