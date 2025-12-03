library(tidyverse)
library(sve)

load("simulation_coverage_wald.rda")

z_crit <- qnorm(0.975)

out_wald <- out_wald |>
  filter(x0 > 0, x1 > 0)

wald_results <- with(out_wald, {
  est_sve(x0, x1, n0, n1, method = "wald")
})

tanh_results <- with(out_wald, {
  est_sve(x0, x1, n0, n1, method = "tanh-wald")
})

wald_results_noep <- with(out_wald, {
  est_sve(x0, x1, n0, n1, method = "wald", smooth = FALSE)
})

tanh_results_noep <- with(out_wald, {
  est_sve(x0, x1, n0, n1, method = "tanh-wald", smooth = FALSE)
})

result <- out_wald |>
  mutate(
    sve_hat = wald_results$estimate,
    ci_lower_wald = wald_results$lower,
    ci_upper_wald = wald_results$upper,
    ci_lower_tanh = tanh_results$lower,
    ci_upper_tanh = tanh_results$upper,
    ci_lower_wald_noep = wald_results_noep$lower,
    ci_upper_wald_noep = wald_results_noep$upper,
    ci_lower_tanh_noep = tanh_results_noep$lower,
    ci_upper_tanh_noep = tanh_results_noep$upper,
    
    true_sve_val = sve:::sve(p0, p1),
    
    covered_wald = (true_sve_val >= ci_lower_wald) & (true_sve_val <= ci_upper_wald),
    covered_tanh = (true_sve_val >= ci_lower_tanh) & (true_sve_val <= ci_upper_tanh),
    covered_wald_noep = (true_sve_val >= ci_lower_wald_noep) & (true_sve_val <= ci_upper_wald_noep),
    covered_tanh_noep = (true_sve_val >= ci_lower_tanh_noep) & (true_sve_val <= ci_upper_tanh_noep)
  ) |>
  mutate(sve = sve:::sve(p0, p1))

result_summary <- result |>
  group_by(p0, p1, sve, n0, n1) |>
  summarise(
    coverage_wald = mean(covered_wald),
    coverage_tanh = mean(covered_tanh),
    coverage_wald_noep = mean(covered_wald_noep),
    coverage_tanh_noep = mean(covered_tanh_noep),
    .groups = "drop"
  ) |>
  mutate(
    sve = round(sve, 2),
    p_diff_label = factor(paste0("SVE = ", sve), 
                          levels = paste0("SVE = ", sort(unique(sve))))
  )


result_summary |>
  ggplot(aes(x = n0, y = coverage_tanh)) +
  geom_hline(yintercept = 0.95, linetype = "dashed", color = "gray40", linewidth = 0.8) +
  geom_line(aes(color = "tanh-Wald (with smoothing)")) +
  geom_line(aes(y = coverage_tanh_noep, color = "tanh-Wald")) +
  
  geom_point(size = 0.75, aes(y = coverage_tanh, color = "tanh-Wald (with smoothing)")) +
  geom_point(size = 0.75, aes(y = coverage_tanh_noep, color = "tanh-Wald")) +
  
  scale_color_manual(
    name = "Method",
    values = c("tanh-Wald" = "cornflower blue", 
               "tanh-Wald (with smoothing)" = "orange")
  ) +
  scale_x_continuous(breaks = c(100, 1000, 2500, 5000)) +
  facet_grid(p0 ~ p_diff_label, 
             labeller = label_bquote(
               rows = p[0] == .(p0),
               cols = .(p_diff_label)
             )) +
  labs(
    x = expression(n[0]==n[1]),
    y = "Coverage Probability"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    strip.background = element_rect(fill = "gray95", color = NA),
    panel.spacing.x = unit(1.5, "lines")
  )

ggsave("fig-s3.png", width = 9, height = 4, dpi = 300)
