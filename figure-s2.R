load("simulation_coverage.rda")
source("functions.R")
library(tidyverse)

z_crit <- qnorm(0.975)

result <- out |>
  mutate(
    z_hat = atanh(sve),
    var_z = sve_var(p0_hat, p1_hat, n0, n1) / (1 - sve^2)^2,
    se_z = sqrt(var_z),
    ci_lower_z_scale = z_hat - z_crit * se_z,
    ci_upper_z_scale = z_hat + z_crit * se_z,
    ci_lower = tanh(ci_lower_z_scale),
    ci_upper = tanh(ci_upper_z_scale),
    width_sve = ci_upper - ci_lower,
    
    ve_val = ve(p0_hat, p1_hat),
    rr_val = p1_hat / p0_hat,
    log_rr = log(rr_val),
    var_log_rr = log_rr_var(p0_hat, p1_hat, n0, n1),
    se_log_rr = sqrt(var_log_rr),
    ci_lower_log_rr = log_rr - z_crit * se_log_rr,
    ci_upper_log_rr = log_rr + z_crit * se_log_rr,
    ci_lower_ve = 1 - exp(ci_upper_log_rr),  
    ci_upper_ve = 1 - exp(ci_lower_log_rr), 
    width_ve = ci_upper_ve - ci_lower_ve,
    
    true_sve = sve(p0, p1),
    true_ve = ve(p0, p1)
  )

result_summary <- result |>
  filter(n0 == 1000, n1 == 1000) |>
  group_by(p0, p1, n0, n1) |>
  summarise(
    true_sve = first(true_sve),
    true_ve = first(true_ve),
    mean_width_sve = mean(width_sve),
    mean_width_ve = mean(width_ve),
    median_width_sve = median(width_sve),
    median_width_ve = median(width_ve),
    width_ratio = mean_width_ve / mean_width_sve,
    .groups = "drop"
  )

result_summary |>
  ggplot(aes(x = true_sve, y = width_ratio)) +
  annotate("rect", 
           xmin = 0, 
           xmax = max(result_summary$true_sve), 
           ymin = 0, ymax = Inf,
           fill = "lightblue", alpha = 0.15) +
  annotate("text", 
           x = 0.45, 
           y = 50, 
           label = "SVE = VE") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.8) +
  geom_line(color = "cornflower blue", linewidth = 1.4) +
  geom_point(color = "cornflower blue") +
  scale_x_continuous(
    name = "Symmetric VE (SVE)",
    breaks = seq(-0.9, 0.9, by = 0.2),
    sec.axis = sec_axis(
      transform = ~.,
      name = "Traditional VE",
      breaks = seq(-0.9, 0.9, by = 0.2),
      labels = c(round((seq(-0.9, -0.1, by = 0.2) / (seq(-0.9, -0.1, by = 0.2) + 1)), 1), seq(0.1, 0.9, by = 0.2))
    )) +
  labs(y = "Ratio: VE CI Width / SVE CI Width") +
  theme_minimal(base_size = 12) 

ggsave("fig-s2.png", width = 5, height = 4, dpi = 300)
