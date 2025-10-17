load("simulation_coverage.rda")
source("functions.R")
library(tidyverse)

z_crit <- qnorm(0.975)

result <- out |>
  filter(p0 == 0.1, p1 == 0.1, n1 == 100, n0 == 100) |>
  mutate(
    z_hat = atanh(sve),
    var_z = sve_var(p0_hat, p1_hat, n0, n1) / (1 - sve^2)^2,
    se_z = sqrt(var_z),
    ci_lower_z_scale = z_hat - z_crit * se_z,
    ci_upper_z_scale = z_hat + z_crit * se_z,
    ci_lower = tanh(ci_lower_z_scale),
    ci_upper = tanh(ci_upper_z_scale),

    ve_val = ve(p0_hat, p1_hat),
    rr_val = p1_hat / p0_hat,
    log_rr = log(rr_val),
    var_log_rr = log_rr_var(p0_hat, p1_hat, n0, n1),
    se_log_rr = sqrt(var_log_rr),
    ci_lower_log_rr = log_rr - z_crit * se_log_rr,
    ci_upper_log_rr = log_rr + z_crit * se_log_rr,
    ci_lower_ve = 1 - exp(ci_upper_log_rr),  
    ci_upper_ve = 1 - exp(ci_lower_log_rr),  

    true_sve = sve(p0, p1),
    true_ve = ve(p0, p1)
  )

set.seed(1)
plot_data <- result |>
  sample_n(25) |>
  mutate(id = 1:n()) |>
  select(id, ve_val, ci_lower_ve, ci_upper_ve, sve, ci_lower, ci_upper) %>%
  pivot_longer(
    cols = c(ve_val, sve),
    names_to = "type",
    values_to = "estimate"
  ) |>
  mutate(
    lower = ifelse(type == "ve_val", ci_lower_ve, ci_lower),
    upper = ifelse(type == "ve_val", ci_upper_ve, ci_upper),
    type = recode(type,
                  ve_val = "Traditional VE",
                  sve = "Symmetric VE (SVE)")
  )

ggplot(plot_data, aes(x = estimate, y = factor(id), color = type)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "gray50") +
  geom_errorbarh(aes(xmin = lower, xmax = upper),
                 height = 0.25, position = position_dodge(width = 0.75)) +
  geom_point(size = 1, position = position_dodge(width = 0.75)) +
  labs(
    x = "Estimated Efficacy",
    y = NULL,
    color = "Estimate Type"
  ) +
  scale_color_manual(values = c("orange", "cornflowerblue")) +
  facet_wrap(~type) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    axis.text.y = element_blank(),
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    strip.background = element_rect(fill = "gray95", color = NA)
  )


ggsave("fig4.png", width = 5, height = 4, dpi = 300)
