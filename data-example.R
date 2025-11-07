source("functions.R")
library(sve)
library(tidyverse)
library(kableExtra)
hiv_data <- tibble(
  category = c(
    "All volunteers", "Men", "Women",
    "White (non-Hispanic)", "White men", "White women",
    "Hispanic", "Hispanic men", "Hispanic women",
    "Black (non-Hispanic)", "Black men", "Black women",
    "Asian (all men)", "Other", "Other men",
    "Nonwhite", "Nonwhite men", "Nonwhite women",
    "Age ≤30", "Age >30",
    "Less than college degree", "College or graduate degree",
    "Low risk", "Medium risk", "High risk"
  ),
  
  vaccine_infected = c(241, 239, 2, 211, 211, 0, 14, 13, 1, 6, 5, 1, 3, 7, 7, 30, 28, 2, 84, 157, 95, 146, 32, 177, 32),
  vaccine_total = c(3598, 3391, 207, 2994, 2930, 64, 239, 211, 28, 233, 121, 112, 56, 76, 73, 604, 461, 143, 971, 2627, 1409, 2188, 1211, 2229, 158),
  
  placebo_infected = c(127, 123, 4, 98, 98, 0, 9, 9, 0, 9, 5, 4, 3, 8, 8, 29, 25, 4, 43, 84, 52, 75, 11, 90, 26),
  placebo_total = c(1805, 1704, 101, 1495, 1468, 27, 128, 114, 14, 116, 59, 57, 21, 45, 42, 310, 236, 74, 504, 1301, 713, 1092, 609, 1107, 89)
)

hiv_data <- hiv_data |>
  filter(vaccine_infected != 0, placebo_infected != 0) |>
  mutate(
    p0 = placebo_infected / placebo_total,
    p1 = vaccine_infected / vaccine_total)
sve_ests <- with(hiv_data, 
                 est_sve(placebo_infected, 
                         vaccine_infected, 
                         placebo_total, 
                         vaccine_total, 
                         method = "profile")
                 )
results <- hiv_data |>
  filter(vaccine_infected != 0, placebo_infected != 0) |>
  mutate(
    p0 = placebo_infected / placebo_total,
    p1 = vaccine_infected / vaccine_total,
    
    sve = sve_ests$estimate,
    sve_lower = sve_ests$lower,
    sve_upper = sve_ests$upper,

    rr = p1 / p0,
    log_rr = log(rr),
    var_log_rr = log_rr_var(p0, p1, placebo_total, vaccine_total),
    se_log_rr = sqrt(var_log_rr),
    log_rr_lower = log_rr - qnorm(0.975) * se_log_rr,
    log_rr_upper = log_rr + qnorm(0.975) * se_log_rr,
    ve = ve(p0, p1),
    ve_lower = 1 - exp(log_rr_upper),
    ve_upper = 1 - exp(log_rr_lower),
    
    vaccine_rate = sprintf("%d/%d (%.2f)", vaccine_infected, vaccine_total, p1),
    placebo_rate = sprintf("%d/%d (%.2f)", placebo_infected, placebo_total, p0),
    ve_ci_str = sprintf("%.2f (%.2f to %.2f)", ve, ve_lower, ve_upper),
    sve_ci_str = sprintf("%.2f (%.2f to %.2f)", sve, sve_lower, sve_upper)
  )

table_data <- results |>
  select(
    category,
    vaccine_rate,
    placebo_rate,
    ve_ci_str,
    sve_ci_str
  )

race_header <- tibble(
  category = "Race",
  vaccine_rate = "",
  placebo_rate = "",
  ve_ci_str = "",
  sve_ci_str = ""
)

age_header <- tibble(
  category = "Age",
  vaccine_rate = "",
  placebo_rate = "",
  ve_ci_str = "",
  sve_ci_str = ""
)

edu_header <- tibble(
  category = "Education level",
  vaccine_rate = "",
  placebo_rate = "",
  ve_ci_str = "",
  sve_ci_str = ""
)

risk_header <- tibble(
  category = "Baseline behavioral risk score",
  vaccine_rate = "",
  placebo_rate = "",
  ve_ci_str = "",
  sve_ci_str = ""
)

table_data_formatted <- bind_rows(
  # All volunteers section
  table_data |> filter(category == "All volunteers") |> mutate(category = "All volunteers"),
  table_data |> filter(category == "Men") |> mutate(category = "\\quad Men"),
  table_data |> filter(category == "Women") |> mutate(category = "\\quad Women"),
  
  # Race section
  race_header,
  table_data |> filter(category == "White (non-Hispanic)") |> mutate(category = "\\quad White (non-Hispanic)"),
  table_data |> filter(category == "White men") |> mutate(category = " \\qquad Men"),
  table_data |> filter(category == "White women") |> mutate(category = " \\qquad Women"),
  table_data |> filter(str_detect(category, "^Hispanic$")) |> mutate(category = "\\quad Hispanic"),
  table_data |> filter(category == "Hispanic men") |> mutate(category = " \\qquad Men"),
  table_data |> filter(category == "Hispanic women") |> mutate(category = "  \\qquad Women"),
  table_data |> filter(str_detect(category, "^Black \\(non-Hispanic\\)$")) |> mutate(category = "\\quad Black (non-Hispanic)"),
  table_data |> filter(category == "Black men") |> mutate(category = "\\qquad Men"),
  table_data |> filter(category == "Black women") |> mutate(category = "\\qquad Women"),
  table_data |> filter(category == "Asian (all men)") |> mutate(category = "\\quad Asian (all men)"),
  table_data |> filter(category == "Other" & !str_detect(category, "men")) |> mutate(category = "\\quad Other"),
  table_data |> filter(category == "Other men") |> mutate(category = "\\qquad Men"),
  table_data |> filter(category == "Nonwhite" & !str_detect(category, "men|women")) |> mutate(category = "\\quad Nonwhite"),
  table_data |> filter(category == "Nonwhite men") |> mutate(category = "\\qquad Men"),
  table_data |> filter(category == "Nonwhite women") |> mutate(category = "\\qquad Women"),
  
  # Age section
  age_header,
  table_data |> filter(str_detect(category, "≤30|<=30")) |> mutate(category = "\\quad $\\leq30$ years"),
  table_data |> filter(str_detect(category, ">30|> 30")) |> mutate(category = "\\quad $> 30$ years"),
  
  # Education section
  edu_header,
  table_data |> filter(category == "Less than college degree") |> mutate(category = "\\quad Less than college degree"),
  table_data |> filter(category == "College or graduate degree") |> mutate(category = "\\quad College or graduate degree"),
  
  # Risk section
  risk_header,
  table_data |> filter(category == "Low risk") |> mutate(category = "\\quad Low risk"),
  table_data |> filter(category == "Medium risk") |> mutate(category = "\\quad Medium risk"),
  table_data |> filter(category == "High risk") |> mutate(category = "\\quad High risk")
)

latex_table <- table_data_formatted |>
  kable(
    format = "latex",
    booktabs = TRUE,
    caption = "Rate of HIV-1 infection and vaccine efficacy (VE) and symmetric vaccine efficacy (SVE) against HIV-1 infection",
    label = "study",
    escape = FALSE,  
    col.names = c("Category", "Vaccine", "Placebo", "VE (95\\% CI)", "SVE (95\\% CI)")
  )

## Figure 5 ---- 

section_headers <- tibble(
  category = c("Race", "Age", "Education level", "Baseline behavioral risk score"),
  ve = NA_real_,
  ve_lower = NA_real_,
  ve_upper = NA_real_,
  sve = NA_real_,
  sve_lower = NA_real_,
  sve_upper = NA_real_
)

results_formatted <- results |>
  mutate(
    category_formatted = case_when(
      category == "All volunteers" ~ "All volunteers",
      category == "Men" ~ "    Men",
      category == "Women" ~ "    Women",
      category == "White (non-Hispanic)" ~ "    White (non-Hispanic)",
      category == "White men" ~ "        White Men",
      category == "Hispanic" ~ "    Hispanic",
      category == "Hispanic men" ~ "        Hispanic Men",
      category == "Black (non-Hispanic)" ~ "    Black (non-Hispanic)",
      category == "Black men" ~ "        Black Men",
      category == "Black women" ~ "        Black Women",
      category == "Asian (all men)" ~ "    Asian (all men)",
      category == "Other" ~ "    Other",
      category == "Other men" ~ "        Other Men",
      category == "Nonwhite" ~ "    Nonwhite",
      category == "Nonwhite men" ~ "        Nonwhite Men",
      category == "Nonwhite women" ~ "        Nonwhite Women",
      category == "Age ≤30" ~ "    ≤30 years",
      category == "Age >30" ~ "    >30 years",
      category == "Less than college degree" ~ "    Less than college degree",
      category == "College or graduate degree" ~ "    College or graduate degree",
      category == "Low risk" ~ "    Low risk",
      category == "Medium risk" ~ "    Medium risk",
      category == "High risk" ~ "    High risk",
      TRUE ~ category
    )
  ) |>
  select(category, category_formatted, ve, ve_lower, ve_upper, sve, sve_lower, sve_upper)

plot_data_with_headers <- bind_rows(
  # All volunteers section
  results_formatted |> filter(category == "All volunteers") |> select(-category) |> rename(category = category_formatted),
  results_formatted |> filter(category == "Men") |> select(-category) |> rename(category = category_formatted),
  results_formatted |> filter(category == "Women") |> select(-category) |> rename(category = category_formatted),
  
  # Race section
  section_headers |> filter(category == "Race"),
  results_formatted |> filter(category == "White (non-Hispanic)") |> select(-category) |> rename(category = category_formatted),
  results_formatted |> filter(category == "White men") |> select(-category) |> rename(category = category_formatted),
  results_formatted |> filter(category == "Hispanic") |> select(-category) |> rename(category = category_formatted),
  results_formatted |> filter(category == "Hispanic men") |> select(-category) |> rename(category = category_formatted),
  results_formatted |> filter(category == "Black (non-Hispanic)") |> select(-category) |> rename(category = category_formatted),
  results_formatted |> filter(category == "Black men") |> select(-category) |> rename(category = category_formatted),
  results_formatted |> filter(category == "Black women") |> select(-category) |> rename(category = category_formatted),
  results_formatted |> filter(category == "Asian (all men)") |> select(-category) |> rename(category = category_formatted),
  results_formatted |> filter(category == "Other") |> select(-category) |> rename(category = category_formatted),
  results_formatted |> filter(category == "Other men") |> select(-category) |> rename(category = category_formatted),
  results_formatted |> filter(category == "Nonwhite") |> select(-category) |> rename(category = category_formatted),
  results_formatted |> filter(category == "Nonwhite men") |> select(-category) |> rename(category = category_formatted),
  results_formatted |> filter(category == "Nonwhite women") |> select(-category) |> rename(category = category_formatted),
  
  # Age section
  section_headers |> filter(category == "Age"),
  results_formatted |> filter(category == "Age ≤30") |> select(-category) |> rename(category = category_formatted),
  results_formatted |> filter(category == "Age >30") |> select(-category) |> rename(category = category_formatted),
  
  # Education section
  section_headers |> filter(category == "Education level"),
  results_formatted |> filter(category == "Less than college degree") |> select(-category) |> rename(category = category_formatted),
  results_formatted |> filter(category == "College or graduate degree") |> select(-category) |> rename(category = category_formatted),
  
  # Risk section
  section_headers |> filter(category == "Baseline behavioral risk score"),
  results_formatted |> filter(category == "Low risk") |> select(-category) |> rename(category = category_formatted),
  results_formatted |> filter(category == "Medium risk") |> select(-category) |> rename(category = category_formatted),
  results_formatted |> filter(category == "High risk") |> select(-category) |> rename(category = category_formatted)
)

plot_data <- plot_data_with_headers |>
  pivot_longer(
    cols = c(ve, sve),
    names_to = "type",
    values_to = "estimate"
  ) |>
  mutate(
    lower = ifelse(type == "ve", ve_lower, sve_lower),
    upper = ifelse(type == "ve", ve_upper, sve_upper),
    type = recode(type,
                  ve = "Traditional VE",
                  sve = "Symmetric VE (SVE)"),
    category = fct_rev(factor(category, levels = unique(plot_data_with_headers$category)))
  )

ggplot(plot_data, aes(x = estimate, y = category, color = type)) +
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
    panel.grid.minor = element_blank(),
    panel.grid.major.y = element_blank(),
    strip.background = element_rect(fill = "gray95", color = NA),
    axis.text.y = element_text(hjust = 0, size = 9)  
  )

ggsave("fig6.png", width = 5, height = 4, dpi = 300)
