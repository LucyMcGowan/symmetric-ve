load("simulation_coverage.rda")
library(tidyverse)
library(sve)

wald_results <- with(out, {
  est_sve(x0, x1, n0, n1, method = "wald", correction = FALSE)
})

save(wald_results, file = "wald_results.rda")

wald_correct_results <- with(out, {
  est_sve(x0, x1, n0, n1, method = "wald", correction = TRUE)
})

save(wald_correct_results, file = "wald_correct_results.rda")

tanh_results <- with(out, {
  est_sve(x0, x1, n0, n1, method = "tanh-wald", correction = FALSE)
})

save(tanh_results, file = "tanh_results.rda")

tanh_correct_results <- with(out, {
  est_sve(x0, x1, n0, n1, method = "tanh-wald", correction = TRUE)
})

save(tanh_correct_results, file = "tanh_correct_results.rda")

profile_results <- with(out, {
  est_sve(x0, x1, n0, n1, method = "profile")
})

save(profile_results, file = "profile_results.rda")


