load("simulation_coverage.rda")
library(tidyverse)
library(sve)

wald_results <- with(out, {
  est_sve(x0, x1, n0, n1, method = "wald")
})

save(wald_results, file = "wald_results.rda")

tanh_results <- with(out, {
  est_sve(x0, x1, n0, n1, method = "tanh-wald")
})

save(tanh_results, file = "tanh_results.rda")

profile_results <- with(out, {
  est_sve(x0, x1, n0, n1, method = "profile")
})

save(profile_results, file = "profile_results.rda")


