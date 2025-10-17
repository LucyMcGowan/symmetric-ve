source("functions.R")
sim_infections <- function(p0, p1, n0, n1, n_iter = 10000) {
  replicate(n_iter, {
    
    infected_unvax <- rbinom(n0, 1, p0)
    infected_vax <- rbinom(n1, 1, p1)
    
    p0_hat <- mean(infected_unvax)
    p1_hat <- mean(infected_vax)
    
    c(p0 = p0, 
      p1 = p1, 
      n0 = n0,
      n1 = n1, 
      p0_hat = p0_hat, 
      p1_hat = p1_hat, 
      sve = sve(p0_hat, p1_hat))
  }, simplify = FALSE)
}

grid <- tidyr::expand_grid(
  p0 = seq(0.1, 0.9, by = 0.1), 
  p1 = seq(0.1, 0.9, by = 0.1),
  n0 = c(100, 250, 1000),
  n1 = c(100, 250, 1000)
)

library(furrr)
library(future)
plan(multisession, workers = availableCores() - 1)  


out <- furrr::future_pmap(
  grid, 
  sim_infections,
  n_iter = 10000,
  .options = furrr_options(seed = TRUE),
  .progress = TRUE
) |>
  unlist(recursive = FALSE) |>
  dplyr::bind_rows()

save(out, file = "simulation_coverage.rda")
