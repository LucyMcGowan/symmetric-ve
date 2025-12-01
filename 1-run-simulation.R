source("functions.R")
sim_infections <- function(p0, p1, n0, n1, n_iter = 10000) {
  replicate(n_iter, {
    
    x0 <- rbinom(1, n0, p0)  
    x1 <- rbinom(1, n1, p1)  
    
    c(p0 = p0, 
      p1 = p1, 
      n0 = n0,
      n1 = n1, 
      x0 = x0, 
      x1 = x1)
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


set.seed(1)

grid_wald <- expand_grid(
  p0 = c(0.1, 0.3, 0.5),
  delta = c(1, 0.99, 0.95, 0.9),
  n0 = c(100, 1000, 2500, 5000)
)
grid_wald$p1 <- grid_wald$p0*grid_wald$delta
grid_wald$n1 <- grid_wald$n0
grid_wald$delta <- NULL
plan(multisession, workers = availableCores() - 1)  

out_wald <- furrr::future_pmap(
  grid_wald, 
  sim_infections,
  n_iter = 10000,
  .options = furrr_options(seed = TRUE),
  .progress = TRUE
) |>
  unlist(recursive = FALSE) |>
  dplyr::bind_rows()

save(out_wald, file = "simulation_coverage_wald.rda")
