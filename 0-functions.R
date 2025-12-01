sve <- function(p0, p1) {
  check_proportions(p0, p1)
  2 * (p0 - p1) / (p0 + p1 + abs(p0 - p1))
}

ve <- function(p0, p1) {
  check_proportions(p0, p1)
  (1 - (p1 / p0)) 
}

log_rr_var <- function(p0, p1, n0, n1) {
  check_proportions(p0, p1)
  
  a <- p1 * n1  # vaccinated infections
  b <- (1 - p1) * n1  # vaccinated non-infections
  c <- p0 * n0  # unvaccinated infections
  d <- (1 - p0) * n0  # unvaccinated non-infections
  
  # from: https://www.medcalc.org/en/manual/relative-risk-odds-ratio.php
  # SE{ln(RR)} = sqrt(1/a + 1/c - 1/(a+b) - 1/(c+d))
  # Var{ln(RR)} = 1/a + 1/c - 1/(a+b) - 1/(c+d)
  result <- 1/a + 1/c - 1/(a+b) - 1/(c+d)
  
  return(result)
}

sve_var <- function(p0, p1, n0, n1, c = 1.96, epsilon = NULL) {

  check_proportions(p0, p1)
  sigma0 <- p0 * (1 - p0) / n0
  sigma1 <- p1 * (1 - p1) / n1
  
  if (is.null(epsilon)) {
    epsilon <- c * sqrt(sigma0 + sigma1)
  } else {
    # If epsilon is a scalar, repeat it to match length of p0
    if (length(epsilon) == 1) {
      epsilon <- rep(epsilon, length(p0))
    }
  }
  
  result <- numeric(length(p0))
  
  abs_diff <- abs(p0 - p1)
  idx_smooth <- abs_diff <= epsilon
  idx_p0_gt_p1 <- (p0 > p1) & !idx_smooth
  idx_p1_gt_p0 <- (p1 > p0) & !idx_smooth
  
  # p0 > p1 and |p0 - p1| > epsilon
  result[idx_p0_gt_p1] <- (p1[idx_p0_gt_p1]^2 * sigma0[idx_p0_gt_p1] + 
                             p0[idx_p0_gt_p1]^2 * sigma1[idx_p0_gt_p1]) / 
    p0[idx_p0_gt_p1]^4
  
  # p1 > p0 and |p0 - p1| > epsilon
  result[idx_p1_gt_p0] <- (p1[idx_p1_gt_p0]^2 * sigma0[idx_p1_gt_p0] + 
                             p0[idx_p1_gt_p0]^2 * sigma1[idx_p1_gt_p0]) / 
    p1[idx_p1_gt_p0]^4
  
  # (|p0 - p1| <= epsilon)
  p_avg <- (p0[idx_smooth] + p1[idx_smooth]) / 2
  result[idx_smooth] <- (sigma0[idx_smooth] + sigma1[idx_smooth]) / 
    (p_avg + epsilon[idx_smooth]/4)^2
  
  return(result)
}


sve_ci <- function(p0, p1, n0, n1, level = 0.95, transform = TRUE, c = 1.96, epsilon = NULL) {
  sve_val <- sve(p0, p1)
  
  if (transform) {
    z_val <- atanh(sve_val)
    
    var_sve <- sve_var(p0, p1, n0, n1, c = c, epsilon = epsilon)
    var_z <- var_sve / (1 - sve_val^2)^2
    se_z <- sqrt(var_z)
    
    z_crit <- qnorm(1 - (1 - level) / 2)
    lower_z <- z_val - z_crit * se_z
    upper_z <- z_val + z_crit * se_z
    
    lower <- tanh(lower_z)
    upper <- tanh(upper_z)
    
  } else {
    var_sve <- sve_var(p0, p1, n0, n1, c = c, epsilon = epsilon)
    se_sve <- sqrt(var_sve)
    z_crit <- qnorm(1 - (1 - level) / 2)
    
    lower <- sve_val - z_crit * se_sve
    upper <- sve_val + z_crit * se_sve
  }
  
  data.frame(
    estimate = sve_val,
    lower = lower,
    upper = upper,
    method = if(transform) "Fisher z-transform" else "Wald"
  )
}

check_proportions <- function(p0, p1) {
  if (any(p0 < 0 | p0 > 1)) {
    cli::cli_abort(c("x" =  "{.var p0} must be a proportion between 0 and 1."))
  } 
  if (any(p1 < 0 | p1 > 1)) {
    cli::cli_abort(c("x" = "{.var p1} must be a proportion between 0 and 1."))
  } 
  if (any(p0 == 0 & p1 == 0)) {
    cli::cli_abort(c("x" = "{.var p0} and {.var p1} are exactly 0. 
                     Relative risk is not defined."))
  }
}
