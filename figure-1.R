library(ggplot2)

ve_curve <- function(VE, p0_vals) {
  p1 <- (1 - VE) * p0_vals
  p1[p1 < 0] <- NA_real_
  p1[p1 > 1] <- NA_real_
  data.frame(p0 = p0_vals, p1 = p1, effect = VE)
}

sve_curve <- function(SVE, p0_vals) {
  if (SVE >= 0) {
    p1 <- (1 - SVE) * p0_vals
  } else {
    p1 <- p0_vals / (1 + SVE)
  }
  p1[p1 < 0] <- NA_real_
  p1[p1 > 1] <- NA_real_
  data.frame(p0 = p0_vals, p1 = p1, effect = SVE)
}

p0_vals <- seq(0, 1, length.out = 600) 
ve_values <- seq(-0.95, 0.95, length.out = 20)
sve_values <- seq(-0.95, 0.95, length.out = 20)  

ve_lines <- do.call(rbind, lapply(ve_values, ve_curve, p0_vals = p0_vals))
sve_lines <- do.call(rbind, lapply(sve_values, sve_curve, p0_vals = p0_vals))

ggplot(ve_lines, aes(x = p0, y = p1, group = factor(effect), color = effect)) +
  geom_line(size = 0.9) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  scale_color_gradient2(name = "VE", midpoint = 0, 
                        low = "orange", mid = "white", high = "cornflower blue") + 
  labs(
    x = expression("Risk in unvaccinated" ~ (p[0])),
    y = expression("Risk in vaccinated" ~ (p[1]))) +
  coord_equal() +
  theme_minimal(base_size = 12) + 
  theme(panel.grid = element_blank())

ggsave("fig1-a.png", width = 5, height = 4, dpi = 300)

ggplot(sve_lines, aes(x = p0, y = p1, group = factor(effect), color = effect)) +
  geom_line(size = 0.9) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  scale_color_gradient2(name = "SVE", midpoint = 0, 
                        low = "orange", mid = "white", high = "cornflower blue") + 
  labs(
    x = expression("Risk in unvaccinated" ~ (p[0])),
    y = expression("Risk in vaccinated" ~ (p[1]))) +
  coord_equal() +
  theme_minimal(base_size = 12) + 
  theme(panel.grid = element_blank())

ggsave("fig1-b.png", width = 5, height = 4, dpi = 300)
