
# grid approximation ----

# define grid ----
p_grid <- seq(from = 0, to = 1, length = 20)

# define prior ----
prior <- rep(1,20)

# compute likelihood at each value in grid ----

likelihood <- dbinom(6, size=9, prob = p_grid)


# compute product of likelihood and prior ---------------------------------

unst.posterior <- likelihood * prior


# standardize the posterior so it sums to 1 -------------------------------

posterior <- unst.posterior / sum(unst.posterior) 

plot(p_grid, posterior, type = "b",
     xlab="probability of water", ylab = "posterior probability")
mtext("20 points")

grid_approx  <- function(points = 20) {
  p_grid <- seq(from = 0, to = 1, length = points)
  prior <- exp( -5*abs(p_grid - 0.5))
  likelihood <- dbinom(6, size=9, prob = p_grid)
  unst.posterior <- likelihood * prior
  posterior <- unst.posterior / sum(unst.posterior)
  plot(p_grid, 
       posterior, 
       type = "b",
       xlab="probability of water", 
       ylab = "posterior probability")
  mtext(paste(points,"points"))
}
