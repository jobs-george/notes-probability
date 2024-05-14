# simulate

set.seed(42)

n <- 10^3
n_vec <- 1:n

simulations <- sample(c(0,1), size = n, replace = TRUE)

moving_average <- cumsum(simulations) / n_vec

plot(moving_average, ylim = c(0,1), type = "l", xlab = "n", ylab = "Sample Average", main = "Bern(1/2)")
curve(0.5 + sqrt(1/(4*x)), from = 0, to = 1000, col = "red", add = TRUE)
curve(0.5 - sqrt(1/(4*x)), from = 0, to = 1000, col = "red", add = TRUE)

# simulate

set.seed(13)

n <- 10^3
n_vec <- 1:n

simulations <- rcauchy(n)

moving_average <- cumsum(simulations) / n_vec

plot(moving_average, type = "l", xlab = "n", ylab = "Sample Average", main = "Cauchy(0,1)")
