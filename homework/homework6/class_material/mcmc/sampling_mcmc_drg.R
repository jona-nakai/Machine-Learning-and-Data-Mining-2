library(ggplot2)

##########################
# Randomly Sampling Using R
##########################
set.seed(42) # for reproducibility
mean_val <- 0
std_dev <- 1
n <- 1000
samples <- rnorm(n, mean = mean_val, sd = std_dev)

# Estimate parameters
mu_hat <- mean(samples)
sigma_hat <- sd(samples)

cat("mu_hat =", mu_hat, "sigma_hat =", sigma_hat, "\n")

# Plot histogram and estimated normal density
ggplot(data = data.frame(samples), aes(x = samples)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "blue", alpha = 0.5) +
  stat_function(fun = dnorm, args = list(mean = mu_hat, sd = sigma_hat), col = "red") +
  labs(title = "Histogram of Gaussian", x = "Value", y = "Density")





##########################
# Rejection Sampling Example
##########################
library(stats)

# Define proposal distribution (exponential)
q <- function(y) {
  dexp(y, rate = 1/6)  # Exponential with scale 6
}

# Define target distribution (Chi-square)
p <- function(y) {
  dchisq(y, df = 3)
}

# Verify that the proposal is above the target distribution
k <- 3
y_vals <- seq(0, 20, length.out = 100)
p1 <- p(y_vals)
p2 <- k * q(y_vals)

# Plot target and proposal
plot(y_vals, p1, type = "l", col = "blue", ylim = c(0, max(p2)), lwd = 2, ylab = "Density")
lines(y_vals, p2, col = "red", lwd = 2)
legend("topright", legend = c("Target (Chi-square)", "Proposal (Scaled Exponential)"), col = c("blue", "red"), lty = 1)

# Rejection Sampling Function
rejection_sampling <- function(n) {
  samples <- c()
  while (length(samples) < n) {
    v <- rexp(1, rate = 1/6)  # Sample from proposal distribution
    if (k * q(v) * runif(1) < p(v)) {
      samples <- c(samples, v)
    }
  }
  return(samples)
}

# Generate rejection samples
set.seed(42)
rej_samples <- rejection_sampling(1000)

# Plot histogram of accepted samples
ggplot(data = data.frame(rej_samples), aes(x = rej_samples)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "blue", alpha = 0.5) +
  stat_function(fun = p, col = "red") +
  labs(title = "Rejection Sampling Example", x = "Value", y = "Density")





##########################
# Ancestral Sampling Example for p(y, theta)
##########################
library(MASS)

set.seed(42)
alpha <- 1
beta <- 19
n <- 10000  # Generate 10,000 samples

# Sample theta from Beta(alpha, beta)
theta_samples <- rbeta(n, alpha, beta)

# Sample y from Bernoulli using theta
y_samples <- rbinom(n, size = 1, prob = theta_samples)

# Create overlaid histograms (for joint distribution)
# Had some trouble getting the same sort of heatmap to work as in python, but this I think is in some sense even more intuitive
joint_df <- data.frame(theta = theta_samples, y = as.factor(y_samples))

ggplot(joint_df, aes(x = theta, fill = y)) +
  geom_histogram(position = "stack", bins = 100) +
  scale_fill_manual(values = c("blue", "red")) +
  labs(title = "Joint Dist of Ancestral Sampling (Beta -> Bernoulli)", x = "theta", y = "Frequency")

# Posterior when y = 0
theta_giveny0 <- theta_samples[y_samples == 0]
theta_giveny1 <- theta_samples[y_samples == 1]

# Plot posterior when y = 0
ggplot(data.frame(theta_giveny0), aes(x = theta_giveny0)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "blue", alpha = 0.5) +
  geom_vline(xintercept = mean(theta_giveny0), col = "purple", lwd = 1) +
  geom_vline(xintercept = 1/20, col = "red", lwd = 1) +
  labs(title = "Posterior Distribution for y = 0", x = "theta", y = "Density")

# Plot posterior when y = 1
ggplot(data.frame(theta_giveny1), aes(x = theta_giveny1)) +
  geom_histogram(aes(y = after_stat(density)), bins = 30, fill = "blue", alpha = 0.5) +
  geom_vline(xintercept = mean(theta_giveny1), col = "purple", lwd = 1) +
  geom_vline(xintercept = 1/20, col = "red", lwd = 1) +
  labs(title = "Posterior Distribution for y = 1", x = "theta", y = "Density")

# the estimated posterior means
mean(theta_giveny0)
mean(theta_giveny1)
# the true posterior means
1/21
2/21





##########################
# Practice Solutions - Normal Model (with known variance)
##########################
# Set up parameters for the prior mu ~ N(eta, tau)
eta <- 50
tau <- 2
known_sigma <- 1

# Define the proposal (q) and target (p) densities for mu
q <- function(mu) {
  dnorm(mu, mean = 40, sd = 15)
}
p <- function(mu) {
  dnorm(mu, mean = 50, sd = 2)
}

k <- 10
mu_range <- seq(-10, 90, length.out = 1000)
p1 <- p(mu_range)
p2 <- k * q(mu_range)
plot(mu_range, p1, type = "l", col = "black", lwd = 2,
     main = "Target vs Proposal for mu", xlab = "mu", ylab = "Density", ylim = c(0,.3))
lines(mu_range, p2, col = "red", lwd = 2)

# Rejection sampling for mu
rejection_sampling <- function(n) {
  samples <- c()
  while(length(samples) < n) {
    v <- rnorm(1, mean = 40, sd = 15)
    if (k * q(v) * runif(1) < p(v)) {
      samples <- c(samples, v)
    }
  }
  return(samples)
}
mu_samples <- rejection_sampling(10000)
hist(mu_samples, breaks = 30, probability = TRUE, col = rgb(0,0,1,0.5), xlim = c(-10, 90), ylim = c(0, .3),
     main = "Rejection Sampling for mu")
lines(mu_range, p1, col = "black", lwd = 2)
lines(mu_range, p2, col = "red", lwd = 2)

# Generate samples from y given mu samples (assume y|mu ~ N(mu, known_sigma^2))
y_samples_norm <- rnorm(length(mu_samples), mean = mu_samples, sd = known_sigma)
range(y_samples_norm)

# Visualize joint distribution of mu and y using a 2D density estimate
dens_est <- kde2d(mu_samples, y_samples_norm, n = 100)
image(dens_est$x, dens_est$y, dens_est$z,
      xlab = "mu (Normal)", ylab = "y (Normal)", main = "Joint Distribution of y and mu")
contour(dens_est$x, dens_est$y, dens_est$z, add = TRUE) #optional contours

# How to estimate the conditional posterior p(mu|y) for a given y*:
y_star <- 48
tolerance <- 0.1
mu_given_y <- mu_samples[abs(y_samples_norm - y_star) < tolerance]
hist(mu_given_y, breaks = 30, probability = TRUE, col = rgb(0,0,1,0.5),
     main = sprintf("Posterior Distribution p(mu | y = %d)", y_star), xlab = expression(mu))
abline(v = mean(mu_given_y), col = "red", lwd = 2, lty = 2)

# Compare with the true posterior (formula for known variance, conjugate Normal model)
true_post_mu <- ((y_star / (known_sigma^2)) + (eta / (tau^2))) / ((1 / (known_sigma^2)) + (1 / (tau^2)))
true_post_sigma <- sqrt(1 / ((1 / (known_sigma^2)) + (1 / (tau^2))))
cat(sprintf("True posterior mean = %f vs. Estimated = %f\n", true_post_mu, mean(mu_given_y)))

mu_range2 <- seq(45, 52, length.out = 100)
lines(mu_range2, dnorm(mu_range2, mean = true_post_mu, sd = true_post_sigma), col = "blue", lwd = 2)
legend("topright", legend = c("Estimated Posterior Mean", "True Posterior"),
       col = c("red", "blue"), lty = c(2,1), lwd = 2)


##########################
# MCMC: Metropolis-Hastings for the Banana Distribution
##########################
# Banana Example
banana_p <- function(y1, y2) {
  exp(-((y1^2 + y2^2 - 0.5)^2 / 0.5) - ((y2 - y1^2 + 1)^2 / 1.5))
}

# Create a grid for the theoretical banana distribution
y1_range <- seq(-1.5, 1.5, length.out = 100)
y2_range <- seq(-1.5, 1.5, length.out = 100)
grid_data <- expand.grid(Y1 = y1_range, Y2 = y2_range)
Y1 <- matrix(grid_data$Y1, nrow = 100, ncol = 100)
Y2 <- matrix(grid_data$Y2, nrow = 100, ncol = 100)

p_values <- matrix(banana_p(Y1, Y2), nrow = 100, ncol = 100)

# Make the plot using base R
image(x = y1_range, y = y2_range, z = p_values,
      col = hcl.colors(100, "Viridis"),
      xlab = "y1", ylab = "y2",
      main = "Banana Distribution")

# For 3D plot using plotly (requires plotly package)
library(plotly)
plot_ly(x = ~y1_range, y = ~y2_range, z = ~p_values) %>%
  add_surface() %>%
  layout(title = "Banana Distribution",
         scene = list(xaxis = list(title = "y1"),
                      yaxis = list(title = "y2"),
                      zaxis = list(title = "Density")))

# Define the metropolis algorithm
metropolis_sampling <- function(n = 10000, sigma = 1.25, burn = 0.18) {
  samples <- list()  # keep track of samples
  y1 <- 0.0
  y2 <- 0.0  # Start from the origin
  total_samples <- floor(n / (1 - burn))
  m <- as.integer(total_samples * burn)  # discard amount
  accepted <- 0  # keep track of acceptances
  
  while (length(samples) < total_samples) {
    y1_prop <- rnorm(1, y1, sigma)
    y2_prop <- rnorm(1, y2, sigma)
    alpha <- banana_p(y1_prop, y2_prop) / banana_p(y1, y2)
    
    if (runif(1) < alpha) {
      y1 <- y1_prop
      y2 <- y2_prop
      accepted <- accepted + 1
    }
    # "Rejecting" the proposed sample is the same as accepting the old one again
    samples[[length(samples) + 1]] <- c(y1, y2)
  }
  
  # Calculate acceptance rate (should be ideally about 23.4%)
  acceptance_rate <- accepted / total_samples
  cat(sprintf('Acceptance rate: %.3f\n', acceptance_rate))
  
  # Convert to matrix
  samples <- do.call(rbind, samples)
  samples <- samples[(m + 1):nrow(samples), ]  # Discard 18% of initial samples
  
  return(samples)
}

# Generate the samples
samples <- metropolis_sampling()
print(nrow(samples))  # verify there are 10000

# Check convergence with plots
plot(samples[, 1], type = 'l', main = 'Convergence of Y1 Samples', 
     xlab = 'Iteration', ylab = 'Y1')

plot(samples[, 2], type = 'l', main = 'Convergence of Y2 Samples',
     xlab = 'Iteration', ylab = 'Y2')

# Check ACF plots to determine if mixing well
acf(samples[, 1], main = 'Metropolis Y1 Samples ACF Plot')

acf(samples[, 2], main = 'Metropolis Y2 Samples ACF Plot')

# Plots below suggest chain is relatively slow mixing; we would like to ideally have very little
# correlation between samples, so we should consider sampling more points and then thinning

## If you want to thin (say, keep only every 10th sample)
thinned_samples <- samples[seq(1, nrow(samples), by = 10), ]  # keeps every 10th

## Should show better mixing
acf(thinned_samples[, 1], main = 'Metropolis Y1 (thinned) Samples ACF Plot')

acf(thinned_samples[, 2], main = 'Metropolis Y2 (thinned) Samples ACF Plot')

# For 3D histogram using plotly
# Create histogram manually
breaks_x <- seq(min(samples[, 1]), max(samples[, 1]), length.out = 26)
breaks_y <- seq(min(samples[, 2]), max(samples[, 2]), length.out = 26)
hist_data <- table(cut(samples[, 1], breaks = breaks_x),
                   cut(samples[, 2], breaks = breaks_y))

# Get bin centers
xpos <- (breaks_x[-1] + breaks_x[-length(breaks_x)]) / 2
ypos <- (breaks_y[-1] + breaks_y[-length(breaks_y)]) / 2

# Create 3D surface plot
plot_ly(x = ~xpos, y = ~ypos, z = ~hist_data) %>%
  add_surface() %>%
  layout(title = "3D Histogram of Samples from 2D Banana Distribution",
         scene = list(xaxis = list(title = "y1"),
                      yaxis = list(title = "y2"),
                      zaxis = list(title = "Density")))






##########################
# MCMC: Metropolis-Hastings for a Bivariate Normal (Bayesian Metropolis)
##########################
library(mvtnorm)

# Bayesian Metropolis
# the likelihood
Sigma <- matrix(c(1, 1, 1, 2), nrow = 2, ncol = 2)

likelihood <- function(y, mu) {
  dmvnorm(y, mean = mu, sigma = Sigma)
}

# the prior
mu0 <- c(0, 0)
Sigma0 <- matrix(c(1, 0, 0, 1), nrow = 2, ncol = 2)

prior <- function(mu) {
  dmvnorm(mu, mean = mu0, sigma = Sigma0)
}

# and the joint we'll sample from
p <- function(y, mu) {
  likelihood(y, mu) * prior(mu)
}

# Define the metropolis algorithm
# We're sampling from a 4 dimensional thing, so we need a four dimensional proposal
# We'll need LOTS of samples to make this work (so this will take a while)
metropolis_sampling_bayesian <- function(n = 100000, sigma = 1.25, burn = 0.18) {
  samples <- list()  # keep track of samples
  y1 <- 0.0
  y2 <- 0.0
  mu1 <- 0.0
  mu2 <- 0.0  # Start at zeros
  total_samples <- as.integer(n / (1 - burn))
  m <- as.integer(total_samples * burn)  # discard amount
  accepted <- 0  # keep track of acceptances
  
  while (length(samples) < total_samples) {
    y1_prop <- rnorm(1, y1, sigma)
    y2_prop <- rnorm(1, y2, sigma)
    mu1_prop <- rnorm(1, mu1, sigma)
    mu2_prop <- rnorm(1, mu2, sigma)
    
    alpha <- p(c(y1_prop, y2_prop), c(mu1_prop, mu2_prop)) / 
      p(c(y1, y2), c(mu1, mu2))
    
    if (runif(1) < alpha) {
      y1 <- y1_prop
      y2 <- y2_prop
      mu1 <- mu1_prop
      mu2 <- mu2_prop
      accepted <- accepted + 1
    }
    
    samples[[length(samples) + 1]] <- c(y1, y2, mu1, mu2)
  }
  
  # Calculate acceptance rate (should be ideally about 23.4%)
  acceptance_rate <- accepted / total_samples
  cat(sprintf('Acceptance rate: %.3f\n', acceptance_rate))
  
  # Convert to matrix
  samples <- do.call(rbind, samples)
  samples <- samples[(m + 1):nrow(samples), ]  # Discard 18% of initial samples
  
  return(samples)
}

# Generate the samples
samples_bayes <- metropolis_sampling_bayesian()
nrow(samples_bayes)  # verify there are 100000

# check convergence with plots
# in this case, it should converge very quickly, since the starting samples were reasonable and this is not too complex a distribution
# try changing the starting values in the metropolis_sampling function (and not discarding) to see
plot(samples_bayes[,1], type = "l", main = "Convergence of y1")
plot(samples_bayes[,2], type = "l", main = "Convergence of y2")
plot(samples_bayes[,3], type = "l", main = "Convergence of mu1")
plot(samples_bayes[,4], type = "l", main = "Convergence of mu2")

# check ACF plots to determine if mixing well (even if not, we won't do anything about it)
acf(samples_bayes[,1])
acf(samples_bayes[,2])
acf(samples_bayes[,3])
acf(samples_bayes[,4])
# plots suggest chain is relatively slow mixing; we would like to ideally have very little
# correlation between samples, so we should consider sampling more points and then thinning

## If you want to thin (say, keep only every 10th sample)
thinned_samples <- samples_bayes[seq(1, nrow(samples_bayes), by = 10), ]  # keeps every 10th

## Should show better mixing
acf(thinned_samples[, 1], main = 'Metropolis Y1 (thinned) Samples ACF Plot')

acf(thinned_samples[, 2], main = 'Metropolis Y2 (thinned) Samples ACF Plot')

# Extract posterior of mu|y for a given observation y_obs
y_obs <- c(3, 2)
tol <- 0.5
mu_given_y <- samples_bayes[abs(samples_bayes[,1] - y_obs[1]) < tol & abs(samples_bayes[,2] - y_obs[2]) < tol, 3:4]
dens_bayes <- kde2d(mu_given_y[,1], mu_given_y[,2], n = 30)
image(dens_bayes$x, dens_bayes$y, dens_bayes$z,
      xlab = "mu1", ylab = "mu2", main = "Joint Posterior of mu|y")
#points(mu_given_y, pch = 19, col = "red")
posterior_mean <- colMeans(mu_given_y)
print(posterior_mean)
true_post_mean_bayes <- solve(solve(Sigma0) + solve(matrix(c(1,1,1,2), ncol = 2))) %*%
  (solve(Sigma0) %*% matrix(mu0, ncol = 1) + solve(matrix(c(1,1,1,2), ncol = 2)) %*% matrix(y_obs, ncol = 1))
print(true_post_mean_bayes)






##########################
# Gibbs Sampling Example for a 2-D Conditional Gaussian
##########################
mu <- matrix(c(3, 2), ncol = 1)
Sigma <- matrix(c(1, 1, 1, 2), ncol = 2, byrow = TRUE)

# Function to generate y_i given y_j for a bivariate Gaussian
pyigiven_yj <- function(yj, j, mu, Sigma) {
  i <- which(!(1:length(mu) == j))
  mu_a <- mu[i, 1]
  mu_b <- mu[j, 1]
  sigma_aa <- Sigma[i, i]
  sigma_ab <- Sigma[i, j]
  sigma_bb <- Sigma[j, j]

  mu_agivenb <- mu_a + sigma_ab * (1/sigma_bb) * (yj - mu_b)
  sigma_agivenb <- sigma_aa - sigma_ab * (1/sigma_bb) * sigma_ab

  return(rnorm(1, mean = mu_agivenb, sd = sqrt(sigma_agivenb)))
}

gibbs_sampling <- function(n, burn = .18) {
  samples <- matrix(0, nrow = n, ncol = 2)
  y1 <- 0
  y2 <- 0
  total_samples = floor(n / (1 - burn))
  m = floor(total_samples * burn)
  
  for(sample in 1:n) {
    y1 <- pyigiven_yj(y2, 2, mu, Sigma)  # if indexing in R, j=2 corresponds to second element
    y2 <- pyigiven_yj(y1, 1, mu, Sigma)
    samples[sample, ] <- c(y1, y2)
  }
  return(samples[(m+1):n,])
}

samples_gibbs <- gibbs_sampling(10000)
head(samples_gibbs, 10)

print(colMeans(samples_gibbs))
print(cov(samples_gibbs))
