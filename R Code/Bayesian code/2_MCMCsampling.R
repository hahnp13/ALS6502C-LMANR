
library(tidyverse)

# imagine you collect yield from 20 tomato plants
# typical values of the yield are around 0 but could be anywhere from like -10 to 10
# (note: obviously negative yields are not possible, but assume data are centered so 0 is a typical value and positive values are better than average)


# Step 1. Specify priors ####
theta_grid <- seq(-10, 10, length.out = 1000)
prior_density <- dnorm(theta_grid, mean = 0, sd = 5)

## plot out priors ####
plot(theta_grid, prior_density,
     type = "l",
     lwd = 2, 
     xlab = 'theta', ylab = "Density",
     main = "Prior distribution for theta")


# Step 2. Setup likelihood function
likelihood <- function(theta) {
  prod(dnorm(y, mean = theta, sd = 1))
}

unnormalized_posterior <- function(theta) {
  likelihood(theta) * dnorm(theta, mean = 0, sd = 5)
}


# Step 3. Collect the data (we will simulate data) 
set.seed(123)

y <- rnorm(30, mean = 2, sd = 1) %>% round(., 2)
head(as.data.frame(y))
summary(y) # look just to get a sense of the values
hist(y)


# Step 4. Estimate likelihood of the data given the range of prior values
likelihood <- function(theta) {
  prod(dnorm(y, mean = theta, sd = 1))
}

likelihood_values <- sapply(theta_grid, likelihood)

plot(theta_grid, likelihood_values,
     type = "l",
     lwd = 2,
     xlab = expression(theta),
     ylab = "Likelihood",
     main = "Likelihood function for theta")


## Plot prior and likelihood together ####
prior_scaled <- prior_density / max(prior_density)
likelihood_scaled <- likelihood_values / max(likelihood_values)
plot(theta_grid, prior_scaled,
     type = "l",
     lwd = 2,
     ylim = c(0, 1),
     xlab = expression(theta),
     ylab = "Scaled density",
     main = "Prior and Likelihood")

lines(theta_grid, likelihood_scaled, lwd = 2, lty = 2)
legend("topright",
       legend = c("Prior", "Likelihood"),
       lty = c(1, 2),
       lwd = 2)

# Step 5. Compute posterior ####
unnormalized_posterior <- likelihood_values * prior_density
posterior_scaled <- unnormalized_posterior / max(unnormalized_posterior)

plot(theta_grid, prior_scaled,
     type = "l",
     lwd = 2,
     ylim = c(0, 1),
     xlab = expression(theta),
     ylab = "Scaled density",
     main = "Prior, Likelihood, and Posterior")

lines(theta_grid, likelihood_scaled, lwd = 2, lty = 2)
lines(theta_grid, posterior_scaled, lwd = 2, lty = 3)

legend("topright",
       legend = c("Prior", "Likelihood", "Posterior"),
       lty = c(1, 2, 3),
       lwd = 2)

# Step 6. Sample from posterior using Metropolis algorithum ####

## first lets walk through an example of 3-4 iterations ####
plot(theta_grid, posterior_scaled,
     type = "l",
     lwd = 2,
     xlab = expression(theta),
     ylab = "Posterior density (scaled)",
     main = "One Metropolis step on the posterior")

theta_current  <- 0.6   # current position
theta_proposal <- 1.2   # proposed step

post_current  <- approx(theta_grid, posterior_scaled, xout = theta_current)$y
post_proposal <- approx(theta_grid, posterior_scaled, xout = theta_proposal)$y

## add current (Starting) point
points(theta_current, post_current,
       pch = 19, col = "blue", cex = 2)

segments(theta_current, 0,
         theta_current, post_current,
         col = "blue", lty = 2)

## add proposed step
points(theta_proposal, post_proposal,
       pch = 19, col = "red", cex = 2)

segments(theta_proposal, 0,
         theta_proposal, post_proposal,
         col = "red", lty = 2)
arrows(theta_current, post_current,
       theta_proposal, post_proposal,
       length = 0.1,
       col = "darkgray",
       lwd = 2)
legend("topleft",
       legend = c("Posterior", "Current position", "Proposed position"),
       lwd = c(2, NA, NA),
       pch = c(NA, 19, 19),
       col = c("black", "blue", "red"))

## should we move there??

### second iteration
theta_current <- theta_proposal
theta_proposal <- theta_current-0.2
post_current  <- approx(theta_grid, posterior_scaled, xout = theta_current)$y
post_proposal <- approx(theta_grid, posterior_scaled, xout = theta_proposal)$y

## add current (Starting) point
points(theta_current, post_current,
       pch = 19, col = "green", cex = 2)

segments(theta_current, 0,
         theta_current, post_current,
         col = "blue", lty = 2)

## add proposed step
points(theta_proposal, post_proposal,
       pch = 19, col = "red", cex = 2)

segments(theta_proposal, 0,
         theta_proposal, post_proposal,
         col = "red", lty = 2)
arrows(theta_current, post_current,
       theta_proposal, post_proposal,
       length = 0.1,
       col = "darkgray",
       lwd = 2)

### third iteration
theta_current <- 1.2
theta_proposal <- theta_current+.6
post_current  <- approx(theta_grid, posterior_scaled, xout = theta_current)$y
post_proposal <- approx(theta_grid, posterior_scaled, xout = theta_proposal)$y

## add current (Starting) point
points(theta_current, post_current,
       pch = 19, col = "green", cex = 2)

segments(theta_current, 0,
         theta_current, post_current,
         col = "blue", lty = 2)

## add proposed step
points(theta_proposal, post_proposal,
       pch = 19, col = "red", cex = 2)

segments(theta_proposal, 0,
         theta_proposal, post_proposal,
         col = "red", lty = 2)
arrows(theta_current, post_current,
       theta_proposal, post_proposal,
       length = 0.1,
       col = "darkgray",
       lwd = 2)

# should we accept?
points(theta_proposal, post_proposal,
       pch = 19, col = "green", cex = 2)


## make trace plot
plot(c(.6, 1.2, 1.2, 1.8), type = "l",
     xlab = "Iteration",
     ylab = expression(theta),
     main = "Metropolis trace plot")


## setup one MCMC chain with the Metropolis algorithum ####
# setup likelihoods on log scale
log_posterior <- function(theta, y) {
  log_lik <- sum(dnorm(y, mean = theta, sd = 1, log = TRUE))
  log_prior <- dnorm(theta, mean = 0, sd = 5, log = TRUE)
  log_lik + log_prior
}

n_iter <- 5000
theta <- numeric(n_iter)

theta[1] <- .6          # starting value
proposal_sd <- 0.5     # step size


## sampler
for (t in 2:n_iter) {
  
  # Current value
  theta_current <- theta[t - 1]
  
  # Propose a new value
  theta_proposal <- rnorm(1, mean = theta_current, sd = proposal_sd) # grabs 1 random value centered at the current value 
                                                                    # with a "step" size (ie. sd) of 0.4, which we set earlier 
                                                                    # (can make this larger/smaller)
                                                                    # it could be above or below the current value
  # Compute log acceptance ratio
  log_r <- log_posterior(theta_proposal, y) -
    log_posterior(theta_current, y)
  
  # Accept or reject
  if (log(runif(1)) < log_r) {
    theta[t] <- theta_proposal
  } else {
    theta[t] <- theta_current
  }
}

## plot trace plot of the chain
plot(theta, type = "l",
     xlab = "Iteration",
     ylab = expression(theta),
     main = "Metropolis trace plot")


# posterior samples (draws)
hist(theta, breaks = 40, probability = TRUE,
     xlab = expression(theta), xlim=c(-0,3),
     main = "Posterior samples from Metropolis")

abline(v = mean(y), col = "blue", lwd = 2)   # MLE
abline(v = mean(theta), col = "red", lwd = 2)


# overlay posterior
theta_grid <- seq(min(theta), max(theta), length.out = 1000)

likelihood <- function(th) prod(dnorm(y, mean = th, sd = 1))
prior_density <- dnorm(theta_grid, mean = 0, sd = 5)

unnormalized_posterior <- sapply(theta_grid, likelihood) * prior_density
posterior_scaled <- unnormalized_posterior / max(unnormalized_posterior)
lines(theta_grid, posterior_scaled, lwd = 2)

accept_rate <- mean(diff(theta) != 0)
accept_rate


## summarize posterior draws ####
post_draws <- theta[-(1:1000)] ## discard first few samples as "burnin" or warmup because the algorithum needs to explore and learn the posterior distribution

summary_stats <- data.frame(
  Mean   = mean(post_draws),
  Median = median(post_draws),
  CI_2.5 = quantile(post_draws, 0.025),
  CI_97.5 = quantile(post_draws, 0.975)
)

summary_stats
## centrality: mean and median, often we use the median b/c less sensitive to skew (in this case they are nearly identical)
## 95% credible intervals -- ie. 95% probability that the parameter lies within this interval, given the data and model

## plot posterior draws and summary stats
hist(post_draws,
     breaks = 30,
     probability = TRUE,
     main = "Posterior distribution of theta",
     xlab = expression(theta))

abline(v = post_mean, col = "blue", lwd = 2) # mean
abline(v = post_median, col = "red", lwd = 2) # median
abline(v = post_ci_95, col = "darkgray", lwd = 2, lty = 2) # credible intervals

## compare to mean of the "data" we have
summary(y)
mean(y)-(sd(y)/sqrt(20))*1.96 ## lower 95% CONFIDENCE Interval
mean(y)+(sd(y)/sqrt(20))*1.96 ## upper 95% CONFIDENCE Interval


## run model in brms ####
ydat <- as.data.frame(y)
library(brms)

bmod <- brm(y ~ 1,
    prior=prior(normal(0,5), class="Intercept"), # set our priors, same as above
    family=gaussian,
    data=ydat)

summary(bmod) # summary of brms model
summary_stats # compare to our chain

plot(bmod) # plot posterior draws and trace plot
