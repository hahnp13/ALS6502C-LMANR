library(car)
library(glmmTMB)
library(tidyverse)
library(emmeans)

# Create a data frame
df <- data.frame(
  group = factor(rep(c("one", "two"), each = 3)),  # Categorical predictor
  value = c(2,3,4,4,5,6)  # Response variable
)

df %>% group_by(group) %>% summarize(mean=mean(value))

# make plot
ggplot(df, aes(x=group, y=value))+
  geom_boxplot()+
  geom_point(size=5)+
  theme_bw(base_size = 20)


modelf <- lm(value ~ group, data=df)
summary(modelf)
Anova(modelf)

modelx2 <- glmmTMB(value ~ group, data=df)
summary(modelx2)
Anova(modelx2)


x2 <- modelx2 <- (2/.6667)^2
# p-value 
1-pchisq(x2, 1)


## calucate difference using likelihood manually 
one <- c(2, 3, 4)
two <- c(4, 5, 6)
log_likelihood <- function(par, y1, y2) {
  mu1 <- par[1]
  mu2 <- par[2]
  sigma <- exp(par[3])  # enforce positivity
  
  ll1 <- sum(dnorm(y1, mean = mu1, sd = sigma, log = TRUE))
  ll2 <- sum(dnorm(y2, mean = mu2, sd = sigma, log = TRUE))
  
  ll1 + ll2
}

start <- c(
  mu1 = mean(one),
  mu2 = mean(two),
  log_sigma = log(sd(c(one, two)))
)

# optimioze
fit <- optim(
  par = start,
  fn = log_likelihood,
  y1 = one,
  y2 = two,
  control = list(fnscale = -1),  # maximize
  hessian = TRUE
)


mu1_hat <- round(fit$par[1], 3)
mu2_hat <- round(fit$par[2], 3)
sigma_hat <- round(exp(fit$par[3]), 3)

delta_hat <- mu2_hat - mu1_hat

mu1_hat
mu2_hat
delta_hat

# extract SE
vcov_mat <- solve(fit$hessian)
vcov_mat
var_delta <- vcov_mat[2, 2] +
  vcov_mat[1, 1] -
  2 * vcov_mat[1, 2]

se_delta <- sqrt(abs(var_delta)) # need abs here for non-postive definite hessian due to small sample size
se_delta

# z test
z_val <- round((delta_hat / se_delta), 3)

p_valz <- 2 * (1 - pnorm(z_val))
p_valz

x2_val <- z_val^2
p_valx2 <- pchisq(x2_val, 1, lower.tail=F)
p_valx2

## visualize normal density
# Create a sequence of x values
x <- seq(0, 8, length.out = 400)

# Parameters
mu1 <- 3
mu2 <- 5
sd  <- 1.140175

# Compute densities
dens1 <- dnorm(x, mean = mu1, sd = sd)
dens2 <- dnorm(x, mean = mu2, sd = sd)

# Plot first normal density
ll1p <- (dnorm(y1, mean = mu1, sd = sd, log = F))
ll2p <- (dnorm(y2, mean = mu2, sd = sd, log = F))
plot(x, dens1,
     type = "l",
     lwd = 2,
     xlab = "group one",
     ylab = "Density",
     main = "Normal Density (mean = 3, sd = 4.86)")
points(x=y1, y=ll1p, cex=2)

# Plot second normal density
plot(x, dens2,
     type = "l",
     lwd = 2,
     xlab = "group two",
     ylab = "Density",
     main = "Normal Density (mean = 5, sd = 4.86)")
points(x=y2, y=ll2p, cex=2)


## visualize likelihood function
delta_grid <- seq(0, 4, length.out = 200)

profile_ll <- sapply(delta_grid, function(d) {
  mu1 <- mean(one)
  mu2 <- mu1 + d
  sigma <- sd(c(one, two))
  
  sum(dnorm(one, mu1, sigma, log = TRUE)) +
    sum(dnorm(two, mu2, sigma, log = TRUE))
})

plot(delta_grid, profile_ll,
     type = "l",
     xlab = "Difference in means (mu2 - mu1)",
     ylab = "Log-likelihood",
     main = "Likelihood for difference in means")


## visualizing z 
x <- seq(-4, 4, length.out = 500)
y <- dt(x, df = 1)
t_crit <- qt(1 - .05/2, df = 4)

plot(x, y, type = "l",
     main = "z distribution",
     xlab = "z value",
     ylab = "Density")

# Observed statistic
abline(v = 3, col = "red", lwd = 2)

abline(v = c(-t_crit, t_crit), lty = 2) # crit values

abline(v = z_val, col = "red", lwd = 2)
#abline(v = -z_val, col = "red", lwd = 2, lty = 2)


## chi sq
x <- seq(0, 10, length.out = 500)
y <- dchisq(x, df = 1)
chi_crit <- qchisq(1 - 0.05, df = 1)

plot(x, y, type = "l",
     main = expression(chi^2~"distribution (df = 1)"),
     xlab = "chi-sq value",
     ylab = "Density")

abline(v = chi_crit, lty = 2)
abline(v = x2_val, col = "red", lwd = 2)

## F distribution
x <- seq(0, 10, length.out = 500)
y <- df(x, df1 = 1, df2 = 4)
F_crit <- qf(1 - 0.05, df1 = 1, df2 = 4)

plot(x, y, type = "l",
     main = "F distribution (df = 1,4)",
     xlab = "F value",
     ylab = "Density")

abline(v = F_crit, lty = 2)
abline(v = 6, col = "red", lwd = 2)

1-pf(6, 1, 4) # pval from F test

## three groups
df3 <- data.frame(
  group = factor(rep(c("one", "two", "three"), each = 3)),  # Categorical predictor
  value = c(2,3,4,4,5,6,1,2,3)  # Response variable
)

modelf <- aov(value ~ group, data=df3)
summary(modelf)
Anova(modelf)

modelx2 <- glmmTMB(value ~ group, data=df3)
summary(modelx2)
Anova(modelx2)

x2 <- modelx2 <- (3/.6666667)^2
x2
# p-value 
pchisq(x2, 2, lower.tail=F)

# Load rmodel# Load required package
if (!require(stats4)) install.packages("stats4", dependencies = TRUE)
library(stats4)

# Define data
one <- c(2, 3, 4)
two <- c(4, 5, 6)

# Negative log-likelihood function for normal distribution
neg_log_likelihood <- function(mu, sigma) {
  if (sigma <= 0) return(Inf)  # Sigma must be positive
  log_lik_one <- sum(dnorm(one, mean = mu, sd = sigma, log = TRUE))
  log_lik_two <- sum(dnorm(two, mean = mu, sd = sigma, log = TRUE))
  return(- (log_lik_one + log_lik_two))  # Negative log-likelihood
}

# MLE estimation using optim
mle_results <- optim(par = c(mean(c(one, two)), sd(c(one, two))), 
                     fn = function(params) neg_log_likelihood(params[1], params[2]), 
                     method = "L-BFGS-B", lower = c(-Inf, 0.0001))

# Extract MLE estimates
mle_mu <- mle_results$par[1]
mle_sigma <- mle_results$par[2]

# Print results
cat("MLE Estimates:\n")
cat("Mean (mu):", mle_mu, "\n")
cat("Standard Deviation (sigma):", mle_sigma, "\n")

# two group sample size comparison ####

# Set seed for reproducibility
set.seed(11)

# Simulate data
n <- 150  # number of observations per group
n1 <- 1000  # number of observations per group
group <- rep(c("Control", "Treatment"), each = n)
value <- c(rnorm(n, mean = 5, sd = 1),   # Control group
           rnorm(n, mean = 6, sd = 1))   # Treatment group

# Put into a data frame
data <- data.frame(group = factor(group), value = value)

group1 <- rep(c("Control", "Treatment"), each = n1)
value1 <- c(rnorm(n1, mean = 5, sd = 1),   # Control group
           rnorm(n1, mean = 6, sd = 1))   # Treatment group

# Put into a data frame
data1 <- data.frame(group1 = factor(group1), value1 = value1)

ggplot(data, aes(x=group, y=value))+
  geom_boxplot()+
  geom_jitter(height=0, width=.2)+
  theme_bw(base_size = 20)

ggplot(data1, aes(x=group1, y=value1))+
  geom_boxplot()+
  geom_jitter(height=0, width=.2)+
  theme_bw(base_size = 20)

# Fit linear model
model <- lm(value ~ group, data = data)
summary(model)

model1 <- lm(value1 ~ group1, data = data1)
summary(model1)

m <- glmmTMB(value ~ group, data = data)
summary(m)
Anova(m)
em_m <- emmeans(m, ~group)
eff_size(em_m, sigma = sigma(m), edf = df.residual(m))


m1 <- glmmTMB(value1 ~ group1, data = data1)
summary(m1)
Anova(m1)
em_m1 <- emmeans(m1, ~group1)
eff_size(em_m1, sigma = sigma(m1), edf = df.residual(m1))

# regression
# Set seed for reproducibility
set.seed(123)

# Generate independent variable x
x <- seq(-10, 10, length.out = 100)

# Define true quadratic relationship with some coefficients
y_true <- 3 + 2 * x - 0.5 * x^2

# Add random noise to simulate real-world data
noise <- rnorm(length(x), mean = 0, sd = 5)
y <- y_true + noise

# Create a data frame
data <- data.frame(x = x, y = y)

# Plot the data
plot(data$x, data$y, main = "Simulated Quadratic Data", xlab = "x", ylab = "y", pch = 19, col = "blue")

# Generate independent variable x
x1 <- seq(-10, 10, length.out = 1000)

# Define true quadratic relationship with some coefficients
y_true1 <- 3 + 2 * x1 - 0.5 * x1^2

# Add random noise to simulate real-world data
noise1 <- rnorm(length(x1), mean = 0, sd = 5)
y1 <- y_true1 + noise1

# Create a data frame
data1 <- data.frame(x1 = x1, y1 = y1)

# Plot the data
plot(data1$x1, data1$y1, main = "Simulated Quadratic Data", xlab = "x", ylab = "y", pch = 19, col = "blue")


r1 <- lm(y ~ x + I(x^2), data=data)
summary(r1)
r2 <- lm(y1 ~ x1 + I(x1^2), data=data1)
summary(r2)

r1emt <- emtrends(r1, ~1, var="x")
eff_size(r1emt, sigma=sigma(r1), edf=df.residual(r1))

