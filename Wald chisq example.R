library(car)
library(glmmTMB)
library(tidyverse)

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


modelf <- aov(value ~ group, data=df)
summary(modelf)
Anova(modelf)

modelx2 <- glmmTMB(value ~ group, data=df)
summary(modelx2)
Anova(modelx2)




2^2/sd(df$value)

model <- glmmTMB(value ~ group, data=df)
summary(model)
Anova(model, test = "Chisq")

x2 <- modelx2 <- (2/.6667)^2
# p-value 
1-pchisq(x2, 1)


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
