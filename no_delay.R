library(brms)
# Bayesian version of an lmer model
# Example: DepDelayMinutes ~ 1 + (1 | Operating_Airline) + (1 | Month)
bayesian_model <- brm(
  DepDelayMinutes ~ 1 + (1 | Operating_Airline) + (1 | Month),
  data = no_delay,
  family = poisson(),
  prior = c(
    set_prior("normal(0, 5)", class = "sd"),
    set_prior("normal(0, 5)", class = "Intercept")
  ),
  chains = 2,
  iter = 1000,
  warmup = 200,
  seed = 123
)




formula <- DepDelayMinutes ~ 1 + (1 | Operating_Airline) + (1 | Month)
available_priors <- get_prior(formula, data = no_delay, family = poisson())
print(available_priors)

summary(bayesian_model)
plot(bayesian_model)
