library(brms)
library(ggplot2)
library(reshape2)
library(dplyr)

formula <- DepDelayMinutes ~ 1 + temp + humidity + windgust + visibility + (1 | Operating_Airline) + (1 | Month)
available_priors <- get_prior(formula, data = no_delay, family = poisson())
print(available_priors)

my_model <- brm(
  formula = DepDelayMinutes ~ temp + humidity + visibility + windgust + (1 | Month) + (1 | Operating_Airline),
  data = no_delay, 
  family = poisson(),  
  prior = c(
    set_prior("normal(0, 25)", class = "b", coef = "temp"),       # Normal prior for temperature
    set_prior("normal(0, 25)", class = "b", coef = "humidity"),   # Normal prior for humidity
    set_prior("normal(0, 25)", class = "b", coef = "visibility"), # Normal prior for visibility
    set_prior("normal(0, 25)", class = "b", coef = "windgust"),   # Normal prior for windgust
    set_prior("student_t(3, 2.6, 2.5)", class = "Intercept"),   
    set_prior("student_t(3, 0, 2.5)", class = "sd", group = "Month"),                # SD for Month
    set_prior("student_t(3, 0, 2.5)", class = "sd", group = "Operating_Airline")     # SD for Operating Airline
  ),
  chains = 4,
  iter = 2000,
  control = list(adapt_delta = 0.5)
)

summary(my_model)
plot(my_model)


predicted <- predict(my_model, newdata = no_delay, summary = TRUE, prob = 0.95)
predicted <- as.data.frame(predicted)
CIs <- predict(my_model, newdata = no_delay, summary = FALSE, probs = c(0.025, 0.975))
CIs <- as.data.frame(CIs)
plot_data <- no_delay %>%
  mutate(
    Predicted = predicted$Estimate,  # or use the appropriate column
    CI_lower = CIs[, "2.5%"],              # Assuming CIs are stored in a data frame
    CI_upper = CIs[, "97.5%"]
  )
plot <- ggplot(combined_data, aes(x = Index)) +  # Assuming 'Index' is your identifier for observations
  geom_point(aes(y = DepDelayMinutes), color = "blue", alpha = 0.6, size = 2, show.legend = TRUE, name = "Actual") +
  geom_line(aes(y = Predicted), color = "red", size = 1, show.legend = TRUE, name = "Predicted") +
  geom_ribbon(aes(ymin = CI_lower, ymax = CI_upper), fill = "orange", alpha = 0.2, show.legend = TRUE, name = "95% CI") +
  labs(title = "Predicted vs Actual DepDelayMinutes",
       x = "Observation Index",
       y = "DepDelayMinutes") +
  theme_minimal()

# Print the plot
print(plot)


ggsave("prior_compare.png", plot, width = 10, height = 8, dpi = 300)

posterior <- as.array(my_model)
mcmc_acf(posterior)

