library(lme4)
library(gridExtra)
library(lmerTest)
library(ggplot2)
library(reshape2)

df <- df %>%
  mutate(Operating_Airline = as.factor(Operating_Airline),
         Month = as.factor(Month))

tail(df)
summary(df)

#analysis for full data
intercept.only.model <- lmer(DepDelayMinutes ~ 1 + (1 | Operating_Airline), data = df, REML = TRUE)
df$intercept.only.preds <- predict(intercept.only.model)
summary(intercept.only.model)
ranef(intercept.only.model)
ranova(intercept.only.model)

cat.only.model <- lmer(DepDelayMinutes ~ 1 + (1 | Operating_Airline) + (1 | Month), data = df, REML = TRUE)
df$cat.only.preds <- predict(cat.only.model)
summary(cat.only.model)
ranef(cat.only.model)
ranova(cat.only.model)

anova(intercept.only.model, model, test ="Chisq")

full_model <-  lmer(DepDelayMinutes ~ volume + temp + humidity + windgust + cloudcover + visibility + (1 | Month) + (1 | Operating_Airline), 
                                data = df, REML = TRUE)
df$full_model.preds <-  predict(full_model)
summary(full_model)
ranef(full_model)
ranova(full_model)

my_model <- lmer(DepDelayMinutes ~ temp + humidity + windgust + visibility + (1 | Month), 
                      data = df, REML = TRUE)
df$my_model.pred <- predict(my_model)
summary(my_model)
ranef(my_model)
ranova(my_model)



#plotting
predicts <- data.frame(
  Index = 1:length(df$DepDelayMinutes),
  True = df$DepDelayMinutes,
  intercept = df$intercept.only.preds,
  catogorical = df$cat.only.preds,
  full = df$full_model.preds,
  final = df$my_model.pred
)
predicts <-melt(predicts, id.vars = "Index", variable.name = "Type", value.name = "Value")
plot <- ggplot(data = predicts, aes(x = Index, y = Value, color = Type)) +
  geom_point() +
  ggtitle("Comparison of Prediction Models with True Data") +
  xlab("Index") +
  ylab("DepDelayMinutes") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")

print(plot)
ggsave("my_model_comparison_plot_point.png", plot, width = 10, height = 8, dpi = 300)

random.coefficients.model <-  lmer(DepDelayMinutes ~ volume + temp + humidity + windgust + cloudcover  + (1 | Operating_Airline),
                                   data = no_delay, REML = TRUE)

# Assuming no_delay is already loaded with your data similar to df
intercept.only.model <- lmer(DepDelayMinutes ~ 1 + (1 | Operating_Airline), data = no_delay, REML = TRUE)
no_delay$intercept.only.preds <- predict(intercept.only.model)
summary(intercept.only.model)
ranef(intercept.only.model)
ranova(intercept.only.model)

cat.only.model <- lmer(DepDelayMinutes ~ 1 + (1 | Operating_Airline) + (1 | Month), data = no_delay, REML = TRUE)
no_delay$cat.only.preds <- predict(cat.only.model)
summary(cat.only.model)
ranef(cat.only.model)
ranova(cat.only.model)

anova(intercept.only.model, cat.only.model, test ="Chisq")

full_model <- lmer(DepDelayMinutes ~ volume + temp + humidity + windgust + cloudcover + visibility + (1 | Month) + (1 | Operating_Airline), 
                    data = no_delay, REML = TRUE)
no_delay$full_model.preds <-  predict(full_model)
summary(full_model)
ranef(full_model)
ranova(full_model)

my_model <- lmer(DepDelayMinutes ~ temp + humidity + windgust + visibility + (1 | Month), 
                 data = no_delay, REML = TRUE)
no_delay$my_model.pred <- predict(my_model)
summary(my_model)
ranef(my_model)
ranova(my_model)

#plotting
predicts <- data.frame(
  Index = 1:length(no_delay$DepDelayMinutes),
  True = no_delay$DepDelayMinutes,
  intercept = no_delay$intercept.only.preds,
  categorical = no_delay$cat.only.preds,
  full = no_delay$full_model.preds,
  final = no_delay$my_model.pred
)
predicts <- melt(predicts, id.vars = "Index", variable.name = "Type", value.name = "Value")
plot <- ggplot(data = predicts, aes(x = Index, y = Value, color = Type)) +
  geom_point() +
  ggtitle("Comparison of Prediction Models with True Data") +
  xlab("Index") +
  ylab("DepDelayMinutes") +
  theme_minimal() +
  scale_color_brewer(palette = "Set1")

print(plot)
ggsave("my_model_comparison_plot_point_nodelay.png", plot, width = 10, height = 8, dpi = 300)


residuals <- residuals(model, type = "pearson")
fitted_values <- fitted(model)
plot(fitted_values, residuals, xlab = "Fitted Values", ylab = "Residuals")
abline(h = 0, col = "red")  # Adds a horizontal line at zero
acf(residuals)





