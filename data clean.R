library(dplyr)
set.seed(123)

data <- read.csv("flight_cleaned_lan.csv")

df <- flight_cleaned_lan %>%
  group_by(Operating_Airline) %>%
  filter(n() >= 6000) %>%     
  sample_n(100)               

df <- df %>%
  mutate(Month = case_when(
    Month %in% c(6, 7, 8, 12) ~ "Peak",
    TRUE ~ "Low"
  ))

df <- df %>%
  select(Month, Operating_Airline, temp, humidity, windgust, 
         cloudcover, visibility, precip, volume, DepDelayMinutes)

df <- df %>%
  mutate(DepDelayMinutes = log(DepDelayMinutes + 1))

df <- df %>%
  mutate(DepDelayMinutes = exp(DepDelayMinutes)-1)

  
write.csv(df, "cleaned.csv", row.names = FALSE)


no_delay <- flight_cleaned_lan %>%
  group_by(Operating_Airline) %>%
  filter(n() >= 6000) %>% 
  filter(DepDelayMinutes > 0) %>%
  sample_n(100)  

no_delay <- no_delay %>%
  select(Month, Operating_Airline, temp, humidity, windgust, 
         cloudcover, visibility, precip, volume, DepDelayMinutes)

no_delay <- no_delay %>%
  mutate(Month = case_when(
    Month %in% c(6, 7, 8, 12) ~ "Peak",
    TRUE ~ "Low"
  ))

no_delay <- no_delay %>%
  mutate(DepDelayMinutes = log(DepDelayMinutes + 1))
