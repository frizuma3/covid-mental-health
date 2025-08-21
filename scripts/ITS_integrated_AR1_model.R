
# INTERRUPTED TIME SERIES & MIXED EFFECTS REGRESSION with AR(1) autocorrelation modelling

# Required LIbraries
library(tidyverse)
library(janitor)
library(dygraphs)
library(xts)
library(lme4)
library(nlme)         # Required for gls/lme AR(1)
library(broom.mixed)

# 1. Load data
df_age_sex <- read_csv("Data/mhia_agesex_dataset.csv") %>% clean_names()

# 2. ITS data: National aggregation
df_its_scotland <- df_age_sex %>%
  filter(dataset == "Psychiatric", measure == "Number of patients") %>%
  group_by(year, period) %>%
  summarise(total_patients = sum(value, na.rm = TRUE), .groups = "drop") %>%
  arrange(year) %>%
  mutate(
    time = year - min(year) + 1,
    covid_indicator = if_else(year >= 2020, 1, 0),
    post_covid_time = if_else(year >= 2020, time - sum(year < 2020), 0)
  )

# 3. Standard (OLS) ITS model for reference
its_model_lm <- lm(total_patients ~ time + covid_indicator + post_covid_time, data = df_its_scotland)
summary(its_model_lm)

# 4. ITS with AR(1) autocorrelation (using nlme::gls)
its_model_ar1 <- gls(
  total_patients ~ time + covid_indicator + post_covid_time,
  data = df_its_scotland,
  correlation = corAR1(form = ~ time)  # models autocorrelation in errors
)
summary(its_model_ar1)

# Add AR1-predicted values to your results frame
df_its_scotland$predicted_ar1 <- predict(its_model_ar1)

# Compare OLS vs AR1 fit visually
ggplot(df_its_scotland, aes(x = year)) +
  geom_line(aes(y = total_patients), color = "#38598B", linewidth = 1.2) +
  geom_point(aes(y = total_patients), color = "#38598B") +
  geom_line(aes(y = predicted_ar1), color = "green", linetype = "dashed", linewidth = 1.2) +
  geom_line(aes(y = predict(its_model_lm)), color = "red", linetype = "dotted", linewidth = 1.2) +
  geom_vline(xintercept = 2020, linetype = "dotted", color = "red", size = 1) +
  labs(
    title = "ITS: Observed vs Predicted (OLS vs AR1) Psychiatric Patients in Scotland",
    x = "", y = "Number of Patients"
  ) +
  theme_minimal()

# 5. Check AR1 autocorrelation parameter estimate
ar1_rho <- coef(its_model_ar1$modelStruct$corStruct, unconstrained = FALSE)
cat("Estimated AR(1) autocorrelation parameter (rho):", round(ar1_rho, 3), "\n")

# 6. Model diagnostics for AR(1)
par(mfrow = c(2,2))
plot(its_model_ar1)
par(mfrow = c(1,1))

# Residual ACF plot
acf(residuals(its_model_ar1), main = "Residual ACF (AR1 Model)")

