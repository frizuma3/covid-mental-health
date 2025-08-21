# INTERRUPTED TIME SERIES & MIXED EFFECTS REGRESSION 

#1. LIBRARIES AND DATA IMPORT 
library(tidyverse)     # Data wrangling, ggplot2 plotting
library(janitor)       # Clean column names
library(dygraphs)      # Interactive time series plots
library(xts)           # Time series objects for dygraphs
library(lme4)          # Mixed-effects modelling (lmer)
library(broom.mixed)   # Tidy model summaries

# Load datasets
df_age_sex <- read_csv("Data/mhia_agesex_dataset.csv") %>% clean_names()
df_datatrends <- read_csv("Data/mhia_datatrends_dataset.csv") %>% clean_names()
df_diagnosistrends <- read_csv("Data/mhia_diagnosistrends_dataset.csv") %>% clean_names()

# 2. ITS DATASET: NATIONAL ANNUAL AGGREGATION
# Aggregate psychiatric inpatients at national level (by year & period)
df_its_scotland <- df_age_sex %>%
  filter(dataset == "Psychiatric", measure == "Number of patients") %>%
  group_by(year, period) %>%
  summarise(total_patients = sum(value, na.rm = TRUE), .groups = "drop") %>%
  arrange(year) %>%
  mutate(
    time = year - min(year) + 1,
    covid_indicator = if_else(year >= 2020, 1, 0),  # 0 before COVID, 1 after/beginning 2020
    post_covid_time = if_else(year >= 2020, time - sum(year < 2020), 0)
  )

# 3. VISUALISATION: NATIONAL TIME SERIES (STATIC & INTERACTIVE)
ggplot(df_its_scotland, aes(x = year, y = total_patients)) +
  geom_line(linewidth = 1.2, color = "#38598B") +
  geom_point(size = 2) +
  geom_vline(xintercept = 2020, linetype = "dashed", color = "red") +
  labs(title = "Total Psychiatric Patients in Scotland (2018–2023)",
       x = "", y = "Total Psychiatric Patients") +
  theme_minimal()

ts_scotland <- xts(df_its_scotland$total_patients, order.by = as.Date(paste0(df_its_scotland$year, "-01-01")))
dygraph(ts_scotland, main = "Total Psychiatric Patients in Scotland (2018–2023)") %>%
  dyAxis("y", label = "Total Patients") %>%
  dyEvent("2020-01-01", "COVID-19", labelLoc = "bottom", color = "red") %>%
  dyRangeSelector()

# 4. ITS MODEL: SEGMENTED REGRESSION
its_model <- lm(total_patients ~ time + covid_indicator + post_covid_time, data = df_its_scotland)
summary(its_model)
df_its_scotland$predicted <- predict(its_model)

# Overlay: Observed vs Predicted
ggplot(df_its_scotland, aes(x = year)) +
  geom_line(aes(y = total_patients), color = "#38598B", linewidth = 1.2) +
  geom_point(aes(y = total_patients), color = "#38598B") +
  geom_line(aes(y = predicted), color = "red", linetype = "dashed", linewidth = 1.2) +
  geom_point(aes(y = predicted), color = "red", shape = 17, size = 2) +
  geom_vline(xintercept = 2020, linetype = "dotted", color = "red", size = 1) +
  labs(title = "ITS: Observed vs Predicted Psychiatric Patients in Scotland",
       x = "", y = "Number of Patients") +
  theme_minimal()

# Confidence intervals on prediction
ci <- predict(its_model, interval = "confidence")
df_its_scotland$ci_lower <- ci[, "lwr"]
df_its_scotland$ci_upper <- ci[, "upr"]

ggplot(df_its_scotland, aes(x = year)) +
  geom_ribbon(aes(ymin = ci_lower, ymax = ci_upper), fill = "pink", alpha = 0.3) +
  geom_line(aes(y = predicted), color = "red", linetype = "dashed", linewidth = 1.2) +
  geom_line(aes(y = total_patients), color = "#38598B", linewidth = 1.2) +
  geom_point(aes(y = total_patients), color = "#38598B") +
  geom_vline(xintercept = 2020, linetype = "dotted", color = "red", size = 1) +
  labs(title = "ITS: Observed vs Predicted with Confidence Interval",
       x = "", y = "Number of Patients") +
  theme_minimal()

# Interactive overlay plot
ts_combined <- cbind(
  Observed = xts(df_its_scotland$total_patients, order.by = as.Date(paste0(df_its_scotland$year, "-01-01"))),
  Predicted = xts(df_its_scotland$predicted, order.by = as.Date(paste0(df_its_scotland$year, "-01-01")))
)
dygraph(ts_combined, main = "Observed vs Predicted Psychiatric Patients (ITS Model)") %>%
  dySeries("Observed", color = "#38598B") %>%
  dySeries("Predicted", color = "red") %>%
  dyEvent("2020-01-01", "COVID-19", labelLoc = "bottom", color = "red") %>%
  dyRangeSelector()

#5. MODEL DIAGNOSTICS: RESIDUALS, LEVERAGE, HETEROSCEDASTICITY
par(mfrow = c(2,2)); plot(its_model); par(mfrow = c(1,1))

# 6. MIXED EFFECTS REGRESSION: REGIONAL VARIATION
# Create region-level ITS dataset for mixed model
df_its_region <- df_age_sex %>%
  filter(dataset == "Psychiatric", measure == "Number of patients") %>%
  group_by(year, geography2, period) %>%
  summarise(total_patients = sum(value, na.rm = TRUE), .groups = "drop") %>%
  arrange(geography2, year) %>%
  mutate(
    time = year - min(year) + 1,
    covid_indicator = if_else(year >= 2020, 1, 0),
    post_covid_time = if_else(year >= 2020, time - sum(year < 2020), 0)
  )

# Random intercept & slope for time by region (best for heterogeneity)
me_model_slope <- lmer(
  total_patients ~ time + covid_indicator + post_covid_time + (time | geography2),
  data = df_its_region
)
summary(me_model_slope)

# Add predictions, plot observed vs predicted for each region
df_its_region$pred <- predict(me_model_slope)
ggplot(df_its_region, aes(x = year, y = total_patients, color = geography2)) +
  geom_line(size = 1) +
  geom_line(aes(y = pred), linetype = "dashed", size = 1.2) +
  facet_wrap(~ geography2, scales = "free_y") +
  labs(title = "Observed vs Predicted Psychiatric Patients by Region",
       x = "Year", y = "Number of Patients") +
  theme_minimal() +
  theme(legend.position = "none")

# Region × COVID interaction model (tests if COVID effects differ by region)
me_model_region_covid <- lmer(
  total_patients ~ time + covid_indicator * geography2 + post_covid_time + (1 | geography2),
  data = df_its_region
)
summary(me_model_region_covid)

# Export coefficients for reporting/tables
write_csv(tidy(me_model_slope), "mixed_effects_coefficients.csv")

# Final diagnostics for mixed model
plot(me_model_slope)



