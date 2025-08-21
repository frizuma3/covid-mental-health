# EXPLORATORY DATA ANALYSIS: DIAGNOSIS TRENDS (PSYCHIATRIC FOCUS)

# 1. Load Required Libraries
library(tidyverse)  # For data manipulation, wrangling, and ggplot2 plotting
library(janitor)    # For clean_names() and other column cleaning utilities
library(scales)     # For formatting axes (e.g., commas on y-axis of plots)
library(viridis)    # For colour palettes

# 2. Load and Clean Data 
df <- read_csv("Data/mhia_diagnosistrends_2024.csv") %>%
  clean_names()    # Standardise column names to lower_case_snake_case

# Exclude rows with "Scotland" in geography1 or geography2
df <- df %>%
  filter(
    # Remove any record that aggregates to "Scotland" for clean NHS Board analysis only
    !str_detect(geography1, regex("Scotland", ignore_case = TRUE)),
    !str_detect(geography2, regex("Scotland", ignore_case = TRUE))
  )

# 3. Standardise and Filter Time Window
df <- df %>%
  mutate(
    # Convert 'year' to integer (handles both YYYY and YYYY/YYYY formats)
    year = ifelse(
      str_detect(as.character(year), "/"),  # If 'year' contains a slash (e.g. 2018/2019)
      as.integer(str_sub(as.character(year), 1, 4)),  # Use the first four digits as the year
      as.integer(year)                       # Otherwise, convert as integer directly
    )
  ) %>%
  filter(year >= 2018, year <= 2023)         # Keep only analysis window (2018-2023)

# 4. Assign COVID Periods
df <- df %>%
  mutate(
    # Create categorical variable for COVID period: pre, during, post
    covid_period = case_when(
      year %in% c(2018, 2019) ~ "Pre-COVID",
      year %in% c(2020, 2021) ~ "COVID-19",
      year %in% c(2022, 2023) ~ "Post-COVID"
    ),
    covid_period = factor(covid_period, levels = c("Pre-COVID", "COVID-19", "Post-COVID"))
  )

# 5. Check Measures and Define Patient-Related Measures
unique(df$measure)  # Review available measure types for validation

measures_patdis <- c(
  "Number of discharges", "Crude rate of discharges (per 100,000 population)",
  "Number of patients", "Crude rate of patients (per 100,000 population)",
  "Number of hospital residents", "Crude rate of hospital residents (per 100,000 population)"
)
# Only use measures relevant to patient trends for downstream analysis

# PSYCHIATRIC PATIENTS: TRENDS AND DISTRIBUTIONS
# 6. Psychiatric Patients per Year
psy_year <- df %>%
  filter(dataset == "Psychiatric", measure == "Number of patients") %>%
  group_by(year) %>%
  summarise(total_patients = sum(value, na.rm = TRUE), .groups = "drop")  # Aggregate total per year

# Plot: Bar chart of psychiatric patients per year
ggplot(psy_year, aes(x = factor(year), y = total_patients, fill = factor(year))) +
  geom_col(show.legend = FALSE) +
  labs(title = "Psychiatric Patients by Year", x = "", y = "Total Psychiatric Patients") +
  theme_minimal() +
  scale_y_continuous(labels = label_comma()) +  # Format y-axis with commas
  scale_fill_viridis_d(option = "C")

# 7. Psychiatric Patients per COVID Period 
psy_covid <- df %>%
  filter(dataset == "Psychiatric", measure == "Number of patients") %>%
  group_by(covid_period) %>%
  summarise(total_patients = sum(value, na.rm = TRUE), .groups = "drop")  # Aggregate by period

# Plot: Psychiatric patient totals by COVID period
ggplot(psy_covid, aes(x = covid_period, y = total_patients, fill = covid_period)) +
  geom_col(show.legend = FALSE) +
  labs(title = "Psychiatric Patients across COVID Periods", x = "", y = "Total Psychiatric Patients") +
  theme_minimal() +
  scale_y_continuous(labels = label_comma()) +
  scale_fill_viridis_d(option = "C")

# 8. Psychiatric Patients by Diagnosis (Total, Year, Period) 
psy_diag <- df %>%
  filter(dataset == "Psychiatric", measure == "Number of patients")

# By diagnosis (total across all years/periods)
ggplot(
  psy_diag %>% group_by(diagnosis_groupings) %>%
    summarise(total_patients = sum(value, na.rm = TRUE), .groups = "drop"),
  aes(x = reorder(diagnosis_groupings, -total_patients), y = total_patients, fill = diagnosis_groupings)
) +
  geom_col(show.legend = FALSE) +
  labs(title = "Psychiatric Patients by Diagnosis", x = "Diagnosis Grouping", y = "Total Patients") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) + # Rotated x labels
  scale_y_continuous(labels = label_comma()) +
  scale_fill_viridis_d(option = "D")

# By diagnosis and year (for time trend visualisation)
ggplot(
  psy_diag %>% group_by(year, diagnosis_groupings) %>%
    summarise(total_patients = sum(value, na.rm = TRUE), .groups = "drop"),
  aes(x = factor(year), y = total_patients, fill = diagnosis_groupings)
) +
  geom_col(position = "dodge") +
  labs(title = "Psychiatric Patients by Diagnosis and Year", x = "", y = "Total Patients", fill = "Diagnosis") +
  theme_minimal() +
  scale_y_continuous(labels = label_comma()) +
  scale_fill_viridis_d(option = "D")

# By diagnosis and COVID period (for COVID impact per diagnostic group)
ggplot(
  psy_diag %>% group_by(covid_period, diagnosis_groupings) %>%
    summarise(total_patients = sum(value, na.rm = TRUE), .groups = "drop"),
  aes(x = covid_period, y = total_patients, fill = diagnosis_groupings)
) +
  geom_col(position = "dodge") +
  labs(title = "Psychiatric Patients by Diagnosis and COVID Period", x = "", y = "Total Patients", fill = "Diagnosis") +
  theme_minimal() +
  scale_y_continuous(labels = label_comma()) +
  scale_fill_viridis_d(option = "D")

# 9. Psychiatric Patients by Diagnosis and Geography 
# Show diagnostic breakdown across NHS Boards/regions
ggplot(
  psy_diag %>% group_by(diagnosis_groupings, geography2) %>%
    summarise(total_patients = sum(value, na.rm = TRUE), .groups = "drop"),
  aes(x = geography2, y = total_patients, fill = diagnosis_groupings)
) +
  geom_col(position = "dodge") +
  labs(title = "Psychiatric Patients by Diagnosis and Geography", 
       x = "NHS Regions", 
       y = "Total Patients(Log Scale)",
       fill= "Psychiatric Diagnosis") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = label_comma()) +
  scale_y_log10(labels = label_comma()) + 
  scale_fill_viridis_d(option = "C")

# 10. Psychiatric vs Total Patients (Year & COVID Period) 
# Compare psychiatric with total (all) patients by year
df %>%
  filter(measure == "Number of patients", dataset %in% c("Psychiatric", "Total")) %>%
  group_by(year, dataset) %>%
  summarise(total_patients = sum(value, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = factor(year), y = total_patients, fill = dataset)) +
  geom_col(position = "dodge") +
  labs(title = "Psychiatric vs Total Patients by Year", x = "", y = "Number of Patients", fill = "Dataset") +
  theme_minimal() +
  scale_y_continuous(labels = label_comma()) +
  scale_fill_viridis_d(option = "D")

# By COVID period (not just by year)
df %>%
  filter(measure == "Number of patients", dataset %in% c("Psychiatric", "Total")) %>%
  group_by(covid_period, dataset) %>%
  summarise(total_patients = sum(value, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = covid_period, y = total_patients, fill = dataset)) +
  geom_col(position = "dodge") +
  labs(title = "Psychiatric vs Total Patients by COVID Period", 
       x = "", y = "Number of Patients", 
       fill = "Dataset") +
  theme_minimal() +
  scale_y_continuous(labels = label_comma()) +
  scale_fill_viridis_d(option = "C")

# 11. Key Statistics: Top Year & Top Diagnosis for Psychiatric Patients 
cat("Year with most psychiatric patients:\n")
df %>%
  filter(dataset == "Psychiatric", measure == "Number of patients") %>%
  group_by(year) %>%
  summarise(total_patients = sum(value, na.rm = TRUE)) %>%
  # arrange(desc(total_patients)) %>%   # Optionally arrange from most to least
  slice(1:6) %>%                      # Print the top 6 (could change to top_n if sorted)
  print()

cat("Most common diagnosis among psychiatric patients:\n")
df %>%
  filter(dataset == "Psychiatric", measure == "Number of patients") %>%
  group_by(diagnosis_groupings) %>%
  summarise(total_patients = sum(value, na.rm = TRUE)) %>%
  arrange(desc(total_patients)) %>%
  slice(1) %>%       # Only the most common
  print()

# 12. Save Cleaned and Filtered Data for further analysis & Reporting 
write_csv(df, "Data/mhia_diagnosistrends_dataset.csv") # Save final tidy dataset for use in Shiny or further analysis

