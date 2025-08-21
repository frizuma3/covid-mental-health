# EXPLORATORY DATA ANALYSIS: HOSPITAL & TREND DATA (PSYCHIATRIC FOCUS)

# LOAD LIBRARIES 
library(tidyverse)  # For data wrangling and ggplot2 for plotting
library(janitor)    # For clean_names(), making columns snake_case
library(scales)     # For pretty axis labels (e.g., thousands comma-separated)
library(viridis)    # For accessible, consistent colour palettes in ggplot

# LOAD AND CLEAN DATA
df <- read_csv("Data/mhia_datatrends_2024.csv") %>%
  clean_names()  # Ensure column names are consistent, lower case, underscore

# STANDARDISE YEAR COLUMN
df <- df %>%
  mutate(
    year = ifelse(
      str_detect(as.character(year), "/"),              # If year column has a "/" (e.g., 2018/2019)
      as.integer(str_sub(as.character(year), 1, 4)),    # Use the first four digits as the year
      as.integer(year)                                  # Otherwise, treat as integer directly
    )
  )

# FILTER FOR FISCAL YEARS 2018–2023
df <- df %>% filter(year >= 2018, year <= 2023)  # Keep only analysis window relevant to the research

# ASSIGN COVID-19 PERIODS
df <- df %>%
  mutate(
    covid_period = case_when(                         # Create a new variable 'covid_period'
      year %in% c(2018, 2019) ~ "Pre-COVID",
      year %in% c(2020, 2021) ~ "COVID-19",
      year %in% c(2022, 2023) ~ "Post-COVID"
    ),
    covid_period = factor(covid_period, levels = c("Pre-COVID", "COVID-19", "Post-COVID"))  # Set factor order
  )

# FILTER OUT "NHS" OR "Scotland" FROM hospital_name AND hospital_code
df_filt <- df %>%
  filter(
    # Remove any rows where hospital_code or hospital_name is an NHS-wide or Scotland-wide aggregate
    !str_detect(hospital_code, regex("NHS|Scotland", ignore_case = TRUE)),
    !str_detect(hospital_name, regex("NHS|Scotland", ignore_case = TRUE))
  )

# SAVE FILTERED DATASET FOR REPRODUCIBILITY 
write_csv(df_filt, "Data/mhia_datatrends_dataset.csv") # Save for downstream use or reporting

# BASIC DESCRIPTIVE STATISTICS
summary_stats <- df_filt %>%
  filter(str_to_lower(specialty) == "psychiatric", str_to_lower(trend_type) == "patients") %>%
  summarise(
    min_patients = min(value, na.rm = TRUE),
    max_patients = max(value, na.rm = TRUE),
    mean_patients = mean(value, na.rm = TRUE),
    median_patients = median(value, na.rm = TRUE),
    total_patients = sum(value, na.rm = TRUE),
    n_obs = n()
  )
summary_stats  # Displays summary of distribution of psychiatric patient numbers

# TOTAL PSYCHIATRIC PATIENTS PER HOSPITAL
df_filt %>%
  filter(str_to_lower(specialty) == "psychiatric", str_to_lower(trend_type) == "patients") %>%
  group_by(hospital_name) %>%
  summarise(total_patients = sum(value, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_patients)) %>%
  slice_head(n = 15) %>%  # Top 15 hospitals by psychiatric patient count
  ggplot(aes(x = reorder(hospital_name, total_patients), y = total_patients, fill = hospital_name)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +  # Flip axes for readable labels
  labs(
    title = "Top Hospitals for Psychiatric Patients (2018-2023)",
    x = "Hospital Name",
    y = "Total Psychiatric Patients"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = label_comma()) +
  scale_fill_viridis_d(option = "C")

# TOTAL PSYCHIATRIC PATIENTS PER REGION 
df_filt %>%
  filter(str_to_lower(specialty) == "psychiatric", str_to_lower(trend_type) == "patients") %>%
  group_by(hbtreat_name) %>%
  summarise(total_patients = sum(value, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_patients)) %>%
  ggplot(aes(x = reorder(hbtreat_name, total_patients), y = total_patients, fill = hbtreat_name)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Total Psychiatric Patients by Region",
    x = "Region (NHS Area)",
    y = "Total Psychiatric Patients(Log Scale)"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = label_comma()) +
  scale_y_log10(labels = label_comma()) + 
  scale_fill_viridis_d(option = "D")

# TOTAL PSYCHIATRIC PATIENTS PER COVID PERIOD
df_filt %>%
  filter(str_to_lower(specialty) == "psychiatric", str_to_lower(trend_type) == "patients") %>%
  group_by(covid_period) %>%
  summarise(total_patients = sum(value, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = covid_period, y = total_patients, fill = covid_period)) +
  geom_col(show.legend = FALSE) +
  labs(
    title = "Total Psychiatric Patients by COVID Period",
    x = "",
    y = "Total Psychiatric Patients"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = label_comma()) +
  scale_fill_viridis_d(option = "D")

# TOTAL PSYCHIATRIC PATIENTS PER YEAR
df_filt %>%
  filter(str_to_lower(specialty) == "psychiatric", str_to_lower(trend_type) == "patients") %>%
  group_by(year) %>%
  summarise(total_patients = sum(value, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = factor(year), y = total_patients, fill = as.factor(year))) +
  geom_col(show.legend = FALSE) +
  labs(
    title = "Psychiatric Patients by Year",
    x = "",
    y = "Total Psychiatric Patients"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = label_comma()) +
  scale_fill_viridis_d(option = "D")

# PSYCHIATRIC PATIENTS BY REGION AND YEAR (FACETED, LOG SCALE)
df_filt %>%
  filter(str_to_lower(specialty) == "psychiatric", str_to_lower(trend_type) == "patients") %>%
  group_by(hbtreat_name, year) %>%
  summarise(total_patients = sum(value, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = hbtreat_name, y = total_patients, fill = as.factor(year))) +
  geom_col(position = "dodge") +
  labs(
    title = "Psychiatric Patients by Region and Year (Log Scale)",
    x = "Region (NHS Area)",
    y = "Total Psychiatric Patients (log scale)",
    fill = "Year"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_log10(labels = label_comma()) + 
  scale_fill_viridis_d(option = "C")

# PSYCHIATRIC PATIENTS BY HOSPITAL AND COVID PERIOD 
df_filt %>%
  filter(str_to_lower(specialty) == "psychiatric", str_to_lower(trend_type) == "patients") %>%
  group_by(hospital_name, covid_period) %>%
  summarise(total_patients = sum(value, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_patients)) %>%
  slice_head(n = 35) %>%
  ggplot(aes(x = reorder(hospital_name, total_patients), y = total_patients, fill = covid_period)) +
  geom_col() +
  coord_flip() +
  labs(
    title = "Psychiatric Patients by Hospital and COVID Period (Top 20 Hospitals)",
    x = "Hospital Name",
    y = "Total Psychiatric Patients",
    fill = "COVID Period"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = label_comma()) +
  scale_fill_viridis_d(option = "C")

# FOCUS ON ACCURATE HOSPITAL/TYPE LABELING
# 1. Standardise specialty and trend_type columns to lower-case, trimmed text
df_filt <- df_filt %>%
  mutate(
    specialty = str_trim(str_to_lower(specialty)),
    trend_type = str_trim(str_to_lower(trend_type))
  )

# 2. Identify the top 20 hospitals by overall total psychiatric patients (across all periods)
top_hospitals <- df_filt %>%
  filter(specialty == "psychiatric", trend_type == "patients") %>%
  group_by(hospital_name) %>%
  summarise(total_patients = sum(value, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_patients)) %>%
  slice_head(n = 20) %>%
  pull(hospital_name)

# 3. Plot only those hospitals, with breakdown by COVID period
df_filt %>%
  filter(specialty == "psychiatric", trend_type == "patients", hospital_name %in% top_hospitals) %>%
  group_by(hospital_name, covid_period) %>%
  summarise(total_patients = sum(value, na.rm = TRUE), .groups = "drop") %>%
  # Join with total so we can order hospitals by grand total for the plot
  left_join(
    df_filt %>%
      filter(specialty == "psychiatric", trend_type == "patients", hospital_name %in% top_hospitals) %>%
      group_by(hospital_name) %>%
      summarise(grand_total = sum(value, na.rm = TRUE), .groups = "drop"),
    by = "hospital_name"
  ) %>%
  ggplot(aes(x = reorder(hospital_name, grand_total), y = total_patients, fill = covid_period)) +
  geom_col(position = "stack") +
  coord_flip() +
  labs(
    title = "Psychiatric Patients by Hospital and COVID Period (Top 20 Hospitals)",
    x = "Hospital Name",
    y = "Total Psychiatric Patients",
    fill = "COVID Period"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = label_comma()) +
  scale_fill_viridis_d(option = "C")

# QUICK STATISTICS (per COVID period)
df_filt %>%
  filter(str_to_lower(specialty) == "psychiatric", str_to_lower(trend_type) == "patients") %>%
  group_by(covid_period) %>%
  summarise(
    mean_patients = mean(value, na.rm = TRUE),
    median_patients = median(value, na.rm = TRUE),
    min_patients = min(value, na.rm = TRUE),
    max_patients = max(value, na.rm = TRUE),
    total_patients = sum(value, na.rm = TRUE),
    n_obs = n()
  )

# QUICK SUMMARY TABLE: TOP HOSPITALS
top_hospitals_table <- df_filt %>%
  filter(str_to_lower(specialty) == "psychiatric", str_to_lower(trend_type) == "patients") %>%
  group_by(hospital_name) %>%
  summarise(total_patients = sum(value, na.rm = TRUE), .groups = "drop") %>%
  arrange(desc(total_patients)) %>%
  slice_head(n = 15)
top_hospitals_table

# SAVE THE FINAL FILTERED DATASET (for reporting/reproducibility)
write_csv(df_filt, "Data/mhia_datatrends_dataset.csv")
