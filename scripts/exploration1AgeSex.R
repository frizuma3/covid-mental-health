# Age-Sex-COVID-Psychiatric Patient Exploration

library(tidyverse)  # For data wrangling, manipulation and plotting
library(janitor)    # For clean_names() and other data cleaning helpers
library(scales)     # For formatting numbers and axis labels in plots
library(viridis)    # For colour palettes in plots
library(ggthemes)   # For pyramid plotting 

# 1. Load and clean data
df <- read_csv("Data/mhia_agesex_2024.csv") %>%  # Load the raw CSV
  clean_names()                                  # Standardise column names to snake_case

# Exclude Scotland-wide rows (only NHS Board level)
df <- df %>%
  filter(
    # Remove rows where 'geography1' or 'geography2' contain 'Scotland' (case-insensitive)
    # They are the total of all other rows
    !str_detect(geography1, regex("Scotland", ignore_case = TRUE)),
    !str_detect(geography2, regex("Scotland", ignore_case = TRUE))
  )

# 2. Standardise year to integer
df <- df %>%
  mutate(
    year = ifelse(
      str_detect(as.character(year), "/"),      # Check if year contains a "/"
      as.integer(str_sub(as.character(year), 1, 4)),  # Take only the first 4 digits (e.g. "2018/2019" becomes 2018)
      as.integer(year)                          # Else convert directly to integer
    )
  ) %>%
  filter(year >= 2018, year <= 2023)            # Keep only years relevant to the study window

# 3. Assign COVID period
df <- df %>%
  mutate(
    # Create 'period' categorical variable by year for easier COVID comparisons
    period = case_when(
      year < 2020 ~ "Pre-COVID",
      year %in% c(2020, 2021) ~ "COVID-19",
      year >= 2022 ~ "Post-COVID",
      TRUE ~ NA_character_
    ),
    period = factor(period, levels = c("Pre-COVID", "COVID-19", "Post-COVID"))
  )

# 4. Focus on psychiatric, number of patients
df_psy <- df %>%
  filter(str_to_lower(dataset) == "psychiatric",  # Only psychiatric records
         measure == "Number of patients")         # Only "number of patients" measure

# I. Age-Sex-COVID Pyramid Plot (by period)

# Create data for the pyramid: negative values for Male, positive for Female for mirroring
df_psy_pivot <- df_psy %>%
  mutate(value = if_else(sex_char == "Male", -value, value)) %>%  # Negative for males
  group_by(period, ageband, sex_char) %>%
  summarise(total = sum(value, na.rm = TRUE), .groups = "drop")   # Aggregate to total by group

# Population pyramid plot: mirrors male and female counts per ageband, per COVID period
ggplot(df_psy_pivot, aes(x = ageband, y = total, fill = sex_char)) +
  geom_bar(stat = "identity", position = "identity") +  # Bar height = total patients
  coord_flip() +                                        # Flip coordinates for horizontal bars
  facet_wrap(~ period, ncol = 1) +                      # One pyramid per COVID period
  scale_fill_manual(values = c("Male" = "#4477AA", "Female" = "#CC6677")) +
  labs(
    title = "Population Pyramid: Psychiatric Patients by Age, Sex & COVID Period",
    x = "Age Band", y = "Number of Patients",
    fill = "Sex"
  ) +
  theme_minimal() +
  scale_y_continuous(labels = abs, breaks = scales::pretty_breaks()) +
  theme(axis.text.x = element_text(size = 10),
        axis.text.y = element_text(size = 10))

# II. Age–Sex Heatmap of Psychiatric Patients Across Periods
df_psy_heat <- df_psy %>%
  group_by(period, ageband, sex_char) %>%
  summarise(total = sum(value, na.rm = TRUE), .groups = "drop")  # Aggregate counts

# Create a heatmap for each period: ageband on x, sex on y, cell fill = patient count
ggplot(df_psy_heat, aes(x = ageband, y = sex_char, fill = total)) +
  geom_tile(color = "white") +                        # Each tile represents a combination
  facet_wrap(~ period) +
  scale_fill_viridis(option = "C", name = "Patients") +
  labs(title = "Heatmap: Psychiatric Patients by Age, Sex & COVID Period",
       x = "Age Band", y = "Sex") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) # Slanted age labels


# III. Age Band Distribution Over Time (by Sex)
# Trend lines by sex, by ageband, over time (2018-2023)
df_psy %>%
  group_by(year, ageband, sex_char) %>%
  summarise(total = sum(value, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = year, y = total, color = sex_char)) +
  geom_line(linewidth = 1.5) +
  facet_wrap(~ ageband, scales = "free_y") +  # Separate plot for each ageband
  labs(
    title = "Trend: Psychiatric Patients by Age Band & Sex (2018–2023)",
    x = "Year", y = "Number of Patients", color = "Sex"
  ) +
  theme_minimal()


# IV. Proportional Age–Sex Distribution within Each Period
# Calculate % of patients by ageband and sex within each period
df_psy_prop <- df_psy %>%
  group_by(period, ageband, sex_char) %>%
  summarise(count = sum(value, na.rm = TRUE), .groups = "drop") %>%
  group_by(period) %>%
  mutate(prop = 100 * count / sum(count)) %>%  # % of total for each period
  ungroup()

# Bar plot of proportions per ageband/sex by period
ggplot(df_psy_prop, aes(x = ageband, y = prop, fill = sex_char)) +
  geom_col(position = "dodge") +
  facet_wrap(~ period) +
  labs(
    title = "Proportion of Psychiatric Patients by Age, Sex & COVID Period",
    x = "Age Band", y = "Percent (%)", fill = "Sex"
  ) +
  scale_y_continuous(labels = scales::percent_format(scale = 1)) + #15% as 15
  theme_minimal() +
  scale_fill_viridis_d(option = "D")


# V. Overall Trend: Psychiatric Patients by Age, Sex, Year
# Bar plots (one per ageband) showing year-on-year counts by sex
df_psy %>%
  group_by(year, ageband, sex_char) %>%
  summarise(total = sum(value, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = factor(year), y = total, fill = sex_char)) +
  geom_col(position = "dodge") +
  facet_wrap(~ ageband, scales = "free_y") +
  labs(
    title = "Psychiatric Patients by Year, Age Band & Sex",
    x = "Year", y = "Patients", fill = "Sex"
  ) +
  theme_minimal() +
  scale_fill_viridis_d(option = "D")


# VI. National Age–Sex Structure by COVID Period (Summary Table)
# Tabular summary: total patients by period, ageband, sex
age_sex_period_summary <- df_psy %>%
  group_by(period, ageband, sex_char) %>%
  summarise(total_patients = sum(value, na.rm = TRUE), .groups = "drop") %>%
  arrange(period, ageband, sex_char)
age_sex_period_summary %>%
  print(n = 30)  # Print the table, all rows visible


# VII. Psychiatric vs. Non-Psychiatric: Age–Sex Trends
# Compare psychiatric vs non-psychiatric patient counts over years, by age and sex
df %>%
  filter(str_to_lower(dataset) %in% c("psychiatric", "non-psychiatric"),
         measure == "Number of patients") %>%
  group_by(year, dataset, ageband, sex_char) %>%
  summarise(total = sum(value, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = factor(year), y = total, fill = dataset)) +
  geom_col(position = "dodge") +
  facet_grid(ageband ~ sex_char) +
  labs(
    title = "Psychiatric vs Non-Psychiatric by Year, Age, Sex",
    x = "Year", y = "Total Patients", fill = "Dataset"
  ) +
  theme_minimal() +
  scale_fill_viridis_d(option = "B")


# VIII. Save Cleaned Psychiatric Age–Sex Dataset (for reporting/reproducibility)
write_csv(df_psy, "Data/mhia_agesex_dataset.csv") # Export cleaned psychiatric dataset for further analysis and reporting

# Summary Table for Report:
summary(df_psy) # Display summary statistics for the final psychiatric dataset
