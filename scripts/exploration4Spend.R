# Exploratory Data Analysis
# Load libraries
library(tidyverse)
library(janitor)
library(scales)


# 1. Load and clean data
df <- read_csv("Data/EF4 - Mental Health Spend Data.csv") %>% clean_names()

# Exclude rows with "Scotland" in geography1 or geography2
df <- df %>%
  filter(
    !str_detect(area_name, regex("NHS Scotland", ignore_case = TRUE)),
    
  )

# 2. Standardise 'financial_year' column to 'year'
# Rename financial_year to year
df <- df %>%
  rename(year = financial_year) %>%
  mutate(
    year = ifelse(
      str_detect(as.character(year), "/"),
      as.integer(str_sub(as.character(year), 1, 4)),
      as.integer(year)
    )
  )


# 3. Filter for fiscal years 2018–2023
df <- df %>% filter(year >= 2018, year <= 2023)

# 4. Assign COVID-19 periods
df <- df %>%
  mutate(
    covid_period = case_when(
      year %in% c(2018, 2019) ~ "Pre-COVID",
      year %in% c(2020, 2021) ~ "COVID-19",
      year %in% c(2022, 2023) ~ "Post-COVID"
    ),
    covid_period = factor(covid_period, levels = c("Pre-COVID", "COVID-19", "Post-COVID"))
  )

# 5. Select Expenditure Measures for analysis
spend_measures <- c("Total Net NHS Expenditure (£ millions)", 
                    "Total Mental Health Expenditure (£ millions)", 
                    "Total CAMHS Expenditure (£ millions)")

df_spend <- df %>%
  filter(measure_name %in% spend_measures)

# 6. Distribution of selected expenditures by year
df_spend %>%
  group_by(year, measure_name) %>%
  summarise(total_spend = sum(value, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = factor(year), y = total_spend, fill = measure_name)) +
  geom_col(position = "dodge") +
  labs(title = "Expenditure by Year", x = "", y = "Total Expenditure (£)", fill = "Measure") +
  theme_minimal() +
  scale_y_continuous(labels = label_comma())+
  scale_fill_viridis_d(option = "C")

# 7. Compare expenditures directly
df_spend %>%
  group_by(measure_name) %>%
  summarise(total = sum(value, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = reorder(measure_name, total), y = total, fill = measure_name)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(title = "Total Expenditure Comparison", x = "Expenditure Type", y = "Total Expenditure (£)") +
  theme_minimal() +
  scale_y_continuous(labels = label_comma())+
  scale_fill_viridis_d(option = "C")

# 8. Distribution of expenditures by region
df_spend %>%
  group_by(area_name, measure_name) %>%
  summarise(total = sum(value, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = area_name, y = total, fill = measure_name)) +
  geom_col(position = "dodge") +
  labs(title = "Expenditure by Region", x = "Region (Area name)", y = "Total Expenditure (£)", fill ="Measure") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_y_continuous(labels = label_comma())+
  scale_fill_viridis_d(option = "C")

# Percentage of Expenditures by Region 
df_spend %>%
  group_by(measure_name) %>%
  mutate(measure_total = sum(value, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(area_name, measure_name) %>%
  summarise(region_total = sum(value, na.rm = TRUE), measure_total = first(measure_total), .groups = "drop") %>%
  mutate(percent = 100 * region_total / measure_total) %>%
  ggplot(aes(x = area_name, y = percent, fill = measure_name)) +
  geom_col(position = "dodge") +
  labs(title = "Region's % Share of Each Expenditure Type", 
       x = "Region (Area name)", 
       y = "Percent (%)",
       fill = "Measure") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))+
  scale_fill_viridis_d(option = "C")



# 9. Which region received the most Total Mental Health and Total CAMHS Expenditure?
df_spend %>%
  filter(measure_name %in% c("Total Mental Health Expenditure (£ millions)", 
                             "Total CAMHS Expenditure (£ millions)")) %>%
  group_by(area_name, measure_name) %>%
  summarise(total = sum(value, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = reorder(area_name, total), y = total, fill = measure_name)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = "Top Regions for Mental Health and CAMHS Expenditure", 
       x = "NHS Region", y = "Total Expenditure (£)", fill = "Measure") +
  theme_minimal() +
  scale_y_continuous(labels = label_comma()) +
  scale_fill_viridis_d(option = "C")

# Percentage of Total Mental Health & CAMHS Expenditure by Region
df_spend %>%
  filter(measure_name %in% c("Total Mental Health Expenditure (£ millions)", 
                             "Total CAMHS Expenditure (£ millions)")) %>%
  group_by(measure_name) %>%
  mutate(measure_total = sum(value, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(area_name, measure_name) %>%
  summarise(region_total = sum(value, na.rm = TRUE), measure_total = first(measure_total), .groups = "drop") %>%
  mutate(percent = 100 * region_total / measure_total) %>%
  ggplot(aes(x = reorder(area_name, percent), y = percent, fill = measure_name)) +
  geom_col(position = "dodge") +
  coord_flip() +
  labs(title = "% of Total Mental Health & CAMHS Expenditure by Region", 
       x = "NHS Region", y = "Percent (%)", fill = "Measure") +
  theme_minimal() +
  scale_fill_viridis_d(option = "C")


# 10. Distribution of expenditures by COVID period
df_spend %>%
  group_by(covid_period, measure_name) %>%
  summarise(total_spend = sum(value, na.rm = TRUE), .groups = "drop") %>%
  ggplot(aes(x = covid_period, y = total_spend, fill = measure_name)) +
  geom_col(position = "dodge") +
  labs(title = "Expenditure by COVID Period", x = "", y = "Total Expenditure (£)", fill = "Measure") +
  theme_minimal() +
  scale_y_continuous(labels = label_comma()) +
  scale_fill_viridis_d(option = "C")


# Percentage of Expenditures by COVID Period
df_spend %>%
  group_by(measure_name) %>%
  mutate(measure_total = sum(value, na.rm = TRUE)) %>%
  ungroup() %>%
  group_by(covid_period, measure_name) %>%
  summarise(period_total = sum(value, na.rm = TRUE), measure_total = first(measure_total), .groups = "drop") %>%
  mutate(percent = 100 * period_total / measure_total) %>%
  ggplot(aes(x = covid_period, y = percent, fill = measure_name)) +
  geom_col(position = "dodge") +
  labs(title = "COVID Period % Share of Each Expenditure Type", x = "", y = "Percent (%)", fill = "Measure") +
  theme_minimal() +
  scale_fill_viridis_d(option = "C")


# 11. Which year was highest for each spend type?
df_spend %>%
  group_by(measure_name, year) %>%
  summarise(total = sum(value, na.rm = TRUE), .groups = "drop") %>%
  group_by(measure_name) %>%
  filter(total == max(total)) %>%
  arrange(measure_name, desc(total)) %>%
  print(n = 10)





# Summary
str(df)
summary(df)

# Write csv
write_csv(df,"Data/mental_health_spend_dataset.csv")

SpendingData <- read_csv("Data/mental_health_spend_dataset.csv")
