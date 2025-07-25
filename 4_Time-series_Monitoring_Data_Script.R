## Loading Data
library(tidyverse)

data <- read.csv("C:\\Users\\Gabriel\\Documents\\GHPortfolio - Time_series_Monitoring_data_Group_012 (Exp 8).csv")

summary(data)
head(data)

## Descriptive Statistics and Advanced Statistical Test
long_data <- data %>%
  pivot_longer(
    cols = -c(Month, Month_numerical),
    names_to = c("Patient", "Metric"),
    names_pattern = "Patient_(\\d)_(.*)",
    values_to = "Value"
  )

grouped_summary <- long_data %>%
  group_by(Patient, Metric) %>%
  summarise(
    Mean = mean(Value, na.rm = TRUE),
    SD = sd(Value, na.rm = TRUE),
    Min = min(Value, na.rm = TRUE),
    Max = max(Value, na.rm = TRUE),
    .groups = 'drop'
  )

print(grouped_summary)

cor_results <- data %>%
  summarise(
    Patient_1 = cor(Patient_1_avg_steps, Patient_1_Stress_Level, use = "complete.obs"),
    Patient_2 = cor(Patient_2_avg_steps, Patient_2_Stress_Level, use = "complete.obs"),
    Patient_3 = cor(Patient_3_avg_steps, Patient_3_Stress_Level, use = "complete.obs")
  ) %>%
  pivot_longer(everything(), names_to = "Patient", values_to = "Correlation")

cor_results

## Plots
#TIME SERIES BMI PLOT
bmi_data <- data %>%
  select(Month_numerical, Patient_1_BMI, Patient_2_BMI, Patient_3_BMI) %>%
  pivot_longer(
    cols = starts_with("Patient_"),
    names_to = "Patient",
    values_to = "BMI"
  ) %>%
  mutate(
    Patient = str_remove(Patient, "_BMI"),
    Patient = str_replace(Patient, "Patient_", "Patient ")
  )

ggplot(bmi_data, aes(x = Month_numerical, y = BMI, color = Patient)) +
  geom_line(size = 1.2, alpha = 0.9) +
  geom_point(size = 2) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Monthly BMI Trends of Patients",
    x = "Month (Numerical)",
    y = "Body Mass Index (BMI)",
    color = "Patient"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(size = 16),
    legend.position = "top",
    panel.grid.minor = element_blank()
  )

#TIME SERIES STEPS PLOT
steps_data <- data %>%
  select(Month_numerical, Patient_1_avg_steps, Patient_2_avg_steps, Patient_3_avg_steps) %>%
  pivot_longer(
    cols = starts_with("Patient_"),
    names_to = "Patient",
    values_to = "Avg_Steps"
  ) %>%
  mutate(
    Patient = str_remove(Patient, "_avg_steps"),
    Patient = str_replace(Patient, "Patient_", "Patient ")
  )

ggplot(steps_data, aes(x = Month_numerical, y = Avg_Steps, color = Patient)) +
  geom_line(size = 1.2, alpha = 0.9) +
  geom_point(size = 2) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Monthly Average Steps of Patients",
    x = "Month (Numerical)",
    y = "Average Daily Steps",
    color = "Patient"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(size = 16),
    legend.position = "top",
    panel.grid.minor = element_blank()
  )

#TIME SERIES STRESS PLOT
stress_data <- data %>%
  select(Month_numerical, Patient_1_Stress_Level, Patient_2_Stress_Level, Patient_3_Stress_Level) %>%
  pivot_longer(
    cols = starts_with("Patient_"),
    names_to = "Patient",
    values_to = "Stress_Level"
  ) %>%
  mutate(
    Patient = str_remove(Patient, "_Stress_Level"),
    Patient = str_replace(Patient, "Patient_", "Patient ")
  )

ggplot(stress_data, aes(x = Month_numerical, y = Stress_Level, color = Patient)) +
  geom_line(size = 1.2, alpha = 0.9) +
  geom_point(size = 2) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "Monthly Stress Level Trends of Patients",
    x = "Month (Numerical)",
    y = "Stress Level (0–10)",
    color = "Patient"
  ) +
  theme_minimal(base_size = 13) +
  theme(
    plot.title = element_text(size = 16),
    legend.position = "top",
    panel.grid.minor = element_blank()
  )