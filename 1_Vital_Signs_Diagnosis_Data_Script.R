## Loading and Cleaning
library(tidyverse)

data <- read.csv("C:\\Users\\Gabriel\\Documents\\GHPortfolio - Vital_signs_diagnosis_data_Group_012 (Exp 3).csv")
summary(data)

data$Systolic_BP <- as.numeric(data$Systolic_BP)
data$Diastolic_BP <- as.numeric(data$Diastolic_BP)
data$Hypertension <- as.numeric(data$Hypertension)

no_zero_vars <- c(
  "Weight_kg", "Height_cm", "BMI", "Systolic_BP", "Diastolic_BP",
  "Heart_rate", "Physical_Activity_Hours_Week", "Daily_Sleeping_hours",
  "Glucose_mg.dL", "Cholesterol_mg.dL")
essential_columns <- c(
  "Patient.ID", "Age", "Weight_kg", "Height_cm", "BMI",
  "Systolic_BP", "Diastolic_BP", "Heart_rate",
  "Glucose_mg.dL", "Cholesterol_mg.dL")

clean_data <- data %>%
  filter(!is.na(Patient.ID)) %>%
  filter(across(all_of(no_zero_vars), ~ . != 0)) %>%
  drop_na(all_of(essential_columns)) %>%
  filter(!is.na(Smoking_Status))

summary(clean_data)

## Desciptive Statistics and Advanced Statistical Test
descriptive_stats <- clean_data %>%
  select(where(is.numeric)) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>%
  group_by(Variable) %>%
  summarise(
    Count = sum(!is.na(Value)),
    Missing = sum(is.na(Value)),
    Mean = mean(Value, na.rm = TRUE),
    Median = median(Value, na.rm = TRUE),
    SD = sd(Value, na.rm = TRUE),
    Min = min(Value, na.rm = TRUE),
    Max = max(Value, na.rm = TRUE),
    .groups = "drop")

print(descriptive_stats)

clean_data$Smoking_status_Legend <- as.factor(clean_data$Smoking_status_Legend)
clean_data$Hypertension_Legend <- as.factor(clean_data$Hypertension_Legend)
smoke_hyper_table <- table(clean_data$Smoking_status_Legend, clean_data$Hypertension_Legend)
chisq_test <- chisq.test(smoke_hyper_table)

print(chisq_test)

## Plots
# BMI DENSITY PLOT
ggplot(clean_data, aes(x = BMI)) +
  geom_density(fill = "skyblue", alpha = 0.6) +
  geom_vline(xintercept = c(18.5, 24.9, 29.9), 
             linetype = "dashed", color = "red") +
  annotate("text", x = 17, y = 0.04, label = "Underweight", size = 3) +
  annotate("text", x = 21.5, y = 0.04, label = "Normal", size = 3) +
  annotate("text", x = 27, y = 0.04, label = "Overweight", size = 3) +
  annotate("text", x = 32, y = 0.04, label = "Obese", size = 3) +
  labs(title = "Density Plot of BMI",
       x = "BMI",
       y = "Density") +
  theme_minimal()

# SMOKING STATUS BAR PLOT
ggplot(clean_data, aes(x = factor(Smoking_Status))) +
  geom_bar(aes(fill = factor(Smoking_Status)), alpha = 0.8) +
  scale_x_discrete(labels = c("0" = "Non-smoker", "1" = "Occasional smoker", "2" = "Chainsmoker")) +
  scale_fill_manual(
    values = c("0" = "skyblue", "1" = "orange", "2" = "firebrick"),
    labels = c("0" = "Non-smoker", "1" = "Occasional smoker", "2" = "Chainsmoker"),
    name = "Smoking Status"
  ) +
  geom_text(
    stat = "count",
    aes(label = ..count..),
    vjust = -0.5,
    size = 4
  ) +
  labs(
    title = "Distribution of Smoking Status",
    x = "Smoking Status",
    y = "Count"
  ) +
  theme_minimal()

# DISTRIBUTION OF HYPERTENSION STATUS BY SMOKING STATUS
# Convert Smoking_Status and Hypertension to factors with custom labels
clean_data$Smoking_Status <- factor(
  clean_data$Smoking_Status,
  levels = c(0, 1, 2),
  labels = c("Non-Smoker", "Occasional Smoker", "Chainsmoker")
)

clean_data$Hypertension <- factor(
  clean_data$Hypertension,
  levels = 0:5,
  labels = c(
    "Hypotensive", "Normal", "Elevated",
    "Stage 1", "Stage 2", "Crisis"
  )
)

# Plot: Stacked bar chart
ggplot(clean_data, aes(x = Smoking_Status, fill = Hypertension)) +
  geom_bar(position = "stack") +
  scale_fill_manual(
    name = "Hypertension Stage",
    values = c(
      "Hypotensive" = "lightblue",
      "Normal" = "lightgreen",
      "Elevated" = "gold",
      "Stage 1" = "orange",
      "Stage 2" = "tomato",
      "Crisis" = "red"
    )
  ) +
  labs(
    title = "Distribution of Hypertension Stages by Smoking Status",
    x = "Smoking Status",
    y = "Number of Individuals"
  ) +
  theme_minimal()

#SCATTERPLOT: AGE VS BMI
ggplot(clean_data, aes(x = Age, y = BMI)) +
  geom_point(color = "steelblue", alpha = 0.6) +
  geom_smooth(method = "lm", se = FALSE, color = "darkred") +
  labs(
    title = "Scatterplot of Age vs. BMI with Linear Trend Line",
    x = "Age (years)",
    y = "BMI"
  ) +
  theme_minimal()