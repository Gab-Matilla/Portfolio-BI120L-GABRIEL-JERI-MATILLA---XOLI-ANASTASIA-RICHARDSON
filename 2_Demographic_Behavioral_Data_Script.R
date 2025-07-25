## Loading and Cleaning
library(tidyverse)

data <- read.csv("C:\\Users\\Gabriel\\Documents\\GHPortfolio - Demographic_Behavioral_data_Group_012 (Exp 2).csv")
summary(data)
head(data)

clean_data <- data %>%
  filter(!is.na(Patient.ID)) %>%
  filter(
    across(
      c(Patient.ID, Age, Sex, Weight_kg, Height_cm, BMI,
        Region, Socioeconomic, Education,
        Physical_Activity_Hours_Week, Smoking_Status, Drinking_Status,
        Patient_Satisfaction_Score, Health_Literacy_Score),
      ~ !is.na(.) & . != ""
    )
  ) %>%
  filter(
    across(
      c(Weight_kg, Height_cm, BMI, Physical_Activity_Hours_Week),
      ~ . != 0
    )
  )

target_vars <- c("Patient.ID", "Age", "Sex", "Weight_kg", "Height_cm", "BMI",
                 "Region", "Socioeconomic", "Education", "Physical_Activity_Hours_Week",
                 "Smoking_Status", "Drinking_Status",
                 "Patient_Satisfaction_Score", "Health_Literacy_Score")

data %>%
  summarise(across(all_of(target_vars), ~sum(is.na(.) | . == " ")))

summary(clean_data)

## Descriptive Statistics and Advanced Statistical Test
descriptive_stats <- clean_data %>%
  select(Patient.ID, Age, Sex, Weight_kg, Height_cm, BMI, Region,
         Socioeconomic, Education, Physical_Activity_Hours_Week,
         Smoking_Status, Drinking_Status, Patient_Satisfaction_Score,
         Health_Literacy_Score) %>%
  pivot_longer(cols = everything(), names_to = "Variable", values_to = "Value") %>%
  group_by(Variable) %>%
  summarise(
    Count = sum(!is.na(Value)),
    Missing = sum(is.na(Value)),
    Mean = if (is.numeric(Value)) round(mean(Value, na.rm = TRUE), 2) else NA_real_,
    Median = if (is.numeric(Value)) round(median(Value, na.rm = TRUE), 2) else NA_real_,
    SD = if (is.numeric(Value)) round(sd(Value, na.rm = TRUE), 2) else NA_real_,
    Min = if (is.numeric(Value)) round(min(Value, na.rm = TRUE), 2) else NA_real_,
    Max = if (is.numeric(Value)) round(max(Value, na.rm = TRUE), 2) else NA_real_
  )
print(descriptive_stats)

# t-test for Patient Satisfaction Score by Region
t_test_satisfaction <- t.test(Patient_Satisfaction_Score ~ Region, data = clean_data)

# t-test for Health Literacy Score by Region
t_test_literacy <- t.test(Health_Literacy_Score ~ Region, data = clean_data)

# t-test for Education by Region
t_test_education <- t.test(Education ~ Region, data = clean_data)

print(t_test_satisfaction)
print(t_test_literacy)
print(t_test_education)

## Plots
#BAR PLOT OF SOCIOECONOMIC STATUS
clean_data %>%
  mutate(Socioeconomic = factor(Socioeconomic,
                                levels = c(1, 2, 3),
                                labels = c("Low Class", "Middle Class", "Upper Class"))) %>%
  ggplot(aes(x = Socioeconomic, fill = Socioeconomic)) +
  geom_bar() +
  geom_text(stat = "count", aes(label = after_stat(count)),
            vjust = -0.3, size = 5) +
  scale_fill_manual(values = c("Low Class" = "firebrick",
                               "Middle Class" = "steelblue",
                               "Upper Class" = "forestgreen")) +
  scale_y_continuous(expand = expansion(mult = c(0, 0.05))) +
  theme_minimal(base_size = 14) +
  theme(
    axis.title.x = element_blank(),
    legend.position = "none",
    plot.title = element_text(size = 16, hjust = 0.5),
    panel.grid.major.x = element_blank()
  ) +
  labs(
    y = "Number of Patients",
    title = "Distribution of Patients by Socioeconomic Class"
  )

#DENSITY PLOT BY AGE
mean_age <- mean(clean_data$Age, na.rm = TRUE)

ggplot(clean_data, aes(x = Age)) +
  geom_density(fill = "skyblue", alpha = 0.6) +
  geom_vline(xintercept = mean_age, color = "red", linetype = "dashed", size = 1) +
  annotate("text", x = mean_age, y = 0.005, label = paste("Mean =", round(mean_age, 1)),
           color = "red", angle = 90, vjust = -0.5, size = 5) +
  theme_minimal(base_size = 14) +
  labs(
    x = "Age",
    y = "Density",
    title = "Age Distribution of Patients"
  ) +
  theme(
    plot.title = element_text(size = 16, hjust = 0.5)
  )

#PROPORTION BARPLOTS
clean_data <- clean_data %>%
  mutate(
    Region_Label = ifelse(Region == 0, "Rural", "Urban"),
    Education_Label = factor(Education, levels = 0:3, labels = c("Uneducated", "Primary", "Secondary", "Tertiary")),
    Satisfaction_Label = factor(Patient_Satisfaction_Score, levels = 1:5),
    Literacy_Label = factor(Health_Literacy_Score, levels = 1:5)
  )
# Education Plot
education_plot <- clean_data %>%
  count(Region_Label, Education_Label) %>%
  group_by(Region_Label) %>%
  mutate(Proportion = n / sum(n)) %>%
  ggplot(aes(x = Education_Label, y = Proportion, fill = Region_Label)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("Rural" = "#66c2a5", "Urban" = "#fc8d62")) +
  labs(x = "Education Level", y = "Proportion of Patients") +
  theme_minimal(base_size = 12) +
  theme(
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.title.x = element_text(margin = margin(t = 10))
  )
# Health Literacy Plot
literacy_plot <- clean_data %>%
  count(Region_Label, Literacy_Label) %>%
  group_by(Region_Label) %>%
  mutate(Proportion = n / sum(n)) %>%
  ggplot(aes(x = Literacy_Label, y = Proportion, fill = Region_Label)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("Rural" = "#66c2a5", "Urban" = "#fc8d62")) +
  labs(x = "Health Literacy Score", y = "Proportion of Patients") +
  theme_minimal(base_size = 12) +
  theme(
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.title.x = element_text(margin = margin(t = 10))
  )
# Patient Satisfaction Plot
satisfaction_plot <- clean_data %>%
  count(Region_Label, Satisfaction_Label) %>%
  group_by(Region_Label) %>%
  mutate(Proportion = n / sum(n)) %>%
  ggplot(aes(x = Satisfaction_Label, y = Proportion, fill = Region_Label)) +
  geom_col(position = "dodge") +
  scale_fill_manual(values = c("Rural" = "#66c2a5", "Urban" = "#fc8d62")) +
  labs(x = "Patient Satisfaction Score", y = "Proportion of Patients") +
  theme_minimal(base_size = 12) +
  theme(
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.title.x = element_text(margin = margin(t = 10))
  )
education_plot
literacy_plot
satisfaction_plot