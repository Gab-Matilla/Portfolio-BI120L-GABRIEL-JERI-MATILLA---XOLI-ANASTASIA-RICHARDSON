## Loading and Cleaning
library(tidyverse)

data <- read.csv("C:\\Users\\Gabriel\\Documents\\GHPortfolio - Nutritional_Dietary_data_Group_012 (Exp 4).csv")
summary(data)
head(data)

data[data == ""] <- NA
clean_data <- data %>%
  drop_na()

clean_data <- clean_data %>%
  filter(
    Daily_Caloric_Intake_kcal > 0,
    Protein_intake_g > 0,
    Fat_intake_g > 0,
    Carbohydrate_intake_g > 0,
    Vitamin_C_mg > 0,
    Iron_mg > 0,
    Water_intake_ml > 0,
    Body_Fat_percent > 0,
    Muscle_Mass_kg > 0,
    BMI > 0)

summary(clean_data)

## Desciptive Statistics and Advance Statistical Test
data_stats <- data %>% select(-Patient.ID)

descriptive_stats <- data_stats %>%
  summarise(across(everything(), list(
    Count = ~sum(!is.na(.)),
    Missing = ~sum(is.na(.)),
    Mean = ~mean(., na.rm = TRUE),
    Median = ~median(., na.rm = TRUE),
    SD = ~sd(., na.rm = TRUE),
    Min = ~min(., na.rm = TRUE),
    Max = ~max(., na.rm = TRUE)
  ), .names = "{.col}_{.fn}")) %>%
  pivot_longer(everything(),
               names_to = c("Variable", "Statistic"),
               names_pattern = "^(.*)_(Count|Missing|Mean|Median|SD|Min|Max)$",
               values_to = "Value") %>%
  pivot_wider(names_from = Statistic, values_from = Value) %>%
  arrange(Variable)

print(descriptive_stats)

cor_test_activity <- cor.test(
  clean_data$Physical_Activity_Hours_Week,
  clean_data$Body_Fat_percent,
  method = "pearson"
)

print(cor_test_activity)

cor_test_caloric <- cor.test(
  clean_data$Daily_Caloric_Intake_kcal,
  clean_data$BMI,
  method = "pearson"
)

print(cor_test_caloric)

## Plots
#PROTEIN INTAKE HISTOGRAM
clean_data %>%
  ggplot(aes(x = Protein_intake_g)) +
  geom_histogram(binwidth = 10, fill = "limegreen", color = "white", boundary = 0) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Distribution of Protein Intake",
    x = "Protein Intake (g)",
    y = "Frequency"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.title.x = element_text(margin = margin(t = 10))
  )

#FAT INTAKE HISTOGRAM
clean_data %>%
  ggplot(aes(x = Fat_intake_g)) +
  geom_histogram(binwidth = 10, fill = "darkorange", color = "white", boundary = 0) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Distribution of Fat Intake",
    x = "Fat Intake (g)",
    y = "Frequency"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.title.x = element_text(margin = margin(t = 10))
  )

#CARBS INTAKE HISTOGRAM
clean_data %>%
  ggplot(aes(x = Carbohydrate_intake_g)) +
  geom_histogram(binwidth = 20, fill = "darkblue", color = "white", boundary = 0) +
  theme_minimal(base_size = 12) +
  labs(
    title = "Distribution of Carbohydrate Intake",
    x = "Carbohydrate Intake (g)",
    y = "Frequency"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title.y = element_text(margin = margin(r = 10)),
    axis.title.x = element_text(margin = margin(t = 10))
  )

#SCATTERPLOT OF CALORIC INTAKE VS BMI
clean_data %>%
  ggplot(aes(x = Daily_Caloric_Intake_kcal, y = BMI)) +
  geom_point(alpha = 0.6, color = "orange", size = 2) +
  geom_smooth(method = "lm", se = TRUE, color = "limegreen", linetype = "dashed") +
  theme_minimal(base_size = 12) +
  labs(
    title = "Relationship between Daily Caloric Intake and BMI",
    x = "Daily Caloric Intake (kcal)",
    y = "Body Mass Index (BMI)"
  ) +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.title.x = element_text(margin = margin(t = 10)),
    axis.title.y = element_text(margin = margin(r = 10))
  )

#SCATTERPLOT OF PHYSICAL ACTIVITY AND BODY FAT
ggplot(clean_data, aes(x = Physical_Activity_Hours_Week, y = Body_Fat_percent)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  geom_smooth(method = "lm", se = TRUE, color = "darkred", linetype = "dashed") +
  labs(
    title = "Relationship Between Physical Activity and Body Fat Percentage",
    x = "Physical Activity (Hours per Week)",
    y = "Body Fat (%)"
  ) +
  theme_minimal(base_size = 13)