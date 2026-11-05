
# Internet Usage and Student Productivity
# Step 4: Data Analysis and Graphs

# Install ggplot2 if it is not yet installed
if (!requireNamespace("ggplot2", quietly = TRUE)) {
  install.packages("ggplot2")
}

library(ggplot2)

# Load dataset
internet_data <- read.csv("internet_usage_productivity.csv")

# Create output folder for graphs
if (!dir.exists("figures")) {
  dir.create("figures")
}

# 1. Summary statistics
summary_statistics <- data.frame(
  Average_Internet_Usage = mean(
    internet_data$Daily_Internet_Usage_Hours
  ),
  Average_Social_Media_Usage = mean(
    internet_data$Social_Media_Usage_Hours
  ),
  Average_Online_Learning_Usage = mean(
    internet_data$Online_Learning_Usage_Hours
  ),
  Average_Study_Hours = mean(
    internet_data$Study_Hours_Per_Day
  ),
  Average_Sleep_Hours = mean(
    internet_data$Sleep_Hours_Per_Day
  ),
  Average_Productivity_Rating = mean(
    internet_data$Productivity_Rating
  )
)

print(summary_statistics)

write.csv(
  summary_statistics,
  "summary_statistics.csv",
  row.names = FALSE
)

# 2. Average internet usage by year level
year_summary <- aggregate(
  Daily_Internet_Usage_Hours ~ Year_Level,
  data = internet_data,
  FUN = mean
)

print(year_summary)

write.csv(
  year_summary,
  "year_level_summary.csv",
  row.names = FALSE
)

# 3. Productivity rating summary
productivity_summary <- aggregate(
  Productivity_Rating ~ Distraction_Level,
  data = internet_data,
  FUN = mean
)

print(productivity_summary)

write.csv(
  productivity_summary,
  "productivity_summary.csv",
  row.names = FALSE
)

# GRAPH 1: Daily Internet Usage Distribution
graph1 <- ggplot(
  internet_data,
  aes(x = Daily_Internet_Usage_Hours)
) +
  geom_histogram(
    bins = 10,
    fill = "steelblue",
    color = "white"
  ) +
  labs(
    title = "Daily Internet Usage Among Students",
    x = "Internet Usage (Hours)",
    y = "Number of Students"
  ) +
  theme_minimal()

print(graph1)

ggsave(
  "figures/daily_internet_usage.png",
  graph1,
  width = 8,
  height = 5
)

# GRAPH 2: Internet Usage by Year Level
graph2 <- ggplot(
  internet_data,
  aes(
    x = Year_Level,
    y = Daily_Internet_Usage_Hours,
    fill = Year_Level
  )
) +
  geom_boxplot() +
  labs(
    title = "Daily Internet Usage by Year Level",
    x = "Year Level",
    y = "Internet Usage (Hours)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(graph2)

ggsave(
  "figures/internet_usage_by_year.png",
  graph2,
  width = 8,
  height = 5
)

# GRAPH 3: Study Hours and Productivity Rating
graph3 <- ggplot(
  internet_data,
  aes(
    x = Study_Hours_Per_Day,
    y = Productivity_Rating
  )
) +
  geom_point(
    color = "darkgreen",
    size = 3
  ) +
  geom_smooth(
    method = "lm",
    se = FALSE,
    color = "orange"
  ) +
  labs(
    title = "Study Hours and Productivity Rating",
    x = "Study Hours per Day",
    y = "Productivity Rating (1-5)"
  ) +
  theme_minimal()

print(graph3)

ggsave(
  "figures/study_hours_productivity.png",
  graph3,
  width = 8,
  height = 5
)

# GRAPH 4: Productivity by Distraction Level
graph4 <- ggplot(
  internet_data,
  aes(
    x = Distraction_Level,
    y = Productivity_Rating,
    fill = Distraction_Level
  )
) +
  geom_boxplot() +
  labs(
    title = "Productivity Rating by Distraction Level",
    x = "Distraction Level",
    y = "Productivity Rating (1-5)"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

print(graph4)

ggsave(
  "figures/productivity_by_distraction.png",
  graph4,
  width = 8,
  height = 5
)

cat("ANALYSIS COMPLETED SUCCESSFULLY!\n")
cat("Four graphs saved in the figures folder.\n")


# Step 5: Additional Analysis

# Internet usage and study hours
cor_internet_study <- cor(
  internet_data$Daily_Internet_Usage_Hours,
  internet_data$Study_Hours_Per_Day
)

# Study hours and productivity rating
cor_study_productivity <- cor(
  internet_data$Study_Hours_Per_Day,
  internet_data$Productivity_Rating
)

cat(
  "Correlation: Internet Usage and Study Hours:",
  round(cor_internet_study, 3), "\n"
)

cat(
  "Correlation: Study Hours and Productivity:",
  round(cor_study_productivity, 3), "\n"
)

