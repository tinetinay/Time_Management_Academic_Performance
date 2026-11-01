library(ggplot2)

# Load dataset
student_data <- read.csv("time_management.csv")

# View dataset
View(student_data)

# SUMMARY STATISTICS

summary_statistics <- data.frame(
  Average_Age = mean(student_data$Age),
  Average_Study_Hours = mean(student_data$Daily_Study_Hours),
  Average_Sleep_Hours = mean(student_data$Sleep_Hours),
  Average_Social_Media_Hours = mean(student_data$Social_Media_Hours),
  Average_Assignment_Completion = mean(student_data$Assignment_Completion_Rate),
  Average_Time_Management_Rating = mean(student_data$Time_Management_Rating),
  Average_Attendance_Rate = mean(student_data$Class_Attendance_Rate),
  Average_Academic_Performance = mean(student_data$Academic_Performance)
)

print(summary_statistics)

# AVERAGE ACADEMIC PERFORMANCE BY GENDER

gender_summary <- aggregate(
  Academic_Performance ~ Gender,
  data = student_data,
  FUN = mean
)

print(gender_summary)


# AVERAGE ACADEMIC PERFORMANCE BY TIME MANAGEMENT RATING

time_management_summary <- aggregate(
  Academic_Performance ~ Time_Management_Rating,
  data = student_data,
  FUN = mean
)

print(time_management_summary)


# AVERAGE ACADEMIC PERFORMANCE BY ATTENDANCE RATE

attendance_summary <- aggregate(
  Academic_Performance ~ Class_Attendance_Rate,
  data = student_data,
  FUN = mean
)

print(attendance_summary)

# CORRELATION ANALYSIS

study_correlation <- cor(
  student_data$Daily_Study_Hours,
  student_data$Academic_Performance
)

sleep_correlation <- cor(
  student_data$Sleep_Hours,
  student_data$Academic_Performance
)

social_media_correlation <- cor(
  student_data$Social_Media_Hours,
  student_data$Academic_Performance
)

time_management_correlation <- cor(
  student_data$Time_Management_Rating,
  student_data$Academic_Performance
)

print(study_correlation)
print(sleep_correlation)
print(social_media_correlation)
print(time_management_correlation)

if (!dir.exists("figures")) {
  dir.create("figures")
}

graph1 <- ggplot(
  student_data,
  aes(x = Daily_Study_Hours, y = Academic_Performance)
) +
  geom_point(color = "steelblue", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "red") +
  labs(
    title = "Daily Study Hours vs Academic Performance",
    x = "Daily Study Hours",
    y = "Academic Performance"
  ) +
  theme_minimal()

print(graph1)

ggsave("figures/study_hours_vs_performance.png",
       graph1, width = 8, height = 5)

graph2 <- ggplot(
  time_management_summary,
  aes(x = factor(Time_Management_Rating),
      y = Academic_Performance)
) +
  geom_col(fill = "darkseagreen") +
  labs(
    title = "Academic Performance by Time Management Rating",
    x = "Time Management Rating (1–5)",
    y = "Average Academic Performance"
  ) +
  theme_minimal()

print(graph2)

ggsave("figures/performance_by_time_management.png",
       graph2, width = 8, height = 5)

graph3 <- ggplot(
  student_data,
  aes(x = Social_Media_Hours, y = Academic_Performance)
) +
  geom_point(color = "coral", size = 3) +
  geom_smooth(method = "lm", se = FALSE, color = "blue") +
  labs(
    title = "Social Media Hours vs Academic Performance",
    x = "Daily Social Media Hours",
    y = "Academic Performance"
  ) +
  theme_minimal()

print(graph3)

ggsave("figures/social_media_vs_performance.png",
       graph3, width = 8, height = 5)

graph4 <- ggplot(
  attendance_summary,
  aes(x = factor(Class_Attendance_Rate),
      y = Academic_Performance)
) +
  geom_col(fill = "mediumpurple") +
  labs(
    title = "Academic Performance by Attendance Rate",
    x = "Class Attendance Rate (%)",
    y = "Average Academic Performance"
  ) +
  theme_minimal()

print(graph4)

ggsave("figures/performance_by_attendance.png",
       graph4, width = 8, height = 5)

# SAVE SUMMARY TABLES

write.csv(
  summary_statistics,
  "summary_statistics.csv",
  row.names = FALSE
)

write.csv(
  gender_summary,
  "gender_summary.csv",
  row.names = FALSE
)

write.csv(
  time_management_summary,
  "time_management_summary.csv",
  row.names = FALSE
)

write.csv(
  attendance_summary,
  "attendance_summary.csv",
  row.names = FALSE
)

print("Data analysis and graph generation completed successfully!")

