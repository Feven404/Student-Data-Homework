library(readxl)
library(dplyr)
library(tidyverse)
library(tidyr)
library(ggplot2)

df_student_course <- read_excel('~/DATA 332/DATA for Student Data/Course.xlsx')
df_student_registration <- read_excel('~/DATA 332/DATA for Student Data/Registration.xlsx')
df_student <- read_excel('~/DATA 332/DATA for Student Data/Student.xlsx')

df_student_course_registration <- left_join(df_student_course, df_student_registration, by = "Instance ID")
df_student_information <- left_join(df_student_course_registration, df_student, by = "Student ID")

df_student_course_registration %>%
  group_by(Title) %>%
  summarise(count_by_title = n()) %>%
  ggplot(aes(x = Title, y = count_by_title, fill = Title)) +
  geom_bar(stat = "identity") +
  labs(title = "Count of Students Enrolled in Each Course",
       x = "Course Title",
       y = "Number of Students")

df_birth_year <- df_student_information %>%
  mutate(Birth_Year = year(`Birth Date`)) %>%
  group_by(Birth_Year) %>%
  summarise(count = n())

ggplot(df_birth_year, aes(x = Birth_Year, y = count, fill = Birth_Year)) +
  geom_bar(stat = "identity", width = 0.7) +
  labs(title = "Number of Students by Birth Year",
       x = "Birth Year",
       y = "Number of Students")

df_student_information %>%
  group_by(Title, `Payment Plan`) %>%
  summarise(`Total Cost` = sum(`Total Cost`)) %>%
  ggplot(aes(x = Title, y = `Total Cost`, fill = `Payment Plan`)) +
  geom_bar(positions="dodge", stat="identity", width=0.75) +
  theme(axis.text = element_text(angle = 55, vjust = .5, hjust = 1))

df_student_information %>%
  group_by(Title, `Payment Plan`) %>%
  summarise(`Balance Due` = sum(`Balance Due`))  %>%
  ggplot(aes(x = Title, y = `Balance Due`, fill = `Payment Plan`)) +
  geom_bar(stat="identity", width=0.75) +
  theme(axis.text = element_text(angle = 55, vjust = .5, hjust = 1))

ggplot(df_student_information, aes(x = City)) +
  geom_bar(fill = "skyblue") +
  labs(title = "Distribution of Students by City",
       x = "City",
       y = "Number of Students") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1))

average_hours <- df_student_information %>%
  group_by(Title) %>%
  summarise(Average_Hours_Per_Week = mean(`Hours Per Week`),
            Average_Cost = mean(Cost))

ggplot(average_hours, aes(x = Title, y = Average_Hours_Per_Week, fill = as.factor(round(Average_Hours_Per_Week)), size = Average_Cost)) +
  geom_bar(stat = "identity") +
  labs(title = "Average Hours Per Week of Study by Major",
       x = "Major",
       y = "Average Hours Per Week",
       fill = "Average Hours Per Week",
       size = "Average Cost") +
  scale_fill_manual(values = c("skyblue", "orange", "green", "red")) +  # Color scale for different hours per week levels
  theme(axis.text.x = element_text(angle = 90, hjust = 1))
