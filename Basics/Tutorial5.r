# Example: Creating a simple data frame
student_data <- data.frame(
  Name = c("Alice", "Bob", "Charlie"),
  Age = c(20, 22, 21),
  Score = c(88, 92, 85)
)
print(student_data)

student_data$Name  # Accessing a specific column
student_data[1, ]  # Accessing the first row
student_data$Passed <- c(TRUE, TRUE, FALSE)
student_data$Passed <- NULL  # Remove column
# Merging two data frames
extra_info <- data.frame(Name = c("Alice", "Bob"), Major = c("Math", "Physics"))
merged_data <- merge(student_data, extra_info, by = "Name")
print(merged_data)