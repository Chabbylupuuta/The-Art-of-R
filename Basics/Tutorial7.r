# Creating a simple factor
gender <- factor(c("Male", "Female", "Female", "Male", "Male"))

# Display the factor
gender

# Creating an ordered factor
priority <- factor(c("Low", "High", "Medium", "Low", "High"), 
                   levels = c("Low", "Medium", "High"), 
                   ordered = TRUE)

# Display the ordered factor
priority

# Creating an ordered factor
priority <- factor(c("Low", "High", "Medium", "Low", "High"), 
                   levels = c("Low", "Medium", "High"), 
                   ordered = TRUE)

# Display the ordered factor
priority

# Creating a table from the factor
gender_table <- table(gender)

# Display the table
gender_table

# Creating a table from two factors
multi_factor_table <- table(gender, priority)

# Display the table
multi_factor_table

# Example dataset
df <- data.frame(
  gender = factor(c("Male", "Female", "Male", "Female", "Female")),
  priority = factor(c("Low", "High", "Medium", "Low", "High"),
                    levels = c("Low", "Medium", "High"), ordered = TRUE)
)

# Create a table of two columns
table(df$gender, df$priority)

# Example data
survey_data <- data.frame(
  gender = factor(c("Male", "Female", "Male", "Female", "Female", "Male", "Female", "Male")),
  language = factor(c("R", "Python", "R", "Python", "R", "Python", "R", "Python"))
)

# Create a contingency table to show gender vs language preference

survey_table <- table(survey_data$gender, survey_data$language)

# Show the table

survey_table