# Tutorial10_Extended.R
# Comprehensive Guide to Functions in R
# This file is designed as a readable tutorial with detailed examples

############################################################
# SECTION 1: INTRODUCTION TO FUNCTIONS
############################################################

# A function is a reusable block of code

greet <- function(name) {
  paste("Hello", name)
}
print(greet("Chabota"))

############################################################
# SECTION 2: MATHEMATICAL FUNCTIONS
############################################################

# Area of a circle
area_circle <- function(r) {
  pi * r^2
}
print(area_circle(5))

# Temperature conversion
celsius_to_fahrenheit <- function(c) {
  (c * 9/5) + 32
}
print(celsius_to_fahrenheit(30))

############################################################
# SECTION 3: LOGICAL FUNCTIONS
############################################################

# Pass or fail checker
check_pass <- function(score) {
  if (score >= 50) {
    "Pass"
  } else {
    "Fail"
  }
}
print(check_pass(65))

############################################################
# SECTION 4: WORKING WITH VECTORS
############################################################

# Find maximum value
find_max <- function(vec) {
  max(vec)
}
print(find_max(c(3,7,2,9,5)))

# Normalize vector
normalize <- function(vec) {
  (vec - min(vec)) / (max(vec) - min(vec))
}
print(normalize(c(10,20,30)))

############################################################
# SECTION 5: LOOPS INSIDE FUNCTIONS
############################################################

# Sum using loop
sum_loop <- function(n) {
  total <- 0
  for (i in 1:n) {
    total <- total + i
  }
  total
}
print(sum_loop(10))

############################################################
# SECTION 6: DATA FRAME OPERATIONS
############################################################

students <- data.frame(
  name = c("A", "B", "C"),
  marks = c(70, 85, 60)
)

# Get top student
top_student <- function(df) {
  df[which.max(df$marks), ]
}
print(top_student(students))

############################################################
# SECTION 7: STRING FUNCTIONS
############################################################

# Count characters
count_chars <- function(text) {
  nchar(text)
}
print(count_chars("R Programming"))

# Convert to uppercase
to_upper <- function(text) {
  toupper(text)
}
print(to_upper("hello"))

############################################################
# SECTION 8: ADVANCED FUNCTIONS
############################################################

# Function returning another function
multiplier <- function(x) {
  function(y) {
    x * y
  }
}

times3 <- multiplier(3)
print(times3(10))

############################################################
# SECTION 9: APPLY FAMILY
############################################################

nums <- list(1:3, 4:6, 7:9)

# Sum each list
print(lapply(nums, sum))

############################################################
# SECTION 10: ERROR HANDLING
############################################################

safe_log <- function(x) {
  if (x <= 0) {
    return("Invalid input")
  }
  log(x)
}
print(safe_log(-5))

############################################################
# SECTION 11: RECURSION
############################################################

factorial_recursive <- function(n) {
  if (n == 0) return(1)
  n * factorial_recursive(n - 1)
}
print(factorial_recursive(5))

############################################################
# SECTION 12: PRACTICE CHALLENGES
############################################################

# 1. Write a function to check prime numbers
is_prime <- function(n) {
  if (n <= 1) return(FALSE)
  for (i in 2:(n-1)) {
    if (n %% i == 0) return(FALSE)
  }
  return(TRUE)
}
print(is_prime(7))

# 2. Reverse a string
reverse_string <- function(text) {
  paste(rev(strsplit(text, "")[[1]]), collapse = "")
}
print(reverse_string("R Language"))

############################################################
# END OF FILE
############################################################
