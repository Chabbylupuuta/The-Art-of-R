# Example: A simple function to calculate the area of a rectangle
calc_area <- function(length, width) {
  return(length * width)
}
print(calc_area(5, 3))  # Output: 15

greet <- function(name = "Guest") {
  print(paste("Hello,", name))
}
greet("Alice")  # Output: Hello, Alice
greet()         # Output: Hello, Guest

stats <- function(x, y) {

  sum_val <- x + y
  product_val <- x * y

  list(sum = sum_val, product = product_val)
}

stats(3,4)

check_even <- function(x) {

  if (x %% 2 == 0) {
    print("Even")
  } else {
    print("Odd")
  }

}

check_even(7)

check_sign <- function(x) {
  if (x > 0) {
    return("Positive")
  } else if (x < 0) {
    return("Negative")
  } else {
    return("Zero")
  }
}

# Test the function
check_sign(10)   # Output: Positive
check_sign(-5)   # Output: Negative
check_sign(0)    # Output: Zero


factorial <- function(n) {
  result <- 1
  for (i in 1:n) {
    result <- result * i
  }
  return(result)
}

# Test the function
factorial(5)  # Output: 120

factorial <- function(n) {
  result <- 1
  for (i in 1:n) {
    result <- result * i
  }
  return(result)
}

# Test the function
factorial(5)  # Output: 120