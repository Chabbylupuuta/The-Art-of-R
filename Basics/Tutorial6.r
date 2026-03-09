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