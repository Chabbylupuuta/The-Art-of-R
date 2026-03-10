# Monte Carlo Simulation for Pi

set.seed(123)

n <- 100000  # Number of random points
x <- runif(n, min = -1, max = 1)  # Random X values
y <- runif(n, min = -1, max = 1)  # Random Y values

# Calculate distance from the origin

distance <- sqrt(x^2 + y^2)

# Estimate Pi

pi_estimate <- 4 * sum(distance <= 1) / n

# Display result

pi_estimate