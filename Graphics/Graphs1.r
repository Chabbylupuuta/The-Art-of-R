# ======================================================
# EXTENDED R GRAPH EXAMPLES
# From basics to advanced visualizations
# ======================================================

# ------------------------------------------------------
# 1. YOUR ORIGINAL EXAMPLES (preserved)
# ------------------------------------------------------

# Simple plot with three points (note: first vector has 3 values, second has 4? 
# Actually c(1,4,3.7) length 3, c(3,2,4,6) length 4 – R will recycle but gives warning.
# Let's correct it to matching lengths)
plot(c(1, 4, 3.7), c(3, 2, 4), main = "Original scatter", col = "blue", pch = 19)

# Blank plot with custom axes limits
plot(c(-3, 3), c(-1, 8), type = "n", xlab = "x", ylab = "y", main = "Empty plot area")

# Scatter plot with regression line
x <- c(3, 8, 5)
y <- c(1, 4, 8)
plot(x, y, main = "Scatter with regression line", col = "red", pch = 16)
lmout <- lm(y ~ x)
abline(lmout, col = "blue", lwd = 2)

# ------------------------------------------------------
# 2. LINE PLOTS
# ------------------------------------------------------

# Simple line plot
time <- 1:10
values <- c(2, 3, 5, 7, 11, 13, 17, 19, 23, 29)
plot(time, values, type = "b", col = "darkgreen", pch = 18, 
     main = "Line plot with points", xlab = "Time", ylab = "Values")

# Multiple lines on one plot
sales_2023 <- c(120, 135, 140, 155, 160, 175, 190, 210, 230, 245, 260, 280)
sales_2024 <- c(130, 145, 155, 170, 185, 200, 215, 235, 260, 280, 300, 325)
months <- 1:12
plot(months, sales_2023, type = "o", col = "steelblue", pch = 16, 
     ylim = c(100, 350), xlab = "Month", ylab = "Sales (USD)", 
     main = "Monthly Sales Comparison")
lines(months, sales_2024, type = "o", col = "orange", pch = 17)
legend("topleft", legend = c("2023", "2024"), 
       col = c("steelblue", "orange"), pch = c(16, 17), lty = 1)

# ------------------------------------------------------
# 3. HISTOGRAMS – distribution of data
# ------------------------------------------------------

# Generate random normal data
set.seed(123)
data_normal <- rnorm(1000, mean = 50, sd = 10)

# Basic histogram
hist(data_normal, main = "Histogram of Normal Data", 
     xlab = "Value", col = "lightblue", border = "white")

# Histogram with density curve
hist(data_normal, probability = TRUE, col = "lightgreen", 
     main = "Histogram with Density Curve")
lines(density(data_normal), col = "red", lwd = 2)

# ------------------------------------------------------
# 4. BARPLOTS – categorical data
# ------------------------------------------------------

# Create frequency table
fruit <- c("Apple", "Banana", "Orange", "Apple", "Apple", "Banana", 
           "Orange", "Orange", "Orange", "Banana")
fruit_table <- table(fruit)

# Basic barplot
barplot(fruit_table, main = "Fruit Frequency", 
        xlab = "Fruit", ylab = "Count", col = c("red", "yellow", "orange"))

# Horizontal barplot
barplot(fruit_table, horiz = TRUE, main = "Horizontal Barplot", 
        col = "skyblue", las = 1)  # las=1 makes labels horizontal

# ------------------------------------------------------
# 5. BOXPLOTS – showing distribution summaries
# ------------------------------------------------------

# Compare multiple groups
set.seed(456)
group_a <- rnorm(50, mean = 10, sd = 2)
group_b <- rnorm(50, mean = 12, sd = 2.5)
group_c <- rnorm(50, mean = 11, sd = 1.5)

boxplot(group_a, group_b, group_c, 
        names = c("Group A", "Group B", "Group C"),
        main = "Boxplot Comparison", 
        ylab = "Values", col = c("lightblue", "lightgreen", "lightcoral"))

# Using formula notation with a data frame
df <- data.frame(
  value = c(group_a, group_b, group_c),
  group = rep(c("A", "B", "C"), each = 50)
)
boxplot(value ~ group, data = df, main = "Boxplot by Group (formula)", 
        col = "lightyellow")

# ------------------------------------------------------
# 6. ADVANCED: Multiple plots in one window
# ------------------------------------------------------

# Set up 2x2 grid
par(mfrow = c(2, 2))

# Plot 1: scatter
plot(x, y, main = "Scatter", pch = 19, col = "darkred")

# Plot 2: histogram
hist(data_normal[1:100], main = "Histogram", col = "orange")

# Plot 3: boxplot
boxplot(group_a, group_b, main = "Boxplots", names = c("A", "B"))

# Plot 4: barplot
barplot(fruit_table, main = "Barplot", col = "purple")

# Reset to single plot
par(mfrow = c(1, 1))

# ------------------------------------------------------
# 7. CUSTOMIZATION: colors, symbols, text
# ------------------------------------------------------

# Colors by groups
x_vals <- 1:20
y_vals <- x_vals + rnorm(20, sd = 2)
groups <- ifelse(y_vals > mean(y_vals), "High", "Low")
colors <- ifelse(groups == "High", "forestgreen", "red")

plot(x_vals, y_vals, col = colors, pch = 19, cex = 1.5,
     main = "Colored by Group", xlab = "X", ylab = "Y")
text(x_vals, y_vals, labels = 1:20, pos = 3, cex = 0.8)
legend("topleft", legend = c("High", "Low"), 
       col = c("forestgreen", "red"), pch = 19)

# ------------------------------------------------------
# 8. SAVING PLOTS to file
# ------------------------------------------------------

# Save as PNG
png("my_plot.png", width = 800, height = 600, res = 120)
plot(x_vals, y_vals, main = "Saved Plot", col = "blue", pch = 16)
abline(lm(y_vals ~ x_vals), col = "red")
dev.off()  # close the graphics device
cat("Plot saved as my_plot.png\n")

# Save as PDF
pdf("my_plot.pdf", width = 8, height = 6)
plot(x_vals, y_vals, main = "PDF Output")
dev.off()
cat("Plot saved as my_plot.pdf\n")

# ------------------------------------------------------
# 9. ADDING EXTRA FEATURES: grid, legend, axis labels
# ------------------------------------------------------

plot(x_vals, y_vals, type = "b", col = "darkblue", lwd = 2,
     main = "Plot with Grid and Custom Axis",
     xlab = "Observation", ylab = "Measurement")
grid(nx = 5, ny = 5, col = "lightgray", lty = "dotted")
axis(side = 4, col.axis = "red", col = "red")  # right axis
legend("bottomright", legend = "Data series", col = "darkblue", 
       lty = 1, pch = 1, bty = "n")

# ------------------------------------------------------
# 10. INTERACTIVE PLOT (simple – zoomable)
# ------------------------------------------------------

# Works in RStudio or if X11 is supported
if(interactive()) {
  plot(x_vals, y_vals, main = "Zoom using mouse (if supported)")
  # You can use identify() to click points:
  # identify(x_vals, y_vals, labels = 1:20)
}

cat("\n========== END OF GRAPH EXAMPLES ==========\n")
cat("Run each section separately or all together.\n")
