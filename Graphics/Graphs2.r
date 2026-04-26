# ============================================
# EXTENDED: TWO DENSITY ESTIMATES ON SAME GRAPH
# With explanations and extra features
# ============================================

# 1. Simulate test scores for two exams
set.seed(123)  # for reproducible results

Exam1 <- rnorm(200, mean = 65, sd = 12)   # 200 scores, average 65
Exam2 <- rnorm(200, mean = 70, sd = 10)   # 200 scores, average 70

# Clip to 0-100 range (test scores can't go outside)
Exam1 <- pmax(0, pmin(100, Exam1))
Exam2 <- pmax(0, pmin(100, Exam2))

# Combine into a data frame (like testscores)
testscores <- data.frame(Exam1, Exam2)

# 2. Compute density estimates, restricting to 0-100
d1 <- density(testscores$Exam1, from = 0, to = 100)
d2 <- density(testscores$Exam2, from = 0, to = 100)

# 3. Plot with better labels, colors, and legend
plot(d1, 
     main = "Exam Score Distributions",   # title
     xlab = "Score (0-100)",              # x-axis label
     ylab = "Density",                    # y-axis label
     col = "blue",                        # line color for Exam1
     lwd = 2,                             # line width
     xlim = c(0, 100),                    # force x-axis limits
     ylim = c(0, max(d1$y, d2$y) * 1.1)   # auto y-limit with headroom
)

# Add second density line
lines(d2, col = "red", lwd = 2, lty = 2)  # lty=2 makes dashed line

# Add a legend
legend("topright", 
       legend = c("Exam 1", "Exam 2"),
       col = c("blue", "red"),
       lwd = 2,
       lty = c(1, 2))   # solid line for Exam1, dashed for Exam2

# 4. Fill areas under curves (optional - adds transparency)
# Re-plot with polygons (need to be careful with coordinates)
plot(d1, main = "Densities with Filled Areas", xlab = "Score", ylab = "Density",
     col = "blue", lwd = 2, xlim = c(0,100))
polygon(d1, col = rgb(0, 0, 1, 0.3), border = NA)  # semi-transparent blue
lines(d2, col = "red", lwd = 2)
polygon(d2, col = rgb(1, 0, 0, 0.3), border = NA)
legend("topright", legend = c("Exam 1", "Exam 2"), 
       fill = c(rgb(0,0,1,0.3), rgb(1,0,0,0.3)), border = NA)

# 5. Alternative: Histogram with density overlay (for comparison)
hist(testscores$Exam1, breaks = 20, col = rgb(0,0,1,0.2), 
     main = "Histograms with Density Overlay", xlab = "Score",
     ylim = c(0, 0.04), probability = TRUE)
hist(testscores$Exam2, breaks = 20, col = rgb(1,0,0,0.2), add = TRUE)
lines(d1, col = "blue", lwd = 2)
lines(d2, col = "red", lwd = 2)
legend("topright", legend = c("Exam 1", "Exam 2"), 
       fill = c(rgb(0,0,1,0.2), rgb(1,0,0,0.2)), 
       lty = 1, col = c("blue", "red"))

# 6. Using ggplot2 (more modern and easier for legends)
library(ggplot2)

# Reshape data to long format
testscores_long <- data.frame(
  Score = c(testscores$Exam1, testscores$Exam2),
  Exam = rep(c("Exam 1", "Exam 2"), each = nrow(testscores))
)

ggplot(testscores_long, aes(x = Score, color = Exam, fill = Exam)) +
  geom_density(alpha = 0.3, from = 0, to = 100) +
  labs(title = "Density Plot using ggplot2",
       x = "Score (0-100)", y = "Density") +
  theme_minimal() +
  scale_color_manual(values = c("blue", "red")) +
  scale_fill_manual(values = c("blue", "red")) +
  xlim(0, 100)
