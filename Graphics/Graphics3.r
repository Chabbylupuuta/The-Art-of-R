# ======================================================
# Graphics3.R
# POLYNOMIAL REGRESSION & THE Polyreg CLASS
# A visual guide to capturing curved relationships in data
# ======================================================

# ------------------------------------------------------
# SECTION 0: UNDERSTANDING THE CONCEPT
# ------------------------------------------------------
# Linear regression draws a straight line:     y = b0 + b1*x
# Polynomial regression draws a curve:          y = b0 + b1*x + b2*x^2 + ... + bn*x^n
#
# WHY USE IT? Real-world relationships are often curved.
# - As advertising spending increases, sales grow quickly, then plateau.
# - As temperature rises, crop yields increase, then fall.
#
# The "polyreg" package automates building and evaluating these models.
# ------------------------------------------------------


# ------------------------------------------------------
# SECTION 1: SETUP & DATA SIMULATION
# ------------------------------------------------------

# 1a. Install and load required packages
# Uncomment the line below if you haven't installed the polyreg package yet
# install.packages("polyreg")

library(polyreg)   # Automated polynomial regression
library(ggplot2)   # Modern, beautiful plots
library(gridExtra) # For arranging multiple plots

# 1b. Create a synthetic dataset with a clear curved relationship
set.seed(2025)  # So everyone gets the same "random" data

# Suppose we study how exam performance (y) changes with hours studied (x)
hours_studied <- seq(0, 10, length.out = 100)           # 0 to 10 hours
true_relationship <- 20 + 15*hours_studied - 1.2*hours_studied^2   # A curve
noise <- rnorm(100, mean = 0, sd = 12)                  # Real-world randomness
exam_score <- true_relationship + noise                 # Our observed data

study_data <- data.frame(
  Hours = hours_studied,
  Score = exam_score
)

# Quick visualization of the raw relationship
ggplot(study_data, aes(x = Hours, y = Score)) +
  geom_point(alpha = 0.6, color = "steelblue") +
  labs(title = "Hours Studied vs. Exam Score",
       subtitle = "Notice how scores rise then fall - a curved pattern!",
       x = "Hours Studied", y = "Exam Score") +
  theme_minimal()


# ------------------------------------------------------
# SECTION 2: LINEAR REGRESSION (THE WRONG MODEL)
# ------------------------------------------------------

# Let's first see what happens if we force a straight line on a curve
linear_model <- lm(Score ~ Hours, data = study_data)

# Extract coefficients
print("LINEAR MODEL COEFFICIENTS:")
print(coef(linear_model))
# Interpretation: For each additional hour, score increases by about X points.
# BUT this misses the plateau and decline at high study hours.

# Visualize the poor fit
study_data$Linear_Pred <- predict(linear_model)

ggplot(study_data, aes(x = Hours, y = Score)) +
  geom_point(alpha = 0.4, color = "gray50") +
  geom_line(aes(y = Linear_Pred), color = "red", size = 1.2) +
  labs(title = "Linear Regression: A Poor Fit for Curved Data",
       subtitle = "The red line misses the curve entirely",
       x = "Hours Studied", y = "Exam Score") +
  theme_minimal() +
  annotate("text", x = 5, y = 80, 
           label = paste("R² =", round(summary(linear_model)$r.squared, 3)),
           color = "darkred", size = 5)


# ------------------------------------------------------
# SECTION 3: POLYNOMIAL REGRESSION STEP-BY-STEP
# ------------------------------------------------------

# 3a. Adding a squared term (Degree 2 polynomial)
#     Formula becomes: Score ~ Hours + Hours^2
study_data$Hours_Squared <- study_data$Hours^2
poly_model_2 <- lm(Score ~ Hours + Hours_Squared, data = study_data)

# Compare models
print("LINEAR VS. POLYNOMIAL (DEGREE 2) COMPARISON:")
print(paste("Linear R²:   ", round(summary(linear_model)$r.squared, 4)))
print(paste("Poly Degree 2 R²:", round(summary(poly_model_2)$r.squared, 4)))
# The higher R² shows the polynomial captures more of the pattern.

# 3b. Using poly() function (more stable and easier)
#     poly(x, degree=2, raw=TRUE) does the same thing safely
poly_model_2_alt <- lm(Score ~ poly(Hours, degree = 2, raw = TRUE), data = study_data)

# 3c. Let's predict and visualize the degree 2 polynomial fit
study_data$Poly2_Pred <- predict(poly_model_2)

# Create a prediction for a smooth curve across all hour values
smooth_hours <- data.frame(Hours = seq(0, 10, length.out = 300))
smooth_hours$Hours_Squared <- smooth_hours$Hours^2
smooth_hours$Poly2_Pred <- predict(poly_model_2, newdata = smooth_hours)

ggplot(study_data, aes(x = Hours, y = Score)) +
  geom_point(alpha = 0.4, color = "gray50") +
  geom_line(data = smooth_hours, aes(x = Hours, y = Poly2_Pred),
            color = "darkgreen", size = 1.5) +
  labs(title = "Polynomial Regression (Degree 2): A Perfect Curve!",
       subtitle = "The green curve captures the rise, plateau, and decline in scores",
       x = "Hours Studied", y = "Exam Score") +
  theme_minimal() +
  annotate("text", x = 5, y = 80,
           label = paste("R² =", round(summary(poly_model_2)$r.squared, 3)),
           color = "darkgreen", size = 5)


# ------------------------------------------------------
# SECTION 4: CHOOSING THE RIGHT DEGREE (THE BIAS-VARIANCE TRADEOFF)
# ------------------------------------------------------

# More degrees = more wiggles = better fit on training data, BUT risk of overfitting

# Create a grid of hours for smooth prediction curves
prediction_grid <- data.frame(Hours = seq(0, 10, length.out = 200))
prediction_grid$Hours_Squared <- prediction_grid$Hours^2
prediction_grid$Hours_Cubed  <- prediction_grid$Hours^3

# Fit models of increasing complexity
# Degree 2 (Quadratic)
poly_deg2 <- lm(Score ~ poly(Hours, degree = 2, raw = TRUE), data = study_data)
prediction_grid$Pred_Deg2 <- predict(poly_deg2, newdata = prediction_grid)

# Degree 4 (More flexible)
poly_deg4 <- lm(Score ~ poly(Hours, degree = 4, raw = TRUE), data = study_data)
prediction_grid$Pred_Deg4 <- predict(poly_deg4, newdata = prediction_grid)

# Degree 9 (Very flexible - likely overfitting)
poly_deg9 <- lm(Score ~ poly(Hours, degree = 9, raw = TRUE), data = study_data)
prediction_grid$Pred_Deg9 <- predict(poly_deg9, newdata = prediction_grid)

# Visual comparison
plot1 <- ggplot(study_data, aes(x = Hours, y = Score)) +
  geom_point(alpha = 0.3, size = 0.8) +
  geom_line(data = prediction_grid, aes(x = Hours, y = Pred_Deg2), color = "blue", size = 1.2) +
  labs(title = "Degree 2: Simple curve", x = "Hours", y = "") +
  theme_minimal()

plot2 <- ggplot(study_data, aes(x = Hours, y = Score)) +
  geom_point(alpha = 0.3, size = 0.8) +
  geom_line(data = prediction_grid, aes(x = Hours, y = Pred_Deg4), color = "purple", size = 1.2) +
  labs(title = "Degree 4: More flexible", x = "Hours", y = "") +
  theme_minimal()

plot3 <- ggplot(study_data, aes(x = Hours, y = Score)) +
  geom_point(alpha = 0.3, size = 0.8) +
  geom_line(data = prediction_grid, aes(x = Hours, y = Pred_Deg9), color = "red", size = 1.2) +
  labs(title = "Degree 9: Overfitting! (Notice the wild oscillations)",
       subtitle = "The red line chases random noise, not the true pattern",
       x = "Hours", y = "") +
  theme_minimal()

# Arrange all three plots side by side
grid.arrange(plot1, plot2, plot3, ncol = 3)


# ------------------------------------------------------
# SECTION 5: USING THE POLYREG PACKAGE FOR AUTOMATED MODELING
# ------------------------------------------------------

# 5a. Basic polyfit() - Builds polynomial models of various degrees
#     and automatically selects the best one using cross-validation

# Prepare data: separate predictors (x) and response (y)
x_data <- study_data[, "Hours", drop = FALSE]
y_data <- study_data$Score

# Run automated polynomial regression
# polyfit() will try degrees 1 through 5 by default
poly_model_auto <- polyFit(x_data, y_data, deg = 5)

# Examine the cross-validation results
print("AUTOMATIC POLYREG MODEL SUMMARY:")
print(poly_model_auto)

# 5b. Make predictions with the automatically selected model
# Create new data for prediction
new_hours <- data.frame(Hours = c(2.5, 5.5, 8.0))
predicted_scores <- predict(poly_model_auto, new_hours)

print("PREDICTIONS FOR NEW HOURS:")
print(data.frame(Hours = new_hours$Hours, Predicted_Score = predicted_scores))

# 5c. Visualize the polyreg best model
# First, manually extract which degree was chosen
best_degree <- poly_model_auto$degree  # The degree with lowest validation error

# Fit that specific degree model manually for smooth plotting
best_model <- lm(Score ~ poly(Hours, degree = best_degree, raw = TRUE), data = study_data)

smooth_pred <- data.frame(Hours = seq(0, 10, length.out = 200))
smooth_pred$Prediction <- predict(best_model, newdata = smooth_pred)

ggplot(study_data, aes(x = Hours, y = Score)) +
  geom_point(alpha = 0.4, color = "gray50") +
  geom_line(data = smooth_pred, aes(x = Hours, y = Prediction),
            color = "darkorange", size = 1.8) +
  labs(title = paste0("POLYREG'S BEST MODEL (Degree ", best_degree, ")"),
       subtitle = "The package automatically finds the balance between fit and overfitting",
       x = "Hours Studied", y = "Exam Score") +
  theme_minimal()


# ------------------------------------------------------
# SECTION 6: HANDLING MULTIPLE PREDICTORS (MULTIVARIATE POLYNOMIALS)
# ------------------------------------------------------

# Real-world data usually has multiple factors. Polyreg handles interactions between them.
# Example: Predict exam score based on Hours Studied AND Sleep Hours

set.seed(2025)
n <- 150
hours_study <- runif(n, 0, 10)
hours_sleep <- runif(n, 4, 9)  # Between 4 and 9 hours of sleep

# True relationship includes interaction: Studying helps more when well-rested
true_score <- 30 + 
              8*hours_study - 0.4*hours_study^2 +   # Quadratic effect of study
              3*hours_sleep +                        # Linear effect of sleep
              1.5*hours_study * hours_sleep          # INTERACTION term

noise <- rnorm(n, 0, 8)
score <- true_score + noise

mult_data <- data.frame(Study = hours_study, Sleep = hours_sleep, Score = score)

# Run polyreg on two predictors
polyreg_multivariate <- polyFit(mult_data[, c("Study", "Sleep")],
                                 mult_data$Score,
                                 deg = 2)

print("MULTIVARIATE MODEL SUMMARY:")
print(polyreg_multivariate)

# Create a 3D-style contour plot to visualize the interaction
library(reshape2)

# Create grid of predictor values
study_grid <- seq(0, 10, length.out = 50)
sleep_grid <- seq(4, 9, length.out = 50)
prediction_grid <- expand.grid(Study = study_grid, Sleep = sleep_grid)

# Predict using the polyreg model
prediction_grid$Score <- predict(polyreg_multivariate, prediction_grid)

# Reshape for contour plot
contour_data <- acast(prediction_grid, Sleep ~ Study, value.var = "Score")

contour_plot <- ggplot(prediction_grid, aes(x = Study, y = Sleep, z = Score)) +
  geom_contour_filled(bins = 8) +
  theme_minimal() +
  labs(title = "Contour Plot: Exam Score as a Function of Study & Sleep",
       subtitle = "Warmer colors = higher scores. Shows how study and sleep interact.",
       x = "Hours Studied", y = "Hours Slept", fill = "Predicted\nScore") +
  scale_fill_viridis_d(option = "plasma")

print(contour_plot)


# ------------------------------------------------------
# SECTION 7: PRACTICAL TIPS & TROUBLESHOOTING
# ------------------------------------------------------

# ✅ DO:
# 1. Standardize your predictors if they have very different scales
# 2. Use cross-validation (polyreg does this automatically!)
# 3. Start with a low max degree (e.g., 3 or 4) to avoid overfitting
# 4. Always visualize your model's fit

# ❌ DON'T:
# 1. Use a very high polynomial degree with small datasets
# 2. Forget that polynomial models extrapolate poorly beyond your data range
# 3. Trust the model blindly - always check residual plots

# Quick residual check for our final model
final_model <- lm(Score ~ poly(Hours, degree = best_degree, raw = TRUE), data = study_data)
study_data$Residuals <- resid(final_model)

ggplot(study_data, aes(x = Hours, y = Residuals)) +
  geom_point(color = "darkblue", size = 2, alpha = 0.6) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "red", size = 1) +
  stat_smooth(method = "loess", color = "darkgreen", se = FALSE) +
  labs(title = "Residual Plot: Verifying Our Model",
       subtitle = "Ideally, residuals should be randomly scattered around zero",
       x = "Hours Studied", y = "Residuals (Predicted - Actual)") +
  theme_minimal()


# ------------------------------------------------------
# SECTION 8: CONCLUSION - WHAT YOU'VE LEARNED
# ------------------------------------------------------

print("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
print("🎓 SUMMARY: POLYNOMIAL REGRESSION")
print("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
print("1. Linear models fail when relationships are curved.")
print("2. Adding squared, cubed, etc. terms creates flexible curves.")
print("3. Higher degrees fit training data better but risk overfitting.")
print("4. The polyreg package automates finding the best degree.")
print("5. Polynomials can handle multiple predictors and interactions.")
print("━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━━")
print("✅ Your Graphics3.r is complete! Ready for credit control analysis.")
