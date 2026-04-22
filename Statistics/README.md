# 📊 Tutorial 16: Statistics & Probability in R 🎲

Welcome to the **Statistical Intelligence** module! This section of my portfolio documents the transition from basic reporting to **Advanced Financial Inference**. Here, I leverage R's mathematical core to turn raw ledger data into predictive models. 🇿🇲

---

## 🎯 The Vision: Data-Driven Credit Control
In the high-stakes environment of telecommunications and banking, a number is more than a digit—it is a behavioral signal. By applying Statistics and Probability, I provide the **Bank of Zambia** and **Zamtel** with a "Weather Forecast" for their finances.

* **Risk Modeling:** Utilizing probability density functions to calculate the "Value at Risk" (VaR) for corporate accounts. 📉
* **Anomaly Detection:** Applying standard deviation to identify "Statistical Outliers"—accounts that deviate dangerously from normal payment behavior. 🚨
* **Predictive Forecasting:** Transitioning from reactive collection to proactive risk mitigation through Linear Regression. 🏛️

---

## 🛠️ The Statistical Toolkit
R is the gold standard for statistical computing. In this module, I master three critical domains:

1.  **Descriptive Analytics:** Utilizing `mean()`, `median()`, and `IQR()` to find the "pulse" of the portfolio.
2.  **Probability Theory:** Implementing `dnorm`, `pnorm`, and `qnorm` to simulate financial stress tests and credit default probabilities.
3.  **Inferential Modeling:** Mastering `cor()` for relationship strength and `lm()` (Linear Models) to predict future liquidity.

---

## 💻 Key Concepts & Code Milestones

### 1. Central Tendency & Dispersion: The "Financial Pulse" 📏
Before predicting the future, we must define the "Normal." I use R to calculate the spread of debt across the portfolio to understand our exposure.
```r
# Analyzing corporate account balances
summary(account_balances)

# Calculating Variance and Standard Deviation
# High SD = Unpredictable cash flow; Low SD = Stable collections
standard_dev <- sd(account_balances) 
coefficient_of_variation <- (standard_dev / mean(account_balances)) * 100
# What is the probability that a client's debt will exceed ZMW 20,000?
# Assuming a Mean of 12,000 and SD of 4,000
risk_level <- pnorm(20000, mean = 12000, sd = 4000, lower.tail = FALSE)

cat("The mathematical probability of this risk event is:", round(risk_level * 100, 2), "%")

# Building a Linear Model (lm)
# Predicts 'Amount_Paid' based on 'Days_Since_Invoice'
model <- lm(Amount_Paid ~ Days_Since_Invoice, data = zamtel_ledger)

# Visualizing the trend line
abline(model, col = "red", lwd = 2)
