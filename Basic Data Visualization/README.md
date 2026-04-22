# 📉 Tutorial 15: Basic Data Visualization in R 🎨

Welcome to the **Data Visualization** module! This section demonstrates how to translate complex financial ledgers into intuitive, high-impact graphics. In Credit Control, a well-crafted chart is the fastest way to communicate risk to management. 🇿🇲

---

## 🎯 The Vision: Visualizing Credit Risk
Data visualization isn't just about "pretty pictures"—it's about **Pattern Recognition**. By mapping Zamtel's corporate debt, I can instantly identify:
* **Exposure Hotspots:** Which sectors or clients hold the most debt? 🚩
* **Payment Trends:** Are collections improving month-over-month? 📈
* **Outlier Detection:** Spotting accounts that deviate from standard payment behavior. 🔍

---

## 🛠️ The Visualization Stack
I utilize two primary approaches to graphics in R:

1.  **Base R Graphics:** The built-in "Quick-Look" engine for immediate data inspection.
2.  **`ggplot2`:** The professional industry standard based on the *Grammar of Graphics*. This allows for layered, highly customized, and publication-ready charts.

---

## 💻 Key Concepts & Gallery

### 1. Categorical Comparisons (Bar Charts) 📊
Used to compare debt levels across different corporate clients like **Lactalis-Parmalat** or **Blue Nile**.
```r
# ggplot2: Comparing account balances
ggplot(df, aes(x = Client, y = Balance, fill = Status)) +
  geom_bar(stat = "identity") +
  theme_minimal() +
  labs(title = "Corporate Debt Distribution", y = "Balance (ZMW)")


# Tracking payment progress over time
ggplot(history, aes(x = Month, y = Outstanding_Amount, group = 1)) +
  geom_line(color = "#0066cc", size = 1) +
  geom_point() +
  labs(title = "Monthly Debt Reduction Trend")

# Visualizing debt variance
ggplot(df, aes(x = Category, y = Balance)) +
  geom_boxplot(fill = "orange", alpha = 0.7) +
  labs(title = "Debt Variance by Business Sector")
# Visualizing debt variance
ggplot(df, aes(x = Category, y = Balance)) +
  geom_boxplot(fill = "orange", alpha = 0.7) +
  labs(title = "Debt Variance by Business Sector")
