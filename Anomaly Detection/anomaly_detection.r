# ======================================================
# CREDIT CONTROL ANOMALY DETECTION IN R
# For customer statement Excel file
# ======================================================

# 1. Install missing packages (run once)
packages <- c("readxl", "tidyverse", "lubridate", "ggplot2")
new_packages <- packages[!(packages %in% installed.packages()[,"Package"])]
if(length(new_packages)) install.packages(new_packages)

# 2. Load libraries
library(readxl)
library(tidyverse)
library(lubridate)
library(ggplot2)

# 3. Read the Excel file (adjust filename/path if needed)
file_name <- "DOC-20251117-WA0009..xlsx"  # change if different
cat("Reading file:", file_name, "\n")

raw_data <- read_excel(file_name, 
                       sheet = "Table 1", 
                       skip = 5, 
                       col_names = FALSE)

# 4. Clean and prepare data
clean_data <- raw_data %>%
  select(Invoice_No = 1, 
         Date = 3, 
         Description = 6, 
         Prev_Balance = 8, 
         Charge_Amount = 9, 
         Account_Balance = 10) %>%
  filter(!is.na(Date)) %>%                 # remove footer rows
  mutate(Date = ymd_hms(Date),             # convert to datetime
         Prev_Balance = as.numeric(Prev_Balance),
         Charge_Amount = as.numeric(Charge_Amount),
         Account_Balance = as.numeric(Account_Balance)) %>%
  arrange(Date) %>%
  mutate(Type = ifelse(Description == "Payment", "Payment", "Invoice"),
         Abs_Amount = abs(Charge_Amount),
         Days_Since_Prev = c(0, diff(Date)))   # days since last transaction

cat("Data loaded:", nrow(clean_data), "transactions\n")

# 5. ANOMALY DETECTION RULES

# ---- A. Unusual invoice amounts (IQR method) ----
invoice_amt <- clean_data %>% filter(Type == "Invoice") %>% pull(Charge_Amount)
if(length(invoice_amt) > 0) {
  Q1_i <- quantile(invoice_amt, 0.25, na.rm = TRUE)
  Q3_i <- quantile(invoice_amt, 0.75, na.rm = TRUE)
  IQR_i <- Q3_i - Q1_i
  lower_i <- Q1_i - 1.5 * IQR_i
  upper_i <- Q3_i + 1.5 * IQR_i
  
  anomaly_invoice <- clean_data %>%
    filter(Type == "Invoice", 
           Charge_Amount < lower_i | Charge_Amount > upper_i) %>%
    mutate(Anomaly_Type = "Unusual Invoice Amount")
} else {
  anomaly_invoice <- tibble()
}

# ---- B. Unusual payment amounts ----
payment_amt <- clean_data %>% filter(Type == "Payment") %>% pull(Abs_Amount)
if(length(payment_amt) > 0) {
  Q1_p <- quantile(payment_amt, 0.25, na.rm = TRUE)
  Q3_p <- quantile(payment_amt, 0.75, na.rm = TRUE)
  IQR_p <- Q3_p - Q1_p
  lower_p <- Q1_p - 1.5 * IQR_p
  upper_p <- Q3_p + 1.5 * IQR_p
  
  anomaly_payment <- clean_data %>%
    filter(Type == "Payment", 
           Abs_Amount < lower_p | Abs_Amount > upper_p) %>%
    mutate(Anomaly_Type = "Unusual Payment Amount")
} else {
  anomaly_payment <- tibble()
}

# ---- C. Long gap without payment (>45 days while balance positive) ----
long_gap <- clean_data %>%
  mutate(Prev_Type = lag(Type),
         Prev_Bal_Pos = lag(Account_Balance, default = 0) > 0) %>%
  filter(Type == "Invoice", 
         Prev_Type != "Payment", 
         Days_Since_Prev > 45) %>%
  mutate(Anomaly_Type = "Long gap without payment (>45 days)")

# ---- D. Negative balance (overpayment) ----
negative_balance <- clean_data %>%
  filter(Account_Balance < 0) %>%
  mutate(Anomaly_Type = "Negative balance (overpayment)")

# ---- E. Rapid balance increase (>50% from previous balance) ----
rapid_increase <- clean_data %>%
  filter(Type == "Invoice") %>%
  mutate(Balance_Change = Account_Balance - lag(Account_Balance),
         Pct_Change = Balance_Change / lag(Account_Balance)) %>%
  filter(Pct_Change > 0.5 & !is.na(Pct_Change)) %>%
  mutate(Anomaly_Type = "Rapid balance increase (>50%)")

# 6. Combine all anomalies
all_anomalies <- bind_rows(anomaly_invoice,
                           anomaly_payment,
                           long_gap,
                           negative_balance,
                           rapid_increase) %>%
  arrange(Date) %>%
  select(Date, Description, Charge_Amount, Account_Balance, Anomaly_Type)

# 7. Output results
cat("\n========== ANOMALY REPORT ==========\n")
if(nrow(all_anomalies) == 0) {
  cat("No anomalies detected.\n")
} else {
  print(all_anomalies)
}

# Save CSV report
write_csv(all_anomalies, "credit_control_anomalies.csv")
cat("\nCSV report saved: credit_control_anomalies.csv\n")

# 8. Visualize anomalies on balance timeline
p <- ggplot(clean_data, aes(x = Date, y = Account_Balance)) +
  geom_line(color = "gray50", size = 0.8) +
  geom_point(data = all_anomalies, 
             aes(x = Date, y = Account_Balance, color = Anomaly_Type), 
             size = 3) +
  labs(title = "Account Balance with Detected Anomalies",
       x = "Date", y = "Balance (ZMW)",
       color = "Anomaly Type") +
  theme_minimal() +
  theme(legend.position = "bottom")

# Display plot
print(p)

# Save plot as PNG
ggsave("anomaly_plot.png", p, width = 10, height = 6)
cat("Plot saved: anomaly_plot.png\n")

# 9. Summary statistics
cat("\n========== SUMMARY STATISTICS ==========\n")
cat("Total transactions:", nrow(clean_data), "\n")
cat("Total invoices:", sum(clean_data$Type == "Invoice"), "\n")
cat("Total payments:", sum(clean_data$Type == "Payment"), "\n")
cat("Current balance:", tail(clean_data$Account_Balance, 1), "ZMW\n")
cat("Anomalies found:", nrow(all_anomalies), "\n")

# 10. Optional: Quick check of latest status
latest <- tail(clean_data, 1)
cat("\nLatest transaction on:", as.Date(latest$Date), 
    "-", latest$Description, "- Balance:", latest$Account_Balance, "ZMW\n")
