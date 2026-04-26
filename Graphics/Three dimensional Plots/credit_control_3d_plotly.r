# ======================================================
# CREDIT CONTROL - INTERACTIVE 3D ANOMALY DETECTION
# Using plotly for rotatable, shareable 3D visualizations
# ======================================================

# 1. Install required packages (run once)
if (!require("readxl")) install.packages("readxl")
if (!require("tidyverse")) install.packages("tidyverse")
if (!require("lubridate")) install.packages("lubridate")
if (!require("plotly")) install.packages("plotly")
if (!require("htmlwidgets")) install.packages("htmlwidgets")

# 2. Load libraries
library(readxl)
library(tidyverse)
library(lubridate)
library(plotly)
library(htmlwidgets)

# 3. Read and clean the Excel statement
file_name <- "DOC-20251117-WA0009..xlsx"  # adjust path if needed
cat("Reading file:", file_name, "\n")

raw_data <- read_excel(file_name, 
                       sheet = "Table 1", 
                       skip = 5, 
                       col_names = FALSE)

clean_data <- raw_data %>%
  select(Invoice_No = 1, 
         Date = 3, 
         Description = 6, 
         Prev_Balance = 8, 
         Charge_Amount = 9, 
         Account_Balance = 10) %>%
  filter(!is.na(Date)) %>%
  mutate(Date = ymd_hms(Date),
         Prev_Balance = as.numeric(Prev_Balance),
         Charge_Amount = as.numeric(Charge_Amount),
         Account_Balance = as.numeric(Account_Balance)) %>%
  arrange(Date) %>%
  mutate(Type = ifelse(Description == "Payment", "Payment", "Invoice"),
         Abs_Amount = abs(Charge_Amount),
         Days_Since_Prev = c(0, diff(Date)))

cat("Data loaded:", nrow(clean_data), "transactions\n")

# 4. Detect anomalies (same logic as earlier, but we'll keep them as columns)
#    We'll flag each row with anomaly types
clean_data$Anomaly <- "Normal"

# A. Unusual invoice amounts (IQR method)
invoice_amt <- clean_data %>% filter(Type == "Invoice") %>% pull(Charge_Amount)
if(length(invoice_amt) > 0) {
  Q1_i <- quantile(invoice_amt, 0.25, na.rm = TRUE)
  Q3_i <- quantile(invoice_amt, 0.75, na.rm = TRUE)
  IQR_i <- Q3_i - Q1_i
  lower_i <- Q1_i - 1.5 * IQR_i
  upper_i <- Q3_i + 1.5 * IQR_i
  clean_data <- clean_data %>%
    mutate(Anomaly = ifelse(Type == "Invoice" & (Charge_Amount < lower_i | Charge_Amount > upper_i),
                            "Unusual Invoice Amount", Anomaly))
}

# B. Unusual payment amounts
payment_amt <- clean_data %>% filter(Type == "Payment") %>% pull(Abs_Amount)
if(length(payment_amt) > 0) {
  Q1_p <- quantile(payment_amt, 0.25, na.rm = TRUE)
  Q3_p <- quantile(payment_amt, 0.75, na.rm = TRUE)
  IQR_p <- Q3_p - Q1_p
  lower_p <- Q1_p - 1.5 * IQR_p
  upper_p <- Q3_p + 1.5 * IQR_p
  clean_data <- clean_data %>%
    mutate(Anomaly = ifelse(Type == "Payment" & (Abs_Amount < lower_p | Abs_Amount > upper_p),
                            "Unusual Payment Amount", Anomaly))
}

# C. Long gap without payment (>45 days while balance positive)
clean_data <- clean_data %>%
  mutate(Prev_Type = lag(Type),
         Prev_Bal_Pos = lag(Account_Balance, default = 0) > 0) %>%
  mutate(Anomaly = ifelse(Type == "Invoice" & Prev_Type != "Payment" & Days_Since_Prev > 45,
                          "Long payment gap", Anomaly))

# D. Negative balance (overpayment)
clean_data <- clean_data %>%
  mutate(Anomaly = ifelse(Account_Balance < 0, "Overpayment (negative balance)", Anomaly))

# E. Rapid balance increase (>50% from previous balance)
clean_data <- clean_data %>%
  mutate(Balance_Change = Account_Balance - lag(Account_Balance),
         Pct_Change = Balance_Change / lag(Account_Balance)) %>%
  mutate(Anomaly = ifelse(Type == "Invoice" & !is.na(Pct_Change) & Pct_Change > 0.5,
                          "Rapid balance increase", Anomaly))

# 5. Convert Date to numeric for plotting (or keep as date - plotly handles dates)
#    We'll keep as date, but for axis we'll use numeric index as backup
#    For 3D, it's easier to use numeric days since first transaction
clean_data$Days_Since_Start <- as.numeric(clean_data$Date - min(clean_data$Date))

# 6. Create interactive 3D scatter plot with plotly
#    X = Days since first transaction (or Date)
#    Y = Charge Amount (positive = invoice, negative = payment)
#    Z = Account Balance
#    Color = Anomaly type (or "Normal")
#    Size = Absolute charge amount (bigger points for larger charges)

# Define color palette for anomalies
anomaly_colors <- c(
  "Normal" = "grey",
  "Unusual Invoice Amount" = "red",
  "Unusual Payment Amount" = "orange",
  "Long payment gap" = "purple",
  "Overpayment (negative balance)" = "blue",
  "Rapid balance increase" = "darkred"
)

# Ensure all anomaly types in data are included in the palette
unique_anomalies <- unique(clean_data$Anomaly)
missing_colors <- setdiff(unique_anomalies, names(anomaly_colors))
if(length(missing_colors) > 0) {
  # Assign default colors for any missing anomaly types
  for(m in missing_colors) anomaly_colors[m] <- "black"
}

# Create hover text
clean_data$HoverText <- paste(
  "Date:", format(clean_data$Date, "%Y-%m-%d"),
  "<br>Description:", clean_data$Description,
  "<br>Invoice No:", clean_data$Invoice_No,
  "<br>Charge:", round(clean_data$Charge_Amount, 2),
  "<br>Balance:", round(clean_data$Account_Balance, 2),
  "<br>Anomaly:", clean_data$Anomaly
)

# Create the 3D plot
fig <- plot_ly(
  clean_data,
  x = ~Days_Since_Start,
  y = ~Charge_Amount,
  z = ~Account_Balance,
  color = ~Anomaly,
  colors = anomaly_colors,
  size = ~abs(Charge_Amount) + 1,   # size proportional to charge abs (avoid zero)
  sizes = c(5, 15),
  text = ~HoverText,
  hoverinfo = "text",
  type = "scatter3d",
  mode = "markers",
  marker = list(symbol = "circle", opacity = 0.8)
) %>%
  layout(
    title = "3D Credit Control Dashboard – Anomaly Detection",
    scene = list(
      xaxis = list(title = "Days Since First Transaction"),
      yaxis = list(title = "Charge Amount (ZMW)<br>(positive = invoice, negative = payment)"),
      zaxis = list(title = "Account Balance (ZMW)"),
      camera = list(eye = list(x = 1.5, y = 1.5, z = 1.5))
    ),
    legend = list(title = list(text = "<b>Anomaly Type</b>"), x = 0.9, y = 0.9)
  )

# Display the plot
fig

# 7. Save the interactive plot as an HTML file (shareable)
saveWidget(fig, "credit_control_3d.html", selfcontained = TRUE)
cat("\n3D plot saved as 'credit_control_3d.html' – open in any browser.\n")

# 8. Also produce a CSV report of anomalies (as before)
anomaly_report <- clean_data %>%
  filter(Anomaly != "Normal") %>%
  select(Date, Description, Invoice_No, Charge_Amount, Account_Balance, Anomaly, Days_Since_Start)

write_csv(anomaly_report, "anomaly_report_3d.csv")
cat("Anomaly report saved: anomaly_report_3d.csv\n")

# 9. Summary
cat("\n========== SUMMARY ==========\n")
cat("Total transactions:", nrow(clean_data), "\n")
cat("Normal transactions:", sum(clean_data$Anomaly == "Normal"), "\n")
cat("Anomalies detected:", sum(clean_data$Anomaly != "Normal"), "\n")
cat("Current balance:", tail(clean_data$Account_Balance, 1), "ZMW\n")
cat("============================\n")
