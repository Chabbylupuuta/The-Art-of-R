
# 🌐 Tutorial 14: Accessing the Internet with R 🚀

Welcome to the **Internet Connectivity** module! This repository documents my journey in transforming R from a local data processor into a **global, web-connected financial engine**. 🛰️

## 🎯 The Vision: Why Connect R to the Web?
In my role as a **Credit Control Officer** 🏦, I’ve realized that data is only as good as its freshness. By connecting R to the internet, I bridge the gap between static spreadsheets and real-time financial intelligence.

* **Live Currency Intelligence:** Automatically pulling **ZMW/USD** and **ZMW/GBP** rates to manage international accounts with precision. 💸
* **Zero-Click Downloads:** Automating the retrieval of bank statements and invoice reports from cloud portals. No more manual downloading! ☁️
* **Central Bank Integration:** Preparing the logic to interface with **Bank of Zambia** or **Zamtel** data servers via secure APIs. 📡

---

## 🛠️ The Professional Toolkit
To communicate with the world, I utilize the industry-standard "Power Duo":

1.  **`httr`** 🔗: The **Courier**. It manages the heavy lifting of sending `GET` and `POST` requests, handling headers, and maintaining secure connections.
2.  **`jsonlite`** 📦: The **Translator**. Most of the internet speaks in **JSON**. This package ensures that messy web text is perfectly parsed into clean R data frames.

---

## 💻 Technical Concepts & Milestones

### 1. The HTTP "Handshake" (GET Requests) 📥
Understanding the protocol of the web. I learned how to request data from an endpoint and verify the status of the connection.
```r
# Example: Fetching the latest rates
library(httr)
response <- GET("[https://api.exchangerate-api.com/v4/latest/USD](https://api.exchangerate-api.com/v4/latest/USD)")

# Check if the connection is healthy (Status 200)
if(status_code(response) == 200) {
  cat("Connection Secure. Data Incoming! ✅")
}
