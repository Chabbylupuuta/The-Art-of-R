# ============================================================================
# TUTORIAL 14: ACCESSING THE INTERNET IN R
# ============================================================================
# Learn how to fetch data from the web, make HTTP requests, parse JSON/XML,
# and interact with APIs in R

# Install required packages (run once):
# install.packages("httr")
# install.packages("jsonlite")
# install.packages("xml2")
# install.packages("rvest")

library(httr)
library(jsonlite)
library(xml2)
library(rvest)

# ============================================================================
# 1. BASIC WEB REQUESTS
# ============================================================================

cat("\n========== 1. BASIC WEB REQUESTS ==========\n")

# Simple GET request
response <- GET("https://httpbin.org/get")
print(response)
print(status_code(response))  # Check HTTP status (200 = success)

# ============================================================================
# 2. FETCHING DATA FROM URLs
# ============================================================================

cat("\n========== 2. FETCHING DATA FROM URLs ==========\n")

# Read HTML content from a website
url <- "https://httpbin.org/html"
response <- GET(url)
content <- content(response, as = "text")
cat("First 500 characters of HTML:\n")
cat(substr(content, 1, 500))

# ============================================================================
# 3. WORKING WITH JSON DATA
# ============================================================================

cat("\n\n========== 3. WORKING WITH JSON DATA ==========\n")

# Fetch JSON data from API
json_url <- "https://jsonplaceholder.typicode.com/users/1"
response <- GET(json_url)
json_data <- content(response, as = "text")

# Parse JSON to R object
user_data <- fromJSON(json_data)
print(user_data)

# Access specific fields
cat("\nUser Name:", user_data$name, "\n")
cat("User Email:", user_data$email, "\n")
cat("User City:", user_data$address$city, "\n")

# ============================================================================
# 4. FETCH MULTIPLE JSON RECORDS
# ============================================================================

cat("\n========== 4. FETCH MULTIPLE JSON RECORDS ==========\n")

# Get first 5 posts
posts_url <- "https://jsonplaceholder.typicode.com/posts?_limit=5"
response <- GET(posts_url)
posts_data <- fromJSON(content(response, as = "text"))

# Convert to data frame
posts_df <- as.data.frame(do.call(rbind, posts_data))
print(head(posts_df))

cat("\nTotal posts fetched:", nrow(posts_df), "\n")

# ============================================================================
# 5. HTTP METHODS (GET, POST, PUT, DELETE)
# ============================================================================

cat("\n========== 5. HTTP METHODS ==========\n")

# GET request (retrieve data)
get_response <- GET("https://httpbin.org/get?param1=value1&param2=value2")
cat("GET Status:", status_code(get_response), "\n")

# POST request (send data)
post_response <- POST("https://httpbin.org/post",
                      body = list(name = "John", age = 30),
                      encode = "json")
cat("POST Status:", status_code(post_response), "\n")
post_content <- fromJSON(content(post_response, as = "text"))
print(post_content$json)

# PUT request (update data)
put_response <- PUT("https://httpbin.org/put",
                    body = list(id = 1, name = "Updated Name"),
                    encode = "json")
cat("PUT Status:", status_code(put_response), "\n")

# DELETE request (remove data)
delete_response <- DELETE("https://httpbin.org/delete")
cat("DELETE Status:", status_code(delete_response), "\n")

# ============================================================================
# 6. WORKING WITH HEADERS AND PARAMETERS
# ============================================================================

cat("\n========== 6. HEADERS AND PARAMETERS ==========\n")

# Add custom headers
response <- GET("https://httpbin.org/headers",
                add_headers("User-Agent" = "My Custom R Client",
                           "Accept" = "application/json"))
content <- fromJSON(content(response, as = "text"))
print(content$headers)

# Query parameters
params <- list(userId = 1, id = 1)
response <- GET("https://jsonplaceholder.typicode.com/comments",
                query = params)
comments <- fromJSON(content(response, as = "text"))
print(comments)

# ============================================================================
# 7. WEB SCRAPING WITH RVEST
# ============================================================================

cat("\n========== 7. WEB SCRAPING WITH RVEST ==========\n")

# Scrape a webpage
page <- read_html("https://en.wikipedia.org/wiki/Zambia")

# Extract title
title <- page %>%
  html_nodes("h1") %>%
  html_text()
cat("Page Title:", title, "\n")

# Extract all paragraphs
paragraphs <- page %>%
  html_nodes("p") %>%
  html_text()
cat("\nFirst paragraph:\n", substr(paragraphs[1], 1, 200), "...\n")

# ============================================================================
# 8. ERROR HANDLING IN WEB REQUESTS
# ============================================================================

cat("\n========== 8. ERROR HANDLING ==========\n")

# Handle errors gracefully
tryCatch({
  response <- GET("https://invalid-url-that-does-not-exist.com",
                  timeout(seconds = 5))
  if (status_code(response) == 200) {
    cat("Request successful!\n")
  } else {
    cat("Request failed with status:", status_code(response), "\n")
  }
}, error = function(e) {
  cat("Error occurred:", e$message, "\n")
})

# ============================================================================
# 9. DOWNLOADING FILES FROM THE WEB
# ============================================================================

cat("\n========== 9. DOWNLOADING FILES ==========\n")

# Download a file
download.file("https://httpbin.org/image/png",
              destfile = "downloaded_image.png",
              mode = "wb")  # wb = write binary
cat("File downloaded as 'downloaded_image.png'\n")

# ============================================================================
# 10. WORKING WITH CSV FILES FROM URLs
# ============================================================================

cat("\n========== 10. CSV FILES FROM URLs ==========\n")

# Read CSV directly from URL
csv_url <- "https://raw.githubusercontent.com/mwaskom/seaborn-data/master/iris.csv"
iris_data <- read.csv(csv_url)

cat("Dataset shape:", nrow(iris_data), "rows x", ncol(iris_data), "columns\n")
print(head(iris_data, 3))

# ============================================================================
# 11. REAL-WORLD EXAMPLE: WEATHER API
# ============================================================================

cat("\n========== 11. REAL-WORLD EXAMPLE: WEATHER API ==========\n")

# Note: This uses Open-Meteo (free, no API key required)
get_weather <- function(latitude, longitude) {
  url <- paste0(
    "https://api.open-meteo.com/v1/forecast?latitude=",
    latitude, "&longitude=", longitude,
    "&current=temperature_2m,relative_humidity_2m,weather_code"
  )
  
  response <- GET(url)
  weather_data <- fromJSON(content(response, as = "text"))
  
  return(weather_data)
}

# Get weather for Lusaka, Zambia (lat: -15.4167, lon: 28.2833)
cat("\nFetching weather for Lusaka, Zambia...\n")
weather <- get_weather(-15.4167, 28.2833)
cat("Temperature:", weather$current$temperature_2m, "°C\n")
cat("Humidity:", weather$current$relative_humidity_2m, "%\n")

# ============================================================================
# 12. REAL-WORLD EXAMPLE: BANK EXCHANGE RATES API
# ============================================================================

cat("\n========== 12. EXCHANGE RATES API ==========\n")

get_exchange_rates <- function(base_currency = "USD") {
  url <- paste0("https://api.exchangerate-api.com/v4/latest/", base_currency)
  
  tryCatch({
    response <- GET(url)
    
    if (status_code(response) == 200) {
      data <- fromJSON(content(response, as = "text"))
      rates <- data$rates
      return(rates)
    } else {
      cat("Error: Could not fetch exchange rates\n")
      return(NULL)
    }
  }, error = function(e) {
    cat("Error occurred:", e$message, "\n")
    return(NULL)
  })
}

# Get USD to ZMW exchange rate
cat("\nFetching USD Exchange Rates...\n")
rates <- get_exchange_rates("USD")
if (!is.null(rates)) {
  cat("1 USD = ", rates$ZMW, "ZMW\n")
  cat("1 USD = ", rates$GBP, "GBP\n")
  cat("1 USD = ", rates$EUR, "EUR\n")
}

# ============================================================================
# 13. API WITH AUTHENTICATION (Bearer Token)
# ============================================================================

cat("\n========== 13. API WITH AUTHENTICATION ==========\n")

# Example function for API with Bearer token
fetch_protected_data <- function(api_token) {
  url <- "https://api.example.com/protected-endpoint"
  
  response <- GET(url,
                  add_headers("Authorization" = paste("Bearer", api_token)))
  
  if (status_code(response) == 200) {
    data <- fromJSON(content(response, as = "text"))
    return(data)
  } else if (status_code(response) == 401) {
    cat("Error: Unauthorized (invalid token)\n")
    return(NULL)
  } else {
    cat("Error: Request failed\n")
    return(NULL)
  }
}

cat("Authentication example shown (requires valid API token)\n")

# ============================================================================
# 14. CREATING A SIMPLE WEB CLIENT FUNCTION
# ============================================================================

cat("\n========== 14. SIMPLE WEB CLIENT ==========\n")

web_client <- function(url, method = "GET", data = NULL, headers = NULL) {
  cat("[REQUEST]", method, url, "\n")
  
  response <- switch(method,
    "GET" = GET(url, add_headers(headers)),
    "POST" = POST(url, body = data, encode = "json", add_headers(headers)),
    "PUT" = PUT(url, body = data, encode = "json", add_headers(headers)),
    "DELETE" = DELETE(url, add_headers(headers)),
    stop("Unsupported method")
  )
  
  cat("[RESPONSE] Status:", status_code(response), "\n")
  
  content_type <- headers(response)$`content-type`
  
  if (grepl("json", content_type)) {
    return(fromJSON(content(response, as = "text")))
  } else {
    return(content(response, as = "text"))
  }
}

# Test the web client
result <- web_client("https://httpbin.org/get?test=true")
print(result)

# ============================================================================
# 15. BEST PRACTICES
# ============================================================================

cat("\n========== 15. BEST PRACTICES ==========\n")

best_practices <- c(
  "1. Always check status_code(response) before processing",
  "2. Use tryCatch() to handle network errors",
  "3. Set timeouts for slow connections: timeout(seconds = 10)",
  "4. Never hardcode API keys in code - use environment variables",
  "5. Respect API rate limits - don't make too many requests",
  "6. Cache results when possible to reduce API calls",
  "7. Use add_headers() for proper User-Agent identification",
  "8. Parse JSON with fromJSON() and XML with read_xml()",
  "9. Test with real data before production use",
  "10. Document API endpoints and authentication requirements"
)

cat(paste(best_practices, collapse = "\n"), "\n")

# ============================================================================
# SUMMARY
# ============================================================================

cat("\n========== SUMMARY ==========\n")
cat("
Key Functions Learned:
- GET(), POST(), PUT(), DELETE() - Make HTTP requests
- fromJSON() - Parse JSON responses
- read_html() - Web scraping with rvest
- download.file() - Download files from web
- read.csv() - Read CSV files from URLs
- add_headers() - Add custom headers
- status_code() - Check response status
- tryCatch() - Handle errors gracefully

Common APIs to Explore:
- OpenWeather (weather data)
- ExchangeRate-API (currency rates)
- JSONPlaceholder (fake data for testing)
- GitHub API (repository information)
- NewsAPI (news articles)
- CoinGecko (cryptocurrency data)
")

cat("\n✓ Tutorial 14 Complete! You can now access the internet in R!\n")
