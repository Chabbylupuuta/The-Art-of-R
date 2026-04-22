# Internet data menu — improved: robust input, headers, currency conversion, better fallbacks
# Requires: httr, jsonlite, rvest
# install.packages(c("httr","jsonlite","rvest")) if needed

library(httr)
library(jsonlite)
library(rvest)

# NOTE: Many sites block automated clients. We set a browser-like User-Agent and Accept headers.
UA <- "Mozilla/5.0 (X11; Linux x86_64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/115.0 Safari/537.36"
COMMON_HEADERS <- add_headers(
  `User-Agent` = UA,
  `Accept` = "text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8",
  `Accept-Language` = "en-US,en;q=0.9"
)

# Robust stdin wrappers
get_user_input <- function(prompt = "") {
  if (nzchar(prompt)) cat(prompt)
  input <- readLines(con = file("stdin"), n = 1)
  if (length(input) == 0) return("") else return(input)
}
get_integer_input <- function(prompt = "") {
  repeat {
    ans <- get_user_input(prompt)
    if (!nzchar(ans)) return(NA_integer_)
    n <- suppressWarnings(as.integer(ans))
    if (!is.na(n)) return(n)
    cat("Please enter a valid integer.\n")
  }
}
pause <- function() { cat("\nPress Enter to return to menu..."); invisible(readLines(con = file("stdin"), n = 1)) }

# --- New helper: retrying GET using COMMON_HEADERS ---
retry_get <- function(url, headers = COMMON_HEADERS, timeout_sec = 10, attempts = 3, pause_sec = 1) {
  last_err <- NULL
  for (i in seq_len(attempts)) {
    res <- tryCatch({
      GET(url, headers, timeout(timeout_sec))
    }, error = function(e) {
      last_err <<- e
      NULL
    })
    if (!is.null(res)) {
      ok <- tryCatch({
        stop_for_status(res)
        TRUE
      }, error = function(e) {
        last_err <<- e
        FALSE
      })
      if (ok) return(res)
    }
    Sys.sleep(pause_sec)
  }
  stop(sprintf("All retries failed for %s : %s", url, if (!is.null(last_err)) conditionMessage(last_err) else "unknown error"))
}

# 1) Exchange rates (latest) using exchangerate.host (free, no key)
get_currency <- function(base = "USD") {
  url <- paste0("https://api.exchangerate.host/latest?base=", toupper(base))
  tryCatch({
    res <- retry_get(url)
    data <- fromJSON(content(res, "text", encoding = "UTF-8"))
    base_out <- if (!is.null(data$base)) data$base else toupper(base)
    date_out <- if (!is.null(data$date)) data$date else as.character(Sys.Date())
    rates <- data$rates
    cat("\n--- Exchange rates (base:", base_out, "| date:", date_out, ") ---\n")
    if (is.null(rates) || length(rates) == 0) {
      cat("[INFO] No rates returned.\n")
      return(NULL)
    }
    print(head(rates, 30))
    invisible(rates)
  }, error = function(e) {
    cat("[ERROR] Currency request failed:", e$message, "\n")
    NULL
  })
}

# 1b) Direct conversion (single amount) using exchangerate.host/convert
convert_currency <- function(amount = 1, from = "USD", to = "ZMW") {
  url <- paste0("https://api.exchangerate.host/convert?from=", toupper(from),
                "&to=", toupper(to), "&amount=", as.numeric(amount))
  tryCatch({
    res <- retry_get(url)
    data <- fromJSON(content(res, "text", encoding = "UTF-8"))
    q <- data$query
    info <- data$info
    result <- data$result
    if (is.null(q) || is.null(info) || is.null(result)) {
      cat("[ERROR] Unexpected response structure from conversion API.\n")
      return(NULL)
    }
    cat(sprintf("\n%s %s → %s %s (rate: %f)\n",
                q$amount, toupper(q$from),
                format(round(result, 4), nsmall = 4), toupper(q$to),
                info$rate))
    invisible(data)
  }, error = function(e) {
    cat("[ERROR] Currency conversion failed:", e$message, "\n")
    NULL
  })
}

# 2) Stocks - primary: Yahoo public quote endpoint with headers; fallback: message or API key (AlphaVantage)
get_stock_market <- function(symbols = "AAPL") {
  url <- paste0("https://query1.finance.yahoo.com/v7/finance/quote?symbols=", URLencode(symbols))
  tryCatch({
    res <- retry_get(url)
    data <- fromJSON(content(res, "text", encoding = "UTF-8"))
    quotes <- data$quoteResponse$result
    if (length(quotes) == 0) { cat("[INFO] No quotes returned for", symbols, "\n"); return(NULL) }
    df <- data.frame(
      symbol = sapply(quotes, `[[`, "symbol"),
      price = sapply(quotes, function(x) x$regularMarketPrice),
      change = sapply(quotes, function(x) x$regularMarketChange),
      pctChange = sapply(quotes, function(x) x$regularMarketChangePercent),
      stringsAsFactors = FALSE
    )
    cat("\n--- Stock quotes ---\n"); print(df); invisible(df)
  }, error = function(e) {
    cat("[ERROR] Stock request failed:", e$message, "\n")
    cat("Possible reasons: site blocks bots or network issue.\n")
    cat("If you have an API key for AlphaVantage set ALPHAVANTAGE_KEY env var and add a fallback.\n")
    NULL
  })
}

# 3) Minerals / precious metals - try goldprice JSON + metals.live fallback
get_minerals <- function() {
  url1 <- "https://data-asg.goldprice.org/dbXRates/USD"
  url2 <- "https://api.metals.live/v1/spot"
  tryCatch({
    res <- retry_get(url1)
    data <- fromJSON(content(res, "text", encoding = "UTF-8"))
    items <- data$items
    cat("\n--- Mineral / metal prices (source: goldprice) ---\n"); print(items); invisible(items)
  }, error = function(e1) {
    tryCatch({
      res2 <- retry_get(url2)
      data2 <- fromJSON(content(res2, "text", encoding = "UTF-8"))
      cat("\n--- Mineral / metal prices (source: metals.live) ---\n"); print(data2); invisible(data2)
    }, error = function(e2) {
      cat("[ERROR] Minerals request failed (both endpoints):\n - goldprice:", e1$message, "\n - metals.live:", e2$message, "\n")
      NULL
    })
  })
}

# 4) Fruit prices - IndexMundi table scraping (use headers)
get_fruit_prices <- function(commodity = "bananas") {
  url <- paste0("https://www.indexmundi.com/commodities/?commodity=", URLencode(commodity))
  tryCatch({
    res <- retry_get(url, timeout_sec = 12)
    page <- read_html(content(res, as = "text", encoding = "UTF-8"))
    tables <- html_table(page, fill = TRUE)
    if (length(tables) == 0) { cat("[INFO] No tables found on", url, "\n"); return(NULL) }
    cat("\n--- IndexMundi:", commodity, " ---\nSource:", url, "\n")
    print(tables[[1]])
    invisible(tables[[1]])
  }, error = function(e) {
    cat("[ERROR] Fruit scrape failed:", e$message, "\n")
    NULL
  })
}

# 5) Land/property prices - GlobalPropertyGuide scraping with headers
get_land_prices <- function(country = "Zambia") {
  slug <- gsub(" ", "-", country)
  url <- paste0("https://www.globalpropertyguide.com/Africa/", slug)
  tryCatch({
    res <- retry_get(url, timeout_sec = 12)
    page <- read_html(content(res, as = "text", encoding = "UTF-8"))
    tables <- html_table(page, fill = TRUE)
    if (length(tables) == 0) { cat("[INFO] No tables found on", url, "\n"); return(NULL) }
    cat("\n--- GlobalPropertyGuide:", country, " ---\nSource:", url, "\n")
    for (i in seq_len(min(2, length(tables)))) { cat("\n-- Table", i, "--\n"); print(tables[[i]]) }
    invisible(tables)
  }, error = function(e) {
    cat("[ERROR] Land scrape failed:", e$message, "\n")
    cat("Site may block scrapers or structure changed. Consider using an official API or manual lookup.\n")
    NULL
  })
}

# Main interactive menu (improved, professional)
internet_menu <- function() {
  repeat {
    cat("\n=====================================================\n")
    cat("       INTERNET DATA MENU — Professional Edition     \n")
    cat("=====================================================\n")
    cat(" 1) View exchange rates (list)\n")
    cat(" 2) Convert currency (amount)\n")
    cat(" 3) Stock quotes (Yahoo)\n")
    cat(" 4) Mineral / metal prices (gold/silver)\n")
    cat(" 5) Fruit commodity prices (IndexMundi)\n")
    cat(" 6) Land / property prices (GlobalPropertyGuide)\n")
    cat(" 7) Exit\n")
    cat("-----------------------------------------------------\n")
    ch <- get_integer_input("Select option [1-7]: ")
    if (is.na(ch)) { cat("No selection made. Try again.\n"); next }

    if (ch == 1) {
      b <- get_user_input("Base currency [USD]: "); if (!nzchar(b)) b <- "USD"
      get_currency(b); pause()

    } else if (ch == 2) {
      # --- Option 2: Convert currency (improved chooser) ---
      common_currencies <- c("USD","EUR","GBP","ZMW","KES","NGN","ZAR","JPY","CNY","AUD")
      cat("\nSelect source currency:\n")
      for (i in seq_along(common_currencies)) cat(i, "-", common_currencies[i], "\n")
      idx_from <- get_integer_input("Enter number for FROM currency (or 0 to type code): ")
      if (is.na(idx_from)) { cat("Cancelled.\n"); next }
      if (idx_from == 0) {
        from <- toupper(get_user_input("From (currency code): "))
      } else {
        if (idx_from >=1 && idx_from <= length(common_currencies)) from <- common_currencies[idx_from] else from <- "USD"
      }

      cat("\nSelect target currency:\n")
      for (i in seq_along(common_currencies)) cat(i, "-", common_currencies[i], "\n")
      idx_to <- get_integer_input("Enter number for TO currency (or 0 to type code): ")
      if (is.na(idx_to)) { cat("Cancelled.\n"); next }
      if (idx_to == 0) {
        to <- toupper(get_user_input("To (currency code): "))
      } else {
        if (idx_to >=1 && idx_to <= length(common_currencies)) to <- common_currencies[idx_to] else to <- "ZMW"
      }

      amt <- get_user_input("Amount (default 1): "); if (!nzchar(amt)) amt <- "1"
      convert_currency(as.numeric(amt), from, to); pause()

    } else if (ch == 3) {
      s <- get_user_input("Symbol(s) comma separated [AAPL]: "); if (!nzchar(s)) s <- "AAPL"
      get_stock_market(s); pause()

    } else if (ch == 4) {
      get_minerals(); pause()

    } else if (ch == 5) {
      cmm <- get_user_input("Commodity (e.g. bananas) [bananas]: "); if (!nzchar(cmm)) cmm <- "bananas"
      get_fruit_prices(cmm); pause()

    } else if (ch == 6) {
      cty <- get_user_input("Country (e.g. Zambia) [Zambia]: "); if (!nzchar(cty)) cty <- "Zambia"
      get_land_prices(cty); pause()

    } else if (ch == 7) {
      cat("\nExiting. Have a productive day.\n"); break

    } else {
      cat("Invalid choice. Try again.\n")
    }
  }
}

# Run if executed
internet_menu()
