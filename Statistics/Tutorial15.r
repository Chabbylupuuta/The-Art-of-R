
# Tutorial15.r: Central Tendency (interactive extension)

# Starter data
balances <- c(1992, 5000, 12500, 1992, 8000, 4500, 1992)

# Robust stdin helpers
get_user_input <- function(prompt = "") {
  if (nzchar(prompt)) cat(prompt)
  ans <- readLines(con = file("stdin"), n = 1)
  if (length(ans) == 0) return("") else return(ans)
}
get_numeric_input <- function(prompt = "") {
  repeat {
    ans <- get_user_input(prompt)
    if (!nzchar(ans)) return(NA_real_)
    val <- suppressWarnings(as.numeric(ans))
    if (!is.na(val)) return(val)
    cat("Please enter a valid number.\n")
  }
}
pause <- function() { cat("\nPress Enter to return to menu..."); invisible(readLines(con = file("stdin"), n = 1)) }

# Utility: mode
compute_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Display descriptive statistics
show_descriptive_stats <- function(x) {
  cat("\n--- Descriptive statistics ---\n")
  cat("Count:", length(x), "\n")
  cat("Mean:", mean(x), "\n")
  cat("Median:", median(x), "\n")
  cat("Mode:", compute_mode(x), "\n")
  cat("Min:", min(x), " Max:", max(x), "\n")
  cat("Std. Dev.:", sd(x), " Variance:", var(x), "\n")
  cat("1st Quartile (Q1):", quantile(x, 0.25), " 3rd Quartile (Q3):", quantile(x, 0.75), "\n")
  cat("\nSummary:\n"); print(summary(x))
}

# Show frequency table
show_frequency_table <- function(x) {
  cat("\n--- Frequency table ---\n")
  print(sort(table(x), decreasing = TRUE))
}

# Plot helpers (base R)
plot_histogram <- function(x) {
  if (interactive()) {
    hist(x, breaks = 10, main = "Histogram of Balances", xlab = "Balance (ZMW)", col = "skyblue")
  } else {
    png("balances_histogram.png", width = 800, height = 600)
    hist(x, breaks = 10, main = "Histogram of Balances", xlab = "Balance (ZMW)", col = "skyblue")
    dev.off()
    cat("Saved histogram to balances_histogram.png\n")
  }
}
plot_boxplot <- function(x) {
  if (interactive()) {
    boxplot(x, main = "Boxplot of Balances", horizontal = TRUE, col = "lightgreen")
  } else {
    png("balances_boxplot.png", width = 800, height = 200)
    boxplot(x, main = "Boxplot of Balances", horizontal = TRUE, col = "lightgreen")
    dev.off()
    cat("Saved boxplot to balances_boxplot.png\n")
  }
}

# Add / remove values
add_balance <- function(x) {
  v <- get_numeric_input("Enter new balance (ZMW): ")
  if (is.na(v)) { cat("No value added.\n"); return(x) }
  x <- c(x, v)
  cat("Added:", v, "\n")
  return(x)
}
remove_balance <- function(x) {
  if (length(x) == 0) { cat("No balances to remove.\n"); return(x) }
  cat("Current balances (index:value):\n")
  print(setNames(x, seq_along(x)))
  idx <- get_numeric_input("Enter index to remove (or 0 to cancel): ")
  if (is.na(idx) || idx == 0) { cat("Cancelled.\n"); return(x) }
  if (idx >= 1 && idx <= length(x)) {
    cat("Removing:", x[idx], "\n")
    x <- x[-idx]
  } else {
    cat("Index out of range.\n")
  }
  return(x)
}

# Main interactive menu
interactive_menu <- function() {
  vals <- balances
  repeat {
    cat("\n================== Central Tendency & Graphics ==================\n")
    cat("1) Show descriptive statistics\n")
    cat("2) Show frequency (mode) table\n")
    cat("3) Add a balance\n")
    cat("4) Remove a balance\n")
    cat("5) List all balances\n")
    cat("6) Plot histogram (and save when non-interactive)\n")
    cat("7) Plot boxplot (and save when non-interactive)\n")
    cat("8) Reset to default sample data\n")
    cat("9) Exit\n")
    ch <- get_user_input("Choose an option [1-9]: ")
    if (!nzchar(ch)) { cat("No choice entered.\n"); next }
    chn <- suppressWarnings(as.integer(ch))
    if (is.na(chn)) { cat("Please enter a number 1-9.\n"); next }
    if (chn == 1) {
      show_descriptive_stats(vals); pause()
    } else if (chn == 2) {
      show_frequency_table(vals); pause()
    } else if (chn == 3) {
      vals <- add_balance(vals); pause()
    } else if (chn == 4) {
      vals <- remove_balance(vals); pause()
    } else if (chn == 5) {
      cat("\nBalances:\n"); print(vals); pause()
    } else if (chn == 6) {
      plot_histogram(vals); pause()
    } else if (chn == 7) {
      plot_boxplot(vals); pause()
    } else if (chn == 8) {
      vals <- c(1992, 5000, 12500, 1992, 8000, 4500, 1992)
      cat("Reset sample data.\n"); pause()
    } else if (chn == 9) {
      cat("Exiting tutorial. Goodbye.\n"); break
    } else {
      cat("Invalid selection.\n")
    }
  }
}

# If run interactively or sourced in terminal, start menu
interactive_menu()

