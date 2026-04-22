# Tutorial15.1.r: Dispersion, Risk & Interactive Menu
# Requires: base R (no extra packages). Run in terminal: Rscript Tutorial15.1.r
# ------------------------------------------------------------------------------

# Starter sample data (editable during session)
balances <- c(1992, 5000, 12500, 1992, 8000, 4500, 1992)

# Robust stdin helpers (works with Rscript / terminal)
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

# Clear terminal (Linux)
clear_screen <- function() {
  try(system("clear"), silent = TRUE)
}

# Pause wrapper
pause <- function() {
  cat("\nPress Enter to return to menu...")
  invisible(readLines(con = file("stdin"), n = 1))
}

# Utility: compute mode (works for numeric or character)
compute_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Descriptive + dispersion summary
show_descriptive_stats <- function(x) {
  cat("\n--- Descriptive Statistics ---\n")
  cat("Count:", length(x), "\n")
  cat("Mean:", round(mean(x), 2), "\n")
  cat("Median:", round(median(x), 2), "\n")
  cat("Mode:", compute_mode(x), "\n")
  cat("Min:", min(x), "  Max:", max(x), "\n")
  cat("Std. Dev.:", round(sd(x), 2), "  Variance:", round(var(x), 2), "\n")
  cat("1st Quartile (Q1):", quantile(x, 0.25), "  3rd Quartile (Q3):", quantile(x, 0.75), "\n")
  cat("\nSummary:\n"); print(summary(x))
}

# Dispersion-focused output and interpretation hint
show_dispersion_risk <- function(x) {
  m <- mean(x)
  s <- sd(x)
  cat("\n--- Dispersion & Risk ---\n")
  cat("Mean:", round(m,2), "  Std.Dev:", round(s,2), "  CV (sd/mean):", ifelse(m==0, NA, round(s/m,4)), "\n")
  if (s/m > 0.5) {
    cat("✱ Interpretation: High relative dispersion (CV > 0.5). Collections are volatile.\n")
  } else if (s/m > 0.2) {
    cat("✱ Interpretation: Moderate dispersion. Monitor accounts with extreme values.\n")
  } else {
    cat("✱ Interpretation: Low dispersion. Collections relatively stable.\n")
  }
}

# Simple plots (save images if not interactive)
plot_histogram <- function(x, file = "balances_histogram.png") {
  if (interactive()) {
    hist(x, breaks = 10, col = "skyblue", main = "Histogram of Balances", xlab = "Balance")
  } else {
    png(file, width = 800, height = 600)
    hist(x, breaks = 10, col = "skyblue", main = "Histogram of Balances", xlab = "Balance")
    dev.off()
    cat("Saved histogram to", file, "\n")
  }
}
plot_boxplot <- function(x, file = "balances_boxplot.png") {
  if (interactive()) {
    boxplot(x, horizontal = TRUE, col = "lightgreen", main = "Boxplot of Balances")
  } else {
    png(file, width = 800, height = 200)
    boxplot(x, horizontal = TRUE, col = "lightgreen", main = "Boxplot of Balances")
    dev.off()
    cat("Saved boxplot to", file, "\n")
  }
}

# Risk simulation: probability of falling below threshold using empirical & normal approx
risk_estimate <- function(x) {
  thr <- get_numeric_input("Enter threshold balance (ZMW) to estimate probability of <= threshold: ")
  if (is.na(thr)) { cat("No threshold provided.\n"); return(NULL) }
  emp_prob <- mean(x <= thr)
  # normal approx
  m <- mean(x); s <- sd(x)
  approx_prob <- ifelse(s==0, ifelse(thr >= m, 1, 0), pnorm(thr, mean = m, sd = s))
  cat(sprintf("\nEmpirical P(balance <= %.2f) = %.4f\n", thr, emp_prob))
  cat(sprintf("Normal-approx P(balance <= %.2f) = %.4f (mean=%.2f sd=%.2f)\n", thr, approx_prob, m, s))
  # simple Monte Carlo simulation
  sims <- as.integer(get_numeric_input("Number of Monte Carlo simulations [default 10000]: "))
  if (is.na(sims) || sims <= 0) sims <- 10000
  set.seed(123)
  sim_vals <- rnorm(sims, mean = m, sd = s)
  sim_prob <- mean(sim_vals <= thr)
  cat(sprintf("Monte Carlo P(balance <= %.2f) ≈ %.4f (n=%d)\n", thr, sim_prob, sims))
  invisible(list(empirical = emp_prob, approx = approx_prob, sim = sim_prob))
}

# Manage balances: add / remove / reset
add_balance <- function(x) {
  v <- get_numeric_input("Enter new balance (ZMW) to add: ")
  if (is.na(v)) { cat("No value added.\n"); return(x) }
  x <- c(x, v)
  cat("Added:", v, "\n")
  return(x)
}
remove_balance <- function(x) {
  if (length(x) == 0) { cat("No balances to remove.\n"); return(x) }
  cat("Current balances (index:value):\n")
  print(setNames(round(x,2), seq_along(x)))
  idx <- as.integer(get_numeric_input("Enter index to remove (or 0 to cancel): "))
  if (is.na(idx) || idx == 0) { cat("Cancelled.\n"); return(x) }
  if (idx >= 1 && idx <= length(x)) {
    cat("Removing:", x[idx], "\n")
    x <- x[-idx]
  } else {
    cat("Index out of range.\n")
  }
  return(x)
}

# Interactive menu (clears screen before showing)
interactive_menu <- function() {
  vals <- balances
  repeat {
    clear_screen()
    cat("\n================= Tutorial 15.1 — Dispersion & Risk =================\n")
    cat("1) Show descriptive statistics\n")
    cat("2) Show dispersion / risk summary (CV & hints)\n")
    cat("3) Plot histogram (or save PNG)\n")
    cat("4) Plot boxplot (or save PNG)\n")
    cat("5) Estimate risk: probability balance <= threshold (empirical, normal, MC)\n")
    cat("6) Add a balance\n")
    cat("7) Remove a balance by index\n")
    cat("8) List balances\n")
    cat("9) Reset sample data\n")
    cat("0) Exit\n")
    cat("---------------------------------------------------------------------\n")
    choice <- get_user_input("Choose option [0-9]: ")
    if (!nzchar(choice)) { cat("No input. Try again.\n"); pause(); next }
    ch <- suppressWarnings(as.integer(choice))
    if (is.na(ch)) { cat("Please enter a number 0-9.\n"); pause(); next }

    if (ch == 1) {
      show_descriptive_stats(vals); pause()
    } else if (ch == 2) {
      show_dispersion_risk(vals); pause()
    } else if (ch == 3) {
      plot_histogram(vals); pause()
    } else if (ch == 4) {
      plot_boxplot(vals); pause()
    } else if (ch == 5) {
      risk_estimate(vals); pause()
    } else if (ch == 6) {
      vals <- add_balance(vals); pause()
    } else if (ch == 7) {
      vals <- remove_balance(vals); pause()
    } else if (ch == 8) {
      cat("\nBalances:\n"); print(vals); pause()
    } else if (ch == 9) {
      vals <- c(1992, 5000, 12500, 1992, 8000, 4500, 1992)
      cat("Sample data reset.\n"); pause()
    } else if (ch == 0) {
      cat("Exiting. Goodbye.\n"); break
    } else {
      cat("Invalid choice.\n"); pause()
    }
  }
}

# Run menu when script executed
interactive_menu()
```// filepath: /home/ching/Desktop/R/Basics/Statistics/Tutorial15.1.r
# Tutorial15.1.r: Dispersion, Risk & Interactive Menu
# Requires: base R (no extra packages). Run in terminal: Rscript Tutorial15.1.r
# ------------------------------------------------------------------------------

# Starter sample data (editable during session)
balances <- c(1992, 5000, 12500, 1992, 8000, 4500, 1992)

# Robust stdin helpers (works with Rscript / terminal)
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

# Clear terminal (Linux)
clear_screen <- function() {
  try(system("clear"), silent = TRUE)
}

# Pause wrapper
pause <- function() {
  cat("\nPress Enter to return to menu...")
  invisible(readLines(con = file("stdin"), n = 1))
}

# Utility: compute mode (works for numeric or character)
compute_mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}

# Descriptive + dispersion summary
show_descriptive_stats <- function(x) {
  cat("\n--- Descriptive Statistics ---\n")
  cat("Count:", length(x), "\n")
  cat("Mean:", round(mean(x), 2), "\n")
  cat("Median:", round(median(x), 2), "\n")
  cat("Mode:", compute_mode(x), "\n")
  cat("Min:", min(x), "  Max:", max(x), "\n")
  cat("Std. Dev.:", round(sd(x), 2), "  Variance:", round(var(x), 2), "\n")
  cat("1st Quartile (Q1):", quantile(x, 0.25), "  3rd Quartile (Q3):", quantile(x, 0.75), "\n")
  cat("\nSummary:\n"); print(summary(x))
}

# Dispersion-focused output and interpretation hint
show_dispersion_risk <- function(x) {
  m <- mean(x)
  s <- sd(x)
  cat("\n--- Dispersion & Risk ---\n")
  cat("Mean:", round(m,2), "  Std.Dev:", round(s,2), "  CV (sd/mean):", ifelse(m==0, NA, round(s/m,4)), "\n")
  if (s/m > 0.5) {
    cat("✱ Interpretation: High relative dispersion (CV > 0.5). Collections are volatile.\n")
  } else if (s/m > 0.2) {
    cat("✱ Interpretation: Moderate dispersion. Monitor accounts with extreme values.\n")
  } else {
    cat("✱ Interpretation: Low dispersion. Collections relatively stable.\n")
  }
}

# Simple plots (save images if not interactive)
plot_histogram <- function(x, file = "balances_histogram.png") {
  if (interactive()) {
    hist(x, breaks = 10, col = "skyblue", main = "Histogram of Balances", xlab = "Balance")
  } else {
    png(file, width = 800, height = 600)
    hist(x, breaks = 10, col = "skyblue", main = "Histogram of Balances", xlab = "Balance")
    dev.off()
    cat("Saved histogram to", file, "\n")
  }
}
plot_boxplot <- function(x, file = "balances_boxplot.png") {
  if (interactive()) {
    boxplot(x, horizontal = TRUE, col = "lightgreen", main = "Boxplot of Balances")
  } else {
    png(file, width = 800, height = 200)
    boxplot(x, horizontal = TRUE, col = "lightgreen", main = "Boxplot of Balances")
    dev.off()
    cat("Saved boxplot to", file, "\n")
  }
}

# Risk simulation: probability of falling below threshold using empirical & normal approx
risk_estimate <- function(x) {
  thr <- get_numeric_input("Enter threshold balance (ZMW) to estimate probability of <= threshold: ")
  if (is.na(thr)) { cat("No threshold provided.\n"); return(NULL) }
  emp_prob <- mean(x <= thr)
  # normal approx
  m <- mean(x); s <- sd(x)
  approx_prob <- ifelse(s==0, ifelse(thr >= m, 1, 0), pnorm(thr, mean = m, sd = s))
  cat(sprintf("\nEmpirical P(balance <= %.2f) = %.4f\n", thr, emp_prob))
  cat(sprintf("Normal-approx P(balance <= %.2f) = %.4f (mean=%.2f sd=%.2f)\n", thr, approx_prob, m, s))
  # simple Monte Carlo simulation
  sims <- as.integer(get_numeric_input("Number of Monte Carlo simulations [default 10000]: "))
  if (is.na(sims) || sims <= 0) sims <- 10000
  set.seed(123)
  sim_vals <- rnorm(sims, mean = m, sd = s)
  sim_prob <- mean(sim_vals <= thr)
  cat(sprintf("Monte Carlo P(balance <= %.2f) ≈ %.4f (n=%d)\n", thr, sim_prob, sims))
  invisible(list(empirical = emp_prob, approx = approx_prob, sim = sim_prob))
}

# Manage balances: add / remove / reset
add_balance <- function(x) {
  v <- get_numeric_input("Enter new balance (ZMW) to add: ")
  if (is.na(v)) { cat("No value added.\n"); return(x) }
  x <- c(x, v)
  cat("Added:", v, "\n")
  return(x)
}
remove_balance <- function(x) {
  if (length(x) == 0) { cat("No balances to remove.\n"); return(x) }
  cat("Current balances (index:value):\n")
  print(setNames(round(x,2), seq_along(x)))
  idx <- as.integer(get_numeric_input("Enter index to remove (or 0 to cancel): "))
  if (is.na(idx) || idx == 0) { cat("Cancelled.\n"); return(x) }
  if (idx >= 1 && idx <= length(x)) {
    cat("Removing:", x[idx], "\n")
    x <- x[-idx]
  } else {
    cat("Index out of range.\n")
  }
  return(x)
}

# Interactive menu (clears screen before showing)
interactive_menu <- function() {
  vals <- balances
  repeat {
    clear_screen()
    cat("\n================= Tutorial 15.1 — Dispersion & Risk =================\n")
    cat("1) Show descriptive statistics\n")
    cat("2) Show dispersion / risk summary (CV & hints)\n")
    cat("3) Plot histogram (or save PNG)\n")
    cat("4) Plot boxplot (or save PNG)\n")
    cat("5) Estimate risk: probability balance <= threshold (empirical, normal, MC)\n")
    cat("6) Add a balance\n")
    cat("7) Remove a balance by index\n")
    cat("8) List balances\n")
    cat("9) Reset sample data\n")
    cat("0) Exit\n")
    cat("---------------------------------------------------------------------\n")
    choice <- get_user_input("Choose option [0-9]: ")
    if (!nzchar(choice)) { cat("No input. Try again.\n"); pause(); next }
    ch <- suppressWarnings(as.integer(choice))
    if (is.na(ch)) { cat("Please enter a number 0-9.\n"); pause(); next }

    if (ch == 1) {
      show_descriptive_stats(vals); pause()
    } else if (ch == 2) {
      show_dispersion_risk(vals); pause()
    } else if (ch == 3) {
      plot_histogram(vals); pause()
    } else if (ch == 4) {
      plot_boxplot(vals); pause()
    } else if (ch == 5) {
      risk_estimate(vals); pause()
    } else if (ch == 6) {
      vals <- add_balance(vals); pause()
    } else if (ch == 7) {
      vals <- remove_balance(vals); pause()
    } else if (ch == 8) {
      cat("\nBalances:\n"); print(vals); pause()
    } else if (ch == 9) {
      vals <- c(1992, 5000, 12500, 1992, 8000, 4500, 1992)
      cat("Sample data reset.\n"); pause()
    } else if (ch == 0) {
      cat("Exiting. Goodbye.\n"); break
    } else {
      cat("Invalid choice.\n"); pause()
    }
  }
}

# Run menu when script executed
interactive_menu()
