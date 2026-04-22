# Tutorial15.2.r: Normal Distribution & Interactive Tools
# Base R only. Run in terminal: Rscript Tutorial15.2.r
# ------------------------------------------------------------------------------

# Sample data (editable during session)
balances <- c(1992, 5000, 12500, 1992, 8000, 4500, 1992)

# Robust stdin helpers (works with Rscript / terminal)
get_user_input <- function(prompt = "") {
  if (nzchar(prompt)) cat(prompt)
  ans <- readLines(con = file("stdin"), n = 1)
  if (length(ans) == 0) return("") else return(ans)
}
get_numeric_input <- function(prompt = "", default = NA_real_) {
  repeat {
    ans <- get_user_input(prompt)
    if (!nzchar(ans)) {
      if (!is.na(default)) return(default) else return(NA_real_)
    }
    val <- suppressWarnings(as.numeric(ans))
    if (!is.na(val)) return(val)
    cat("Please enter a valid number.\n")
  }
}
get_integer_input <- function(prompt = "", default = NA_integer_) {
  repeat {
    ans <- get_user_input(prompt)
    if (!nzchar(ans)) {
      if (!is.na(default)) return(default) else return(NA_integer_)
    }
    val <- suppressWarnings(as.integer(ans))
    if (!is.na(val)) return(val)
    cat("Please enter a valid integer.\n")
  }
}

# Clear terminal (Linux)
clear_screen <- function() try(system("clear"), silent = TRUE)

# Pause wrapper
pause <- function() {
  cat("\nPress Enter to return to menu...")
  invisible(readLines(con = file("stdin"), n = 1))
}

# Descriptive helpers
show_descriptive <- function(x) {
  cat("\n--- Descriptive statistics ---\n")
  cat("N:", length(x), "\n")
  cat("Mean:", round(mean(x), 2), "\n")
  cat("SD:", round(sd(x), 2), "\n")
  cat("Median:", median(x), "\n")
  cat("Min:", min(x), " Max:", max(x), "\n")
  cat("\nSummary:\n"); print(summary(x))
}

# 1) Probability client owes more than threshold using Normal approx and empirical
probability_over_threshold <- function(x) {
  thr <- get_numeric_input("Enter threshold (ZMW) [default 10000]: ", default = 10000)
  if (is.na(thr)) { cat("Cancelled.\n"); return(NULL) }
  m <- mean(x); s <- sd(x)
  emp <- mean(x > thr)
  approx <- ifelse(s == 0, ifelse(thr < m, 1, 0), pnorm(thr, mean = m, sd = s, lower.tail = FALSE))
  cat(sprintf("\nThreshold: %.2f ZMW\n", thr))
  cat(sprintf("Empirical P(balance > %.2f) = %.4f\n", thr, emp))
  cat(sprintf("Normal-approx P(balance > %.2f) = %.4f (mean=%.2f sd=%.2f)\n", thr, approx, m, s))
  invisible(list(threshold = thr, empirical = emp, approx = approx))
}

# 2) Quantile: find balance that corresponds to top p%
quantile_for_top_percent <- function(x) {
  p <- get_numeric_input("Enter top percentile (e.g. 5 for top 5%) [default 5]: ", default = 5)
  if (is.na(p) || p <= 0 || p >= 100) { cat("Enter a percent between 0 and 100.\n"); return(NULL) }
  m <- mean(x); s <- sd(x)
  # top p% => quantile at 1 - p/100
  q_emp <- quantile(x, probs = 1 - p/100, names = FALSE)
  q_norm <- qnorm(1 - p/100, mean = m, sd = s)
  cat(sprintf("\nTop %.2f%% corresponds to empirical balance >= %.2f\n", p, q_emp))
  cat(sprintf("Normal-approx threshold for top %.2f%% = %.2f (mean=%.2f sd=%.2f)\n", p, q_norm, m, s))
  invisible(list(percent = p, empirical = q_emp, normal = q_norm))
}

# 3) Z-score for given balance
z_score_for_value <- function(x) {
  val <- get_numeric_input("Enter balance value to compute Z-score: ")
  if (is.na(val)) { cat("Cancelled.\n"); return(NULL) }
  m <- mean(x); s <- sd(x)
  zs <- ifelse(s == 0, NA, (val - m) / s)
  cat(sprintf("\nValue: %.2f  Mean: %.2f  SD: %.2f  Z-score: %.4f\n", val, m, s, zs))
  cat(sprintf("Probability (value <= %.2f) under Normal approx: %.4f\n", val,
              ifelse(s==0, NA, pnorm(val, mean = m, sd = s))))
  invisible(list(value = val, z = zs))
}

# 4) Monte Carlo: sampling distribution of mean
simulate_sample_means <- function(x) {
  n <- get_integer_input("Sample size per draw (n) [default 30]: ", default = 30)
  reps <- get_integer_input("Number of simulations (reps) [default 5000]: ", default = 5000)
  if (is.na(n) || n <= 0) n <- 30
  if (is.na(reps) || reps <= 0) reps <- 5000
  set.seed(123)
  sims <- replicate(reps, mean(sample(x, size = n, replace = TRUE)))
  cat(sprintf("\nSimulated sample means: mean=%.4f sd=%.4f (n=%d reps=%d)\n", mean(sims), sd(sims), n, reps))
  # Save histogram when not interactive
  if (interactive()) {
    hist(sims, main = sprintf("Sampling distribution of mean (n=%d)", n),
         xlab = "Sample mean", col = "lightblue", breaks = 40)
  } else {
    png("sample_means_hist.png", width = 800, height = 600)
    hist(sims, main = sprintf("Sampling distribution of mean (n=%d)", n),
         xlab = "Sample mean", col = "lightblue", breaks = 40)
    dev.off()
    cat("Saved sampling distribution plot to sample_means_hist.png\n")
  }
  invisible(sims)
}

# 5) Plot histogram with Normal overlay
plot_hist_with_normal <- function(x) {
  m <- mean(x); s <- sd(x)
  if (interactive()) {
    hist(x, prob = TRUE, breaks = 10, col = "wheat",
         main = "Histogram with Normal overlay", xlab = "Balance")
    curve(dnorm(x, mean = m, sd = s), add = TRUE, col = "blue", lwd = 2)
    legend("topright", legend = c("Normal approx"), col = c("blue"), lwd = 2)
  } else {
    png("balances_hist_normal.png", width = 800, height = 600)
    hist(x, prob = TRUE, breaks = 10, col = "wheat",
         main = "Histogram with Normal overlay", xlab = "Balance")
    curve(dnorm(x, mean = m, sd = s), add = TRUE, col = "blue", lwd = 2)
    dev.off()
    cat("Saved histogram with normal overlay to balances_hist_normal.png\n")
  }
}

# Interactive menu
interactive_menu <- function() {
  vals <- balances
  repeat {
    clear_screen()
    cat("\n================ Tutorial 15.2 — Normal Distribution Tools ================\n")
    cat("1) Show descriptive statistics\n")
    cat("2) Probability a client owes MORE than threshold (empirical & Normal approx)\n")
    cat("3) Find balance threshold for top p% (empirical & Normal)\n")
    cat("4) Compute Z-score for a balance value\n")
    cat("5) Simulate sampling distribution of the mean (Monte Carlo)\n")
    cat("6) Plot histogram with Normal overlay (save PNG if non-interactive)\n")
    cat("7) Add a balance\n")
    cat("8) Remove a balance by index\n")
    cat("9) List balances\n")
    cat("0) Exit\n")
    cat("--------------------------------------------------------------------------\n")
    choice <- get_user_input("Choose option [0-9]: ")
    if (!nzchar(choice)) { cat("No input. Try again.\n"); pause(); next }
    ch <- suppressWarnings(as.integer(choice))
    if (is.na(ch)) { cat("Please enter a number 0-9.\n"); pause(); next }

    if (ch == 1) {
      show_descriptive(vals); pause()
    } else if (ch == 2) {
      probability_over_threshold(vals); pause()
    } else if (ch == 3) {
      quantile_for_top_percent(vals); pause()
    } else if (ch == 4) {
      z_score_for_value(vals); pause()
    } else if (ch == 5) {
      simulate_sample_means(vals); pause()
    } else if (ch == 6) {
      plot_hist_with_normal(vals); pause()
    } else if (ch == 7) {
      v <- get_numeric_input("Enter new balance (ZMW) to add: ")
      if (!is.na(v)) { vals <- c(vals, v); cat("Added:", v, "\n") }
      pause()
    } else if (ch == 8) {
      cat("Balances (index:value):\n"); print(setNames(round(vals,2), seq_along(vals)))
      idx <- get_integer_input("Enter index to remove (0 to cancel): ", default = 0)
      if (!is.na(idx) && idx >= 1 && idx <= length(vals)) { cat("Removing:", vals[idx], "\n"); vals <- vals[-idx] }
      pause()
    } else if (ch == 9) {
      cat("\nBalances:\n"); print(vals); pause()
    } else if (ch == 0) {
      cat("Exiting. Goodbye.\n"); break
    } else {
      cat("Invalid choice.\n"); pause()
    }
  }
}

# Run menu when script executed
interactive_menu()
```# filepath: /home/ching/Desktop/R/Basics/Statistics/Tutorial15.2.r
# Tutorial15.2.r: Normal Distribution & Interactive Tools
# Base R only. Run in terminal: Rscript Tutorial15.2.r
# ------------------------------------------------------------------------------

# Sample data (editable during session)
balances <- c(1992, 5000, 12500, 1992, 8000, 4500, 1992)

# Robust stdin helpers (works with Rscript / terminal)
get_user_input <- function(prompt = "") {
  if (nzchar(prompt)) cat(prompt)
  ans <- readLines(con = file("stdin"), n = 1)
  if (length(ans) == 0) return("") else return(ans)
}
get_numeric_input <- function(prompt = "", default = NA_real_) {
  repeat {
    ans <- get_user_input(prompt)
    if (!nzchar(ans)) {
      if (!is.na(default)) return(default) else return(NA_real_)
    }
    val <- suppressWarnings(as.numeric(ans))
    if (!is.na(val)) return(val)
    cat("Please enter a valid number.\n")
  }
}
get_integer_input <- function(prompt = "", default = NA_integer_) {
  repeat {
    ans <- get_user_input(prompt)
    if (!nzchar(ans)) {
      if (!is.na(default)) return(default) else return(NA_integer_)
    }
    val <- suppressWarnings(as.integer(ans))
    if (!is.na(val)) return(val)
    cat("Please enter a valid integer.\n")
  }
}

# Clear terminal (Linux)
clear_screen <- function() try(system("clear"), silent = TRUE)

# Pause wrapper
pause <- function() {
  cat("\nPress Enter to return to menu...")
  invisible(readLines(con = file("stdin"), n = 1))
}

# Descriptive helpers
show_descriptive <- function(x) {
  cat("\n--- Descriptive statistics ---\n")
  cat("N:", length(x), "\n")
  cat("Mean:", round(mean(x), 2), "\n")
  cat("SD:", round(sd(x), 2), "\n")
  cat("Median:", median(x), "\n")
  cat("Min:", min(x), " Max:", max(x), "\n")
  cat("\nSummary:\n"); print(summary(x))
}

# 1) Probability client owes more than threshold using Normal approx and empirical
probability_over_threshold <- function(x) {
  thr <- get_numeric_input("Enter threshold (ZMW) [default 10000]: ", default = 10000)
  if (is.na(thr)) { cat("Cancelled.\n"); return(NULL) }
  m <- mean(x); s <- sd(x)
  emp <- mean(x > thr)
  approx <- ifelse(s == 0, ifelse(thr < m, 1, 0), pnorm(thr, mean = m, sd = s, lower.tail = FALSE))
  cat(sprintf("\nThreshold: %.2f ZMW\n", thr))
  cat(sprintf("Empirical P(balance > %.2f) = %.4f\n", thr, emp))
  cat(sprintf("Normal-approx P(balance > %.2f) = %.4f (mean=%.2f sd=%.2f)\n", thr, approx, m, s))
  invisible(list(threshold = thr, empirical = emp, approx = approx))
}

# 2) Quantile: find balance that corresponds to top p%
quantile_for_top_percent <- function(x) {
  p <- get_numeric_input("Enter top percentile (e.g. 5 for top 5%) [default 5]: ", default = 5)
  if (is.na(p) || p <= 0 || p >= 100) { cat("Enter a percent between 0 and 100.\n"); return(NULL) }
  m <- mean(x); s <- sd(x)
  # top p% => quantile at 1 - p/100
  q_emp <- quantile(x, probs = 1 - p/100, names = FALSE)
  q_norm <- qnorm(1 - p/100, mean = m, sd = s)
  cat(sprintf("\nTop %.2f%% corresponds to empirical balance >= %.2f\n", p, q_emp))
  cat(sprintf("Normal-approx threshold for top %.2f%% = %.2f (mean=%.2f sd=%.2f)\n", p, q_norm, m, s))
  invisible(list(percent = p, empirical = q_emp, normal = q_norm))
}

# 3) Z-score for given balance
z_score_for_value <- function(x) {
  val <- get_numeric_input("Enter balance value to compute Z-score: ")
  if (is.na(val)) { cat("Cancelled.\n"); return(NULL) }
  m <- mean(x); s <- sd(x)
  zs <- ifelse(s == 0, NA, (val - m) / s)
  cat(sprintf("\nValue: %.2f  Mean: %.2f  SD: %.2f  Z-score: %.4f\n", val, m, s, zs))
  cat(sprintf("Probability (value <= %.2f) under Normal approx: %.4f\n", val,
              ifelse(s==0, NA, pnorm(val, mean = m, sd = s))))
  invisible(list(value = val, z = zs))
}

# 4) Monte Carlo: sampling distribution of mean
simulate_sample_means <- function(x) {
  n <- get_integer_input("Sample size per draw (n) [default 30]: ", default = 30)
  reps <- get_integer_input("Number of simulations (reps) [default 5000]: ", default = 5000)
  if (is.na(n) || n <= 0) n <- 30
  if (is.na(reps) || reps <= 0) reps <- 5000
  set.seed(123)
  sims <- replicate(reps, mean(sample(x, size = n, replace = TRUE)))
  cat(sprintf("\nSimulated sample means: mean=%.4f sd=%.4f (n=%d reps=%d)\n", mean(sims), sd(sims), n, reps))
  # Save histogram when not interactive
  if (interactive()) {
    hist(sims, main = sprintf("Sampling distribution of mean (n=%d)", n),
         xlab = "Sample mean", col = "lightblue", breaks = 40)
  } else {
    png("sample_means_hist.png", width = 800, height = 600)
    hist(sims, main = sprintf("Sampling distribution of mean (n=%d)", n),
         xlab = "Sample mean", col = "lightblue", breaks = 40)
    dev.off()
    cat("Saved sampling distribution plot to sample_means_hist.png\n")
  }
  invisible(sims)
}

# 5) Plot histogram with Normal overlay
plot_hist_with_normal <- function(x) {
  m <- mean(x); s <- sd(x)
  if (interactive()) {
    hist(x, prob = TRUE, breaks = 10, col = "wheat",
         main = "Histogram with Normal overlay", xlab = "Balance")
    curve(dnorm(x, mean = m, sd = s), add = TRUE, col = "blue", lwd = 2)
    legend("topright", legend = c("Normal approx"), col = c("blue"), lwd = 2)
  } else {
    png("balances_hist_normal.png", width = 800, height = 600)
    hist(x, prob = TRUE, breaks = 10, col = "wheat",
         main = "Histogram with Normal overlay", xlab = "Balance")
    curve(dnorm(x, mean = m, sd = s), add = TRUE, col = "blue", lwd = 2)
    dev.off()
    cat("Saved histogram with normal overlay to balances_hist_normal.png\n")
  }
}

# Interactive menu
interactive_menu <- function() {
  vals <- balances
  repeat {
    clear_screen()
    cat("\n================ Tutorial 15.2 — Normal Distribution Tools ================\n")
    cat("1) Show descriptive statistics\n")
    cat("2) Probability a client owes MORE than threshold (empirical & Normal approx)\n")
    cat("3) Find balance threshold for top p% (empirical & Normal)\n")
    cat("4) Compute Z-score for a balance value\n")
    cat("5) Simulate sampling distribution of the mean (Monte Carlo)\n")
    cat("6) Plot histogram with Normal overlay (save PNG if non-interactive)\n")
    cat("7) Add a balance\n")
    cat("8) Remove a balance by index\n")
    cat("9) List balances\n")
    cat("0) Exit\n")
    cat("--------------------------------------------------------------------------\n")
    choice <- get_user_input("Choose option [0-9]: ")
    if (!nzchar(choice)) { cat("No input. Try again.\n"); pause(); next }
    ch <- suppressWarnings(as.integer(choice))
    if (is.na(ch)) { cat("Please enter a number 0-9.\n"); pause(); next }

    if (ch == 1) {
      show_descriptive(vals); pause()
    } else if (ch == 2) {
      probability_over_threshold(vals); pause()
    } else if (ch == 3) {
      quantile_for_top_percent(vals); pause()
    } else if (ch == 4) {
      z_score_for_value(vals); pause()
    } else if (ch == 5) {
      simulate_sample_means(vals); pause()
    } else if (ch == 6) {
      plot_hist_with_normal(vals); pause()
    } else if (ch == 7) {
      v <- get_numeric_input("Enter new balance (ZMW) to add: ")
      if (!is.na(v)) { vals <- c(vals, v); cat("Added:", v, "\n") }
      pause()
    } else if (ch == 8) {
      cat("Balances (index:value):\n"); print(setNames(round(vals,2), seq_along(vals)))
      idx <- get_integer_input("Enter index to remove (0 to cancel): ", default = 0)
      if (!is.na(idx) && idx >= 1 && idx <= length(vals)) { cat("Removing:", vals[idx], "\n"); vals <- vals[-idx] }
      pause()
    } else if (ch == 9) {
      cat("\nBalances:\n"); print(vals); pause()
    } else if (ch == 0) {
      cat("Exiting. Goodbye.\n"); break
    } else {
      cat("Invalid choice.\n"); pause()
    }
  }
}

# Run menu when script executed
interactive_menu()