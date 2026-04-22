# Tutorial15.5.r: Interactive T-Test (Comparing Groups)
# Base R only. Run in terminal: Rscript Tutorial15.5.r
# ------------------------------------------------------------------------------

# Starter samples (editable)
debt_before_policy <- c(2000, 2500, 3000, 1900)
debt_after_policy  <- c(1500, 1800, 1200, 1600)

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
    vals <- strsplit(ans, "[,; ]+")[[1]]
    nums <- suppressWarnings(as.numeric(vals))
    if (all(!is.na(nums))) return(nums)
    cat("Please enter comma/space-separated numeric values.\n")
  }
}
get_single_numeric <- function(prompt = "", default = NA_real_) {
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

# UI helpers
clear_screen <- function() try(system("clear"), silent = TRUE)
pause <- function() { cat("\nPress Enter to return to menu..."); invisible(readLines(con = file("stdin"), n = 1)) }

# Statistics helpers
compute_cohens_d_independent <- function(x, y) {
  nx <- length(x); ny <- length(y)
  mx <- mean(x); my <- mean(y)
  sx2 <- var(x); sy2 <- var(y)
  pooled_sd <- sqrt(((nx - 1) * sx2 + (ny - 1) * sy2) / (nx + ny - 2))
  if (pooled_sd == 0) return(NA_real_)
  (mx - my) / pooled_sd
}
compute_cohens_d_paired <- function(x, y) {
  d <- x - y
  md <- mean(d); sd_d <- sd(d)
  if (sd_d == 0) return(NA_real_)
  md / sd_d
}

# Run a t-test and show interpretation
run_t_test <- function(x, y, paired = FALSE, var.equal = FALSE, alt = "two.sided") {
  if (paired && length(x) != length(y)) {
    cat("[ERROR] Paired test requires equal-length vectors.\n"); return(NULL)
  }
  if (length(x) < 2 || length(y) < 2) {
    cat("[ERROR] Need at least 2 observations per group.\n"); return(NULL)
  }
  res <- tryCatch({
    t.test(x, y, paired = paired, var.equal = var.equal, alternative = alt)
  }, error = function(e) {
    cat("[ERROR] t.test failed:", e$message, "\n"); return(NULL)
  })
  if (is.null(res)) return(NULL)

  cat("\n--- t-test results ---\n")
  print(res)
  # effect size
  d <- if (paired) compute_cohens_d_paired(x, y) else compute_cohens_d_independent(x, y)
  cat(sprintf("\nCohen's d (effect size) = %.4f\n", ifelse(is.na(d), NA_real_, d)))
  # Interpretation
  p <- res$p.value
  if (!is.na(p) && p < 0.05) {
    cat("Interpretation: Statistically significant (p < 0.05).\n")
  } else {
    cat("Interpretation: Not statistically significant at 5% level.\n")
  }
  # practical significance hint
  if (!is.na(d)) {
    mag <- if (abs(d) < 0.2) "negligible" else if (abs(d) < 0.5) "small" else if (abs(d) < 0.8) "medium" else "large"
    cat("Practical effect size:", mag, "\n")
  }
  invisible(res)
}

# Normality checks
run_shapiro <- function(x, label = "group") {
  if (length(x) < 3) {
    cat(sprintf("Shapiro-Wilk requires >=3 observations for %s. Skipping.\n", label)); return(NULL)
  }
  res <- tryCatch(shapiro.test(x), error = function(e) NULL)
  if (is.null(res)) {
    cat("Shapiro test failed for", label, "\n"); return(NULL)
  }
  cat(sprintf("\nShapiro-Wilk for %s: W=%.4f, p=%.4g\n", label, res$statistic, res$p.value))
  if (res$p.value < 0.05) cat(" -> Evidence against normality (p < 0.05)\n") else cat(" -> No strong evidence against normality\n")
  invisible(res)
}

# Simple boxplot (saves PNG when non-interactive)
plot_groups_boxplot <- function(x, y, file = "t_test_boxplot.png") {
  groups <- factor(c(rep("Before", length(x)), rep("After", length(y))))
  values <- c(x, y)
  if (interactive()) {
    boxplot(values ~ groups, col = c("lightblue", "lightgreen"), main = "Group comparison", ylab = "Value")
  } else {
    png(file, width = 800, height = 600)
    boxplot(values ~ groups, col = c("lightblue", "lightgreen"), main = "Group comparison", ylab = "Value")
    dev.off()
    cat("Saved boxplot to", file, "\n")
  }
}

# Menu actions
list_samples <- function(x, y) {
  cat("\n--- Sample A (Before) ---\n"); print(round(x, 2))
  cat("\n--- Sample B (After) ---\n"); print(round(y, 2))
}

set_samples_manual <- function() {
  cat("Enter values for BEFORE group (comma/space separated):\n")
  a <- get_numeric_input("> ")
  cat("Enter values for AFTER group (comma/space separated):\n")
  b <- get_numeric_input("> ")
  return(list(a = a, b = b))
}

generate_synthetic <- function() {
  n1 <- get_integer_input("n for BEFORE [default 20]: ", default = 20)
  n2 <- get_integer_input("n for AFTER [default 20]: ", default = 20)
  m1 <- get_single_numeric("mean BEFORE [default 2500]: ", default = 2500)
  m2 <- get_single_numeric("mean AFTER  [default 1800]: ", default = 1800)
  sd1 <- get_single_numeric("sd BEFORE [default 500]: ", default = 500)
  sd2 <- get_single_numeric("sd AFTER  [default 600]: ", default = 600)
  set.seed(123)
  a <- rnorm(n1, mean = m1, sd = sd1)
  b <- rnorm(n2, mean = m2, sd = sd2)
  cat("Synthetic samples generated.\n")
  return(list(a = a, b = b))
}

save_results <- function(res) {
  if (is.null(res)) { cat("No results to save.\n"); return(NULL) }
  fname <- get_user_input("Filename to save t.test result (RDS) [t_test_result.rds]: ")
  if (!nzchar(fname)) fname <- "t_test_result.rds"
  saveRDS(res, file = fname)
  cat("Saved result to", fname, "\n")
}

# Main interactive menu
interactive_menu <- function() {
  a <- debt_before_policy
  b <- debt_after_policy
  last_result <- NULL
  repeat {
    clear_screen()
    cat("\n================= Tutorial 15.5 — Interactive T-Test =================\n")
    cat("1) Show current samples\n")
    cat("2) Run two-sample t-test (independent)\n")
    cat("3) Run paired t-test\n")
    cat("4) Run Welch t-test (unequal variances)\n")
    cat("5) Normality checks (Shapiro-Wilk) for each group\n")
    cat("6) Compute effect size (Cohen's d)\n")
    cat("7) Boxplot groups (save PNG when non-interactive)\n")
    cat("8) Enter new samples manually\n")
    cat("9) Generate synthetic samples (rnorm)\n")
    cat("10) Save last t-test result to file\n")
    cat("0) Reset to starter samples\n")
    cat("q) Quit\n")
    cat("---------------------------------------------------------------------\n")
    choice <- get_user_input("Choose option: ")
    if (!nzchar(choice)) { cat("No input. Try again.\n"); pause(); next }
    if (tolower(choice) == "q") break
    ch <- suppressWarnings(as.integer(choice))
    if (is.na(ch)) { cat("Invalid selection.\n"); pause(); next }

    if (ch == 1) {
      list_samples(a, b); pause()

    } else if (ch == 2) {
      alt <- get_user_input("Alternative [two.sided / greater / less] [two.sided]: ")
      if (!nzchar(alt)) alt <- "two.sided"
      last_result <- run_t_test(a, b, paired = FALSE, var.equal = TRUE, alt = alt)
      pause()

    } else if (ch == 3) {
      last_result <- run_t_test(a, b, paired = TRUE, var.equal = FALSE, alt = "two.sided")
      pause()

    } else if (ch == 4) {
      alt <- get_user_input("Alternative [two.sided / greater / less] [two.sided]: ")
      if (!nzchar(alt)) alt <- "two.sided"
      last_result <- run_t_test(a, b, paired = FALSE, var.equal = FALSE, alt = alt)
      pause()

    } else if (ch == 5) {
      run_shapiro(a, "Before"); run_shapiro(b, "After"); pause()

    } else if (ch == 6) {
      d_ind <- compute_cohens_d_independent(a, b)
      d_paired <- if (length(a) == length(b)) compute_cohens_d_paired(a, b) else NA_real_
      cat(sprintf("\nCohen's d (independent) = %.4f\n", ifelse(is.na(d_ind), NA_real_, d_ind)))
      if (!is.na(d_paired)) cat(sprintf("Cohen's d (paired) = %.4f\n", d_paired))
      pause()

    } else if (ch == 7) {
      plot_groups_boxplot(a, b); pause()

    } else if (ch == 8) {
      res <- set_samples_manual()
      a <- res$a; b <- res$b
      cat("Samples updated.\n"); pause()

    } else if (ch == 9) {
      res <- generate_synthetic()
      a <- res$a; b <- res$b
      pause()

    } else if (ch == 10) {
      save_results(last_result); pause()

    } else if (ch == 0) {
      a <- c(2000, 2500, 3000, 1900)
      b <- c(1500, 1800, 1200, 1600)
      last_result <- NULL
      cat("Reset to starter samples.\n"); pause()

    } else {
      cat("Invalid option.\n"); pause()
    }
  }
  cat("Exiting T-Test tutorial. Goodbye.\n")
}

# Run when script executed
interactive_menu()