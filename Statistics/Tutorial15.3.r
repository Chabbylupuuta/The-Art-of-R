# Tutorial15.3.r: Correlation & Simple Regression (interactive)
# Base R only. Run in terminal: Rscript Tutorial15.3.r
# ------------------------------------------------------------------------------

# Sample paired data (editable during session)
limits <- c(5000, 10000, 20000, 5000, 15000, 8000, 5000)
debt   <- c(1992, 5000, 12500, 1992, 8000, 4500, 1992)

# Robust stdin helpers
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

# Terminal helpers
clear_screen <- function() try(system("clear"), silent = TRUE)
pause <- function() { cat("\nPress Enter to return to menu..."); invisible(readLines(con = file("stdin"), n = 1)) }

# Validate pair lengths
validate_pairs <- function(x, y) {
  if (length(x) != length(y)) stop("Vectors must have equal length.")
  if (length(x) < 2) stop("Need at least 2 pairs to compute correlation.")
  return(TRUE)
}

# Show Pearson & Spearman correlation with hypothesis test
show_correlation <- function(x, y) {
  tryCatch({
    validate_pairs(x, y)
    cat("\n--- Correlation summary ---\n")
    cat("Pairs (n):", length(x), "\n")
    pearson <- cor.test(x, y, method = "pearson")
    spearman <- cor.test(x, y, method = "spearman")
    cat(sprintf("\nPearson r = %.4f  (95%% CI: %.4f to %.4f)  p = %.4g\n",
                pearson$estimate, pearson$conf.int[1], pearson$conf.int[2], pearson$p.value))
    cat(sprintf("Spearman rho = %.4f  p = %.4g\n", spearman$estimate, spearman$p.value))
    # Interpretation hint
    r <- as.numeric(pearson$estimate)
    strength <- if (abs(r) >= 0.9) "very strong" else if (abs(r) >= 0.7) "strong" else if (abs(r) >= 0.5) "moderate" else if (abs(r) >= 0.3) "weak" else "very weak"
    cat("Interpretation (Pearson):", strength, "linear relationship.\n")
    invisible(list(pearson = pearson, spearman = spearman))
  }, error = function(e) {
    cat("[ERROR]", e$message, "\n"); NULL
  })
}

# Scatter plot with regression line (saves PNG when non-interactive)
plot_scatter_with_lm <- function(x, y, file = "limits_vs_debt.png", log_transform = FALSE) {
  tryCatch({
    validate_pairs(x, y)
    if (log_transform) {
      x1 <- log10(x + 1); y1 <- log10(y + 1)
      title_suffix <- " (log10+1 transformed)"
    } else {
      x1 <- x; y1 <- y; title_suffix <- ""
    }
    if (interactive()) {
      plot(x1, y1, pch = 19, col = "steelblue", xlab = "Credit Limit", ylab = "Debt",
           main = paste("Limit vs Debt", title_suffix))
      m <- lm(y1 ~ x1)
      abline(m, col = "red", lwd = 2)
      legend("topleft", legend = c(sprintf("R^2=%.3f", summary(m)$r.squared)), bty = "n")
      invisible(summary(m))
    } else {
      png(file, width = 800, height = 600)
      plot(x1, y1, pch = 19, col = "steelblue", xlab = "Credit Limit", ylab = "Debt",
           main = paste("Limit vs Debt", title_suffix))
      m <- lm(y1 ~ x1)
      abline(m, col = "red", lwd = 2)
      legend("topleft", legend = c(sprintf("R^2=%.3f", summary(m)$r.squared)), bty = "n")
      dev.off()
      cat("Saved scatter plot to", file, "\n")
      invisible(summary(m))
    }
  }, error = function(e) {
    cat("[ERROR] Plot failed:", e$message, "\n"); NULL
  })
}

# Linear regression summary (returns model)
show_regression <- function(x, y, log_transform = FALSE) {
  tryCatch({
    validate_pairs(x, y)
    if (log_transform) {
      x1 <- log10(x + 1); y1 <- log10(y + 1)
    } else {
      x1 <- x; y1 <- y
    }
    model <- lm(y1 ~ x1)
    cat("\n--- Linear regression summary ---\n")
    print(summary(model))
    cat("\nCoefficients:\n"); print(coef(model))
    invisible(model)
  }, error = function(e) {
    cat("[ERROR] Regression failed:", e$message, "\n"); NULL
  })
}

# Manage paired data: add / remove / list
add_pair <- function(x, y) {
  L <- get_numeric_input("Enter credit limit to add: ")
  if (is.na(L)) { cat("Cancelled.\n"); return(list(x=x,y=y)) }
  D <- get_numeric_input("Enter corresponding debt amount: ")
  if (is.na(D)) { cat("Cancelled.\n"); return(list(x=x,y=y)) }
  x <- c(x, L); y <- c(y, D)
  cat("Added pair -> limit:", L, " debt:", D, "\n")
  return(list(x = x, y = y))
}
remove_pair <- function(x, y) {
  if (length(x) == 0) { cat("No pairs to remove.\n"); return(list(x=x,y=y)) }
  cat("Current pairs (index: limit -> debt):\n")
  for (i in seq_along(x)) cat(i, ": ", x[i], " -> ", y[i], "\n")
  idx <- get_integer_input("Enter index to remove (0 to cancel): ", default = 0)
  if (is.na(idx) || idx == 0) { cat("Cancelled.\n"); return(list(x=x,y=y)) }
  if (idx >= 1 && idx <= length(x)) {
    cat("Removing pair:", x[idx], "->", y[idx], "\n")
    x <- x[-idx]; y <- y[-idx]
  } else {
    cat("Index out of range.\n")
  }
  return(list(x = x, y = y))
}

# Interactive menu
interactive_menu <- function() {
  x <- limits; y <- debt
  repeat {
    clear_screen()
    cat("\n=========== Correlation & Regression Tutorial 15.3 ===========\n")
    cat("1) Show Pearson & Spearman correlations (with tests)\n")
    cat("2) Show linear regression summary (debt ~ limit)\n")
    cat("3) Scatter plot with regression line (save PNG if non-interactive)\n")
    cat("4) Scatter + regression (log10+1 transform)\n")
    cat("5) Add a limit-debt pair\n")
    cat("6) Remove a pair by index\n")
    cat("7) List pairs\n")
    cat("0) Exit\n")
    cat("---------------------------------------------------------------\n")
    choice <- get_integer_input("Choose option [0-7]: ", default = NA_integer_)
    if (is.na(choice)) { cat("No selection. Try again.\n"); pause(); next }
    if (choice == 1) {
      show_c <- show_correlation(x, y); pause()
    } else if (choice == 2) {
      show_regression(x, y, log_transform = FALSE); pause()
    } else if (choice == 3) {
      plot_scatter_with_lm(x, y, file = "limits_vs_debt.png", log_transform = FALSE); pause()
    } else if (choice == 4) {
      plot_scatter_with_lm(x, y, file = "limits_vs_debt_log.png", log_transform = TRUE); pause()
    } else if (choice == 5) {
      res <- add_pair(x, y); x <- res$x; y <- res$y; pause()
    } else if (choice == 6) {
      res <- remove_pair(x, y); x <- res$x; y <- res$y; pause()
    } else if (choice == 7) {
      cat("\nPairs (index: limit -> debt):\n")
      for (i in seq_along(x)) cat(i, ": ", x[i], " -> ", y[i], "\n")
      pause()
    } else if (choice == 0) {
      cat("Exiting. Goodbye.\n"); break
    } else {
      cat("Invalid option.\n"); pause()
    }
  }
}

# Run when script executed
interactive_menu()