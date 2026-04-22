# Tutorial15.4.r: Linear Regression (improved) — includes MAE, RMSE, CV, interactive menu
# Base R only. Run in terminal: Rscript Tutorial15.4.r
# ------------------------------------------------------------------------------

# Sample paired data (editable during session)
limits <- c(5000, 10000, 20000, 5000, 15000, 8000, 5000)
debt   <- c(1992, 5000, 12500, 1992, 8000, 4500, 1992)

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
clear_screen <- function() try(system("clear"), silent = TRUE)
pause <- function() { cat("\nPress Enter to return to menu..."); invisible(readLines(con = file("stdin"), n = 1)) }

# Utility: compute metrics
compute_metrics <- function(actual, predicted) {
  resid <- actual - predicted
  mae <- mean(abs(resid), na.rm = TRUE)
  rmse <- sqrt(mean(resid^2, na.rm = TRUE))
  mape <- mean(abs(resid / ifelse(actual==0, NA, actual)), na.rm = TRUE) * 100
  list(mae = mae, rmse = rmse, mape = mape, residuals = resid)
}

# Fit linear model on provided data frame
fit_model <- function(df) {
  model <- lm(debt ~ limits, data = df)
  return(model)
}

# Train/test split evaluation
train_test_eval <- function(x, y, train_frac = 0.7, seed = 123) {
  set.seed(seed)
  n <- length(x)
  idx <- sample(seq_len(n), size = floor(train_frac * n))
  train_df <- data.frame(limits = x[idx], debt = y[idx])
  test_df  <- data.frame(limits = x[-idx], debt = y[-idx])
  model <- fit_model(train_df)
  preds <- predict(model, newdata = test_df)
  metrics <- compute_metrics(test_df$debt, preds)
  list(model = model, train = train_df, test = test_df, metrics = metrics)
}

# k-fold cross-validation (simple)
k_fold_cv <- function(x, y, k = 5, seed = 123) {
  set.seed(seed)
  n <- length(x)
  folds <- sample(rep(1:k, length.out = n))
  results <- vector("list", k)
  for (i in seq_len(k)) {
    train_idx <- which(folds != i)
    test_idx  <- which(folds == i)
    train_df <- data.frame(limits = x[train_idx], debt = y[train_idx])
    test_df  <- data.frame(limits = x[test_idx], debt = y[test_idx])
    model <- fit_model(train_df)
    preds <- predict(model, newdata = test_df)
    metrics <- compute_metrics(test_df$debt, preds)
    results[[i]] <- metrics
  }
  maes <- sapply(results, function(r) r$mae)
  rmses <- sapply(results, function(r) r$rmse)
  list(mae_mean = mean(maes), mae_sd = sd(maes), rmse_mean = mean(rmses), rmse_sd = sd(rmses), per_fold = results)
}

# Plot diagnostics (saves PNG if not interactive)
plot_diagnostics <- function(model, file_prefix = "reg_diagnostics") {
  if (is.null(model)) { cat("No model provided.\n"); return(NULL) }
  if (interactive()) {
    par(mfrow = c(2,2))
    plot(model)
    par(mfrow = c(1,1))
  } else {
    png(paste0(file_prefix, ".png"), width = 1200, height = 900)
    par(mfrow = c(2,2))
    plot(model)
    par(mfrow = c(1,1))
    dev.off()
    cat("Saved diagnostics to", paste0(file_prefix, ".png"), "\n")
  }
  invisible(TRUE)
}

# Interactive menu
interactive_menu <- function() {
  x <- limits; y <- debt
  fitted_model <- NULL
  repeat {
    clear_screen()
    cat("\n============ Tutorial 15.4 — Linear Regression (Debt ~ Limit) ============\n")
    cat("1) Fit model on ALL data and show summary\n")
    cat("2) Evaluate with train/test split (show MAE, RMSE, MAPE)\n")
    cat("3) k-Fold cross-validation (report mean MAE & RMSE)\n")
    cat("4) Predict debt for a given credit limit\n")
    cat("5) Plot regression diagnostics (residuals, QQ, fitted vs actual)\n")
    cat("6) Add a limit-debt pair\n")
    cat("7) Remove a pair by index\n")
    cat("8) List pairs\n")
    cat("9) Save current model to file (RDS)\n")
    cat("0) Exit\n")
    cat("--------------------------------------------------------------------------\n")
    choice <- get_integer_input("Choose option [0-9]: ", default = NA_integer_)
    if (is.na(choice)) { cat("No selection. Try again.\n"); pause(); next }

    if (choice == 1) {
      df <- data.frame(limits = x, debt = y)
      fitted_model <- fit_model(df)
      cat("\n--- Model summary (fitted on ALL data) ---\n")
      print(summary(fitted_model))
      preds <- predict(fitted_model, newdata = df)
      m <- compute_metrics(df$debt, preds)
      cat(sprintf("\nMAE: %.2f   RMSE: %.2f   MAPE: %.2f%%\n", m$mae, m$rmse, m$mape))
      pause()

    } else if (choice == 2) {
      frac <- get_numeric_input("Train fraction (0.1 - 0.9) [default 0.7]: ", default = 0.7)
      if (is.na(frac) || frac <= 0 || frac >= 1) { cat("Invalid fraction. Using 0.7\n"); frac <- 0.7 }
      res <- train_test_eval(x, y, train_frac = frac)
      cat("\n--- Train/Test evaluation ---\n")
      cat(sprintf("Train n = %d   Test n = %d\n", nrow(res$train), nrow(res$test)))
      cat(sprintf("MAE (test): %.2f   RMSE (test): %.2f   MAPE (test): %.2f%%\n",
                  res$metrics$mae, res$metrics$rmse, res$metrics$mape))
      fitted_model <- res$model
      pause()

    } else if (choice == 3) {
      k <- get_integer_input("Number of folds k [default 5]: ", default = 5)
      if (is.na(k) || k < 2) { cat("Invalid k. Using 5.\n"); k <- 5 }
      cv <- k_fold_cv(x, y, k = k)
      cat("\n--- k-Fold CV results ---\n")
      cat(sprintf("Mean MAE = %.2f  (sd = %.2f)\nMean RMSE = %.2f  (sd = %.2f)\n",
                  cv$mae_mean, cv$mae_sd, cv$rmse_mean, cv$rmse_sd))
      pause()

    } else if (choice == 4) {
      if (is.null(fitted_model)) {
        df_all <- data.frame(limits = x, debt = y)
        fitted_model <- fit_model(df_all)
      }
      lim <- get_numeric_input("Enter credit limit to predict debt for: ")
      if (is.na(lim)) { cat("Cancelled.\n"); pause(); next }
      pred <- predict(fitted_model, newdata = data.frame(limits = lim))
      cat(sprintf("\nPredicted debt for limit = %.2f is approx ZMW %.2f\n", lim, pred))
      pause()

    } else if (choice == 5) {
      if (is.null(fitted_model)) {
        df_all <- data.frame(limits = x, debt = y)
        fitted_model <- fit_model(df_all)
      }
      plot_diagnostics(fitted_model)
      pause()

    } else if (choice == 6) {
      L <- get_numeric_input("Enter credit limit to add: ")
      if (is.na(L)) { cat("Cancelled.\n"); pause(); next }
      D <- get_numeric_input("Enter debt for this limit: ")
      if (is.na(D)) { cat("Cancelled.\n"); pause(); next }
      x <- c(x, L); y <- c(y, D)
      cat("Added pair -> limit:", L, " debt:", D, "\n")
      fitted_model <- NULL
      pause()

    } else if (choice == 7) {
      if (length(x) == 0) { cat("No pairs to remove.\n"); pause(); next }
      cat("Pairs (index: limit -> debt):\n")
      for (i in seq_along(x)) cat(i, ": ", x[i], " -> ", y[i], "\n")
      idx <- get_integer_input("Enter index to remove (0 to cancel): ", default = 0)
      if (!is.na(idx) && idx >= 1 && idx <= length(x)) {
        cat("Removing pair:", x[idx], "->", y[idx], "\n")
        x <- x[-idx]; y <- y[-idx]
        fitted_model <- NULL
      } else {
        cat("Cancelled or out of range.\n")
      }
      pause()

    } else if (choice == 8) {
      cat("\nPairs (index: limit -> debt):\n")
      for (i in seq_along(x)) cat(i, ": ", x[i], " -> ", y[i], "\n")
      pause()

    } else if (choice == 9) {
      if (is.null(fitted_model)) {
        df_all <- data.frame(limits = x, debt = y)
        fitted_model <- fit_model(df_all)
      }
      fname <- get_user_input("Filename to save model [model_debt_limit.rds]: ")
      if (!nzchar(fname)) fname <- "model_debt_limit.rds"
      saveRDS(fitted_model, file = fname)
      cat("Model saved to", fname, "\n")
      pause()

    } else if (choice == 0) {
      cat("Exiting. Goodbye.\n"); break

    } else {
      cat("Invalid option.\n"); pause()
    }
  }
}

# Run menu when script executed
interactive_menu()