library(R6)

# 1. THE CLASS DEFINITION (The Blueprint)
# ------------------------------------------------------------------------------
AccountManager <- R6Class("AccountManager",
  public = list(
    client_name = NULL,
    account_id = NULL,
    balance = 0,
    history = NULL, 
    
    initialize = function(name, id, initial_bal) {
      self$client_name <- name
      self$account_id <- id
      self$balance <- initial_bal
      
      # Initialize the Audit Trail
      self$history <- data.frame(
        Date = Sys.Date(),
        Action = "Opening Balance",
        Amount = initial_bal,
        Running_Balance = initial_bal,
        stringsAsFactors = FALSE
      )
    },
    
    # Method to reduce debt
    post_payment = function(amount) {
      if(amount <= 0) stop("Payment must be positive.")
      self$balance <- self$balance - amount
      private$log_action("Payment Received", -amount)
      cat("\n[PAYMENT] ZMW", amount, "posted for", self$client_name)
    },
    
    # Method to increase debt
    post_invoice = function(amount) {
      if(amount <= 0) stop("Invoice must be positive.")
      self$balance <- self$balance + amount
      private$log_action("Invoice Issued", amount)
      cat("\n[INVOICE] ZMW", amount, "billed to", self$client_name)
    },
    
    # Method to generate a professional summary
    generate_statement = function() {
      cat("\n\n================================================")
      cat("\nOFFICIAL STATEMENT: ", toupper(self$client_name))
      cat("\nAccount ID:         ", self$account_id)
      cat("\nCurrent Balance:    ZMW", format(self$balance, nsmall = 2, big.mark = ","))
      cat("\nStatus:             ", ifelse(self$balance > 10000, "OVER LIMIT", "REGULAR"))
      cat("\n================================================")
      print(self$history)
      cat("================================================\n")
    },

    # Method to save history to CSV (for your email attachments)
    save_ledger = function() {
      filename <- paste0(gsub(" ", "_", self$client_name), "_Ledger.csv")
      write.csv(self$history, filename, row.names = FALSE)
      cat("\n[SYSTEM] Ledger saved as:", filename)
    }
  ),
  
  private = list(
    log_action = function(action, amount) {
      new_entry <- data.frame(
        Date = Sys.Date(),
        Action = action,
        Amount = amount,
        Running_Balance = self$balance,
        stringsAsFactors = FALSE
      )
      self$history <- rbind(self$history, new_entry)
    }
  )
)

# 2. THE PORTFOLIO (Applying the Blueprint)
# ------------------------------------------------------------------------------
portfolio <- list(
  Lactalis = AccountManager$new("Lactalis-Parmalat", "16416734", 1992.00),
  BlueNile = AccountManager$new("Blue Nile Investments", "16074433", 5000.00),
  Zamtel   = AccountManager$new("Zamtel Corporate", "16000000", 12500.00)
)

# 3. ACTIONS & SIMULATIONS
# ------------------------------------------------------------------------------

# Post a payment for Lactalis
portfolio$Lactalis$post_payment(500)

# Post a new invoice for Blue Nile
portfolio$BlueNile$post_invoice(2500)

# Generate statements for everyone in the portfolio
cat("\n\n--- GENERATING ALL CORPORATE STATEMENTS ---\n")
lapply(portfolio, function(client) client$generate_statement())

# Export the Lactalis ledger to a CSV file on your Desktop/Folder
portfolio$Lactalis$save_ledger()