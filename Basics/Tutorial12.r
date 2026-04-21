# Tutorial12.r
cat("--- Zamtel Credit Control Portal ---\n")

# Using file("stdin") is the most 'aggressive' way to force a pause
cat("Enter Agent Name: ")
agent_name <- readLines(con = file("stdin"), n = 1)

cat("Enter Client Account: ")
account_id <- readLines(con = file("stdin"), n = 1)

cat("\n[LOG] Accessing records for Account:", account_id)
cat("\n[LOG] Authenticated by Agent:", agent_name, "\n")
cat("SYSTEM SECURITY\n")
cat("Enter Access Code: ")
code <- readLines(con = file("stdin"), n = 1)

if (code == "Zamtel2026") {
  cat("ACCESS GRANTED. Welcome back.\n")
} else {
  cat("ACCESS DENIED. Invalid code.\n")
}

cat("Enter the names of 3 clients (press Enter after each):\n")
clients <- readLines(con = file("stdin"), n = 3)

cat("\nProcessing the following batch:\n")
# This prints the list we just created


print(clients) 

# Access the second client specifically
cat("The second client in this batch is:", clients[2], "\n")

cat("--- Quick Debt Adjustment Tool ---\n")

cat("Enter Current Balance: ")
# Convert input to numeric immediately
current_bal <- as.numeric(readLines(con = file("stdin"), n = 1))

cat("Enter Payment Received: ")
payment <- as.numeric(readLines(con = file("stdin"), n = 1))

# Perform the math
new_bal <- current_bal - payment

cat("\n----------------------------------\n")
cat("Adjustment Successful.\n")
cat("New Remaining Balance: ZMW", format(new_bal, nsmall = 2), "\n")
cat("----------------------------------\n")

cat("Enter Note: ")
note <- readLines(con = file("stdin"), n = 1)

if (nchar(note) == 0) {
  cat("Error: You didn't type anything!\n")
} else {
  cat("Note saved:", note, "\n")
}