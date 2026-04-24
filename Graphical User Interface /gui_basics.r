# ======================================================
# gui_basics.R
# Simple tcltk example – a window with a button
# ======================================================

# Load the tcltk package (comes with base R)
library(tcltk)

# Create the main window
main_window <- tktoplevel()

# Set window title
tktitle(main_window) <- "My First R GUI"

# Set window size (width, height)
tkwm.geometry(main_window, "300x150")

# Create a label (optional)
label <- tklabel(main_window, text = "Welcome to R GUI!")
tkpack(label, padx = 10, pady = 10)

# Define what happens when the button is clicked
on_click <- function() {
  # Show a popup message
  tkmessageBox(type = "ok", 
               message = "Button clicked! 🎉", 
               title = "Info")
}

# Create a button
button <- tkbutton(main_window, 
                   text = "Click Me!", 
                   command = on_click)

# Pack the button into the window
tkpack(button, padx = 10, pady = 10)

# Start the GUI event loop
tkfocus(main_window)
