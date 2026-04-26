# ======================================================
# Complete R script for interfacing with C
# Creates example1.c, compiles, calls the function, cleans up
# ======================================================

# 1. Write the C source code to disk
c_code <- '
#include <R.h>
#include <Rinternals.h>

// Add two doubles using .Call interface
SEXP add(SEXP a, SEXP b) {
    double da = REAL(a)[0];
    double db = REAL(b)[0];
    double sum = da + db;
    
    SEXP result = PROTECT(allocVector(REALSXP, 1));
    REAL(result)[0] = sum;
    UNPROTECT(1);
    return result;
}
'

writeLines(c_code, "example1.c")
cat("✅ Created example1.c\n")

# 2. Compile the C code into a shared library
#    (R CMD SHLIB works on all platforms if compilers are set up)
cat("Compiling example1.c ...\n")
compile_status <- system("R CMD SHLIB example1.c")

if (compile_status != 0) {
    stop("Compilation failed. On Windows, ensure Rtools is installed and in PATH.\n",
         "On Linux/macOS, make sure you have build tools (gcc).")
}
cat("✅ Compilation successful.\n")

# 3. Determine the correct shared library file extension
if (.Platform$OS.type == "windows") {
    so_file <- "example1.dll"
} else if (Sys.info()["sysname"] == "Darwin") {
    so_file <- "example1.dylib"
} else {
    so_file <- "example1.so"
}

# 4. Load the shared library
dyn.load(so_file)
cat("✅ Loaded", so_file, "\n")

# 5. Call the C function from R
#    The function is called "add" as defined in the C code
result <- .Call("add", 3.5, 2.7)
cat("\n🎉 Result from C: 3.5 + 2.7 =", result, "\n")

# 6. Another test
result2 <- .Call("add", 100.0, 200.0)
cat("🎉 Result from C: 100 + 200 =", result2, "\n\n")

# 7. Unload the library (optional, but good practice)
dyn.unload(so_file)
cat("✅ Unloaded", so_file, "\n")

# 8. Clean up temporary files (keep the .c if you want to inspect)
cat("Cleaning up compiled files...\n")
file.remove(list.files(pattern = "example1\\.(o|so|dll|dylib)"))
cat("✅ Cleanup complete.\n")

# 9. Show that the C file remains for inspection
cat("\n📁 example1.c is still in your working directory.\n")
cat("You can open it to see the C code.\n")
