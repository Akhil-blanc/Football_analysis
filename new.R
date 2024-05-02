list_used_packages <- function(directory) {
  # Get the names of all R files in the directory
  r_files <- list.files(directory, pattern = "\\.R$", full.names = TRUE, recursive = TRUE)
  
  # Initialize an empty vector to store the package names
  packages <- c()
  
  # Loop over the R files
  for (file in r_files) {
    # Read the file
    lines <- readLines(file)
    
    # Find lines that call the library() or require() function
    library_lines <- grep("^(library|require)\\(", lines)
    
    # Extract the package names from these lines
    pkgs <- gsub("^(library|require)\\((.*)\\).*", "\\2", lines[library_lines])
    
    # Remove any trailing comments
    pkgs <- gsub("\\s*#.*", "", pkgs)
    
    # Remove quotes
    pkgs <- gsub("\"", "", pkgs)
    pkgs <- gsub("'", "", pkgs)
    
    # Add the package names to the vector
    packages <- c(packages, pkgs)
  }
  
  # Remove duplicates
  packages <- unique(packages)
  
  return(packages)
}

# Use the function
used_packages <- list_used_packages(".")
print(used_packages)