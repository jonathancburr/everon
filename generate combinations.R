generate_combinations <- function(x) {
  n <- length(x)
  results <- list()
  
  # Recursive helper
  recurse <- function(start, parts) {
    if (start > n) {
      results[[length(results) + 1]] <<- parts
      return()
    }
    
    for (end in start:n) {
      part <- paste(x[start:end], collapse = "+")
      recurse(end + 1, c(parts, part))
    }
  }
  
  recurse(1, character(0))
  return(results)
}

# Example usage:
segments <- c("a", "b", "c", "d")
combinations <- generate_combinations(segments)

# Display all combinations
for (combo in combinations) {
  print(combo)
}
