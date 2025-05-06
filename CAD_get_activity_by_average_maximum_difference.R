#' Calculate activity value based on the average of the maximum differences in the filter
#' 
#' This function mimics the logic of the C function CAD_get_activity_by_average_maximum_difference.
#' It expects the following global variables (as in the C code):
#'   - filter_maximum_difference_sum: numeric vector (circular buffer)
#'   - counter_fifo_reading: integer, number of valid entries in the buffer
#'   - FILTER_SIZE: integer, size of the buffer
#'   - pointer_filter: integer, buffer index (reset in this function)
#'   - CAD_reset_table: function to reset buffer and counter
#'   - floorSqrt: function to compute integer sqrt (or use base::floor(sqrt(...)))
#' 
#' Returns: activity value (integer >= 1)

CAD_get_activity_by_average_maximum_difference <- function() {
  sum_average_difference <- 0
  pointer <- 1
  # Sum the values in the buffer up to counter_fifo_reading or FILTER_SIZE, whichever is smaller
  n <- min(counter_fifo_reading, FILTER_SIZE)
  if (n > 0) {
    for (pointer in 1:n) {
      sum_average_difference <- sum_average_difference + filter_maximum_difference_sum[pointer]
      filter_maximum_difference_sum[pointer] <<- 0
    }
    average_for_sqrt <- sum_average_difference / n
    average <- ifelse(average_for_sqrt > 0xffff, 0xffff, average_for_sqrt)
    # Use integer square root, as in C
    average_maximum_difference <- floor(sqrt(average))
  } else {
    average_maximum_difference <- 0
  }
  # Reset buffer and counter (mimics CAD_reset_table)
  filter_maximum_difference_sum <<- rep(0, FILTER_SIZE)
  pointer_filter <<- 1
  counter_fifo_reading <<- 0
  # Ensure minimum value is 1
  if (average_maximum_difference < 1) {
    average_maximum_difference <- 1
  }
  return(average_maximum_difference)
}

# Example of initializing variables (uncomment and modify as needed):
# FILTER_SIZE <- 32
# filter_maximum_difference_sum <- rep(0, FILTER_SIZE)
# counter_fifo_reading <- 0
# pointer_filter <- 1
