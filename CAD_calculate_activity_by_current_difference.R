#' Calculate activity by current difference on each axis
#' 
#' This function mimics the logic of the C function CAD_calculate_activity_by_current_difference.
#' It expects the following global variables (as in the C code):
#'   - acceleration_current: numeric vector of length 3 (current X, Y, Z)
#'   - acceleration_previous: numeric vector of length 3 (previous X, Y, Z)
#'   - AXIS_NOISE_FILTER_LIMIT_CURRENT: numeric threshold for noise filtering
#'   - filter_maximum_difference_sum: numeric vector acting as a circular buffer
#'   - pointer_filter: integer index for the circular buffer
#'   - FILTER_SIZE: integer, size of the buffer
#'   - counter_fifo_reading: integer counter for buffer usage
#' 
#' The axes are assumed to be indexed as 1=X, 2=Y, 3=Z in R (not 0-based as in C).

CAD_calculate_activity_by_current_difference <- function() {
  # Calculate differences between current and previous acceleration for each axis
  difference <- abs(acceleration_current - acceleration_previous)

  # Filter out small differences (noise)
  difference[difference < AXIS_NOISE_FILTER_LIMIT_CURRENT] <- 0

  # Calculate the sum of the differences (activity)
  maximum_axis_activity <- sum(difference)

  # Store the result in the circular buffer
  filter_maximum_difference_sum[pointer_filter] <<- maximum_axis_activity
  pointer_filter <<- (pointer_filter %% FILTER_SIZE) + 1  # R is 1-based

  # Update previous acceleration values
  acceleration_previous <<- acceleration_current

  # Increase FIFO reading counter
  counter_fifo_reading <<- counter_fifo_reading + 1
}

# Example of initializing variables (uncomment and modify as needed):
# acceleration_current <- c(0, 0, 0)
# acceleration_previous <- c(0, 0, 0)
# AXIS_NOISE_FILTER_LIMIT_CURRENT <- 5
# FILTER_SIZE <- 32
# filter_maximum_difference_sum <- rep(0, FILTER_SIZE)
# pointer_filter <- 1
# counter_fifo_reading <- 0
