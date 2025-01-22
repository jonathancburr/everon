CAD_get_activity_by_average_maximum_difference <- function(filter_maximum_difference_sum, counter_fifo_reading, filter_size) {
  # Initialize variables
  sum_average_difference <- 0
  average_maximum_difference <- 0
  
  # Calculate the sum of the maximum differences
  for (pointer in seq_len(counter_fifo_reading)) {
    sum_average_difference <- sum_average_difference + filter_maximum_difference_sum[pointer]
    filter_maximum_difference_sum[pointer] <- 0
    
    # Break the loop if the pointer reaches the maximum filter size
    if (pointer >= filter_size) {
      break
    }
  }
  
  # Calculate the average maximum difference
  if (counter_fifo_reading > 0) {
    average_for_sqrt <- sum_average_difference / counter_fifo_reading
    average <- min(average_for_sqrt, 0xffff) # Clamp to 16 bits
    average_maximum_difference <- floor(sqrt(average)) # Square root
  } else {
    average_maximum_difference <- 0
  }
  
  # Ensure the activity value is at least 1
  average_maximum_difference <- max(average_maximum_difference, 1)
  
  # Return the result
  return(average_maximum_difference)
}

CAD_calculate_activity_by_current_difference <- function(acceleration_current, acceleration_previous, 
                                                         AXIS_NOISE_FILTER_LIMIT_CURRENT, filter_maximum_difference_sum, 
                                                         pointer_filter, filter_size, counter_fifo_reading) {
  # Initialize variables
  difference <- c(0, 0, 0)
  maximum_axis_activity <- 0
  
  # Check if previous acceleration values are initialized
  if (any(acceleration_previous != 0)) {
    # Calculate differences for each axis
    difference <- abs(acceleration_current - acceleration_previous)
    
    # Apply noise filtering
    difference <- ifelse(difference < AXIS_NOISE_FILTER_LIMIT_CURRENT, 0, difference)
    
    # Calculate the maximum activity
    maximum_axis_activity <- sum(difference)
  }
  
  # Update the FIFO buffer
  pointer_filter <- (pointer_filter + 1) %% filter_size
  filter_maximum_difference_sum[pointer_filter + 1] <- maximum_axis_activity
  
  # Update the previous acceleration values
  acceleration_previous <- acceleration_current
  
  # Increment the counter
  counter_fifo_reading <- counter_fifo_reading + 1
  
  # Return updated values as a list
  return(list(
    filter_maximum_difference_sum = filter_maximum_difference_sum,
    pointer_filter = pointer_filter,
    acceleration_previous = acceleration_previous,
    counter_fifo_reading = counter_fifo_reading
  ))
}

# Initialize variables
acceleration_current <- c(2, 2, 2)
acceleration_previous <- c(1, 1, 1)
AXIS_NOISE_FILTER_LIMIT_CURRENT <- 3
filter_maximum_difference_sum <- rep(0, 8) # Example buffer size of 8
pointer_filter <- 0
filter_size <- length(filter_maximum_difference_sum)
counter_fifo_reading <- 0

# Call the function
result <- CAD_calculate_activity_by_current_difference(
  acceleration_current, acceleration_previous,
  AXIS_NOISE_FILTER_LIMIT_CURRENT, filter_maximum_difference_sum,
  pointer_filter, filter_size, counter_fifo_reading
)

# Extract updated variables
filter_maximum_difference_sum <- result$filter_maximum_difference_sum
pointer_filter <- result$pointer_filter
acceleration_previous <- result$accele
ration_previous
counter_fifo_reading <- result$counter_fifo_reading

# Print updated buffer
print(filter_maximum_difference_sum)


