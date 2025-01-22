/**

* @brief   Returns activity value based on the average of the maximum

*          differences located in the filter. When activity value has been

*          defined, the filter is reset with zeros.

*

* @param   None

*

* @return  Activity value

*/

uint8_t CAD_get_activity_by_average_maximum_difference(void)

{

   volatile uint32_t average_for_sqrt = 0.0;

   volatile uint64_t sum_average_difference = 0;

   volatile uint8_t average_maximum_difference = 0;

   uint8_t pointer = 0;


   // Calculate the total sum of the maximum differences.

   for (pointer = 0 ; pointer < counter_fifo_reading ; pointer++)

   {

       sum_average_difference += filter_maximum_difference_sum[pointer];

       filter_maximum_difference_sum[pointer] = 0;


       // The loop has to be broken if the maximum pointer (index)

       // value has been reached.

       if (pointer >= (FILTER_SIZE-1))

       {

           break;

       }

   }


   // Calculate average maximum difference.

   if (counter_fifo_reading > 0)

   {

       average_for_sqrt = sum_average_difference / counter_fifo_reading;

       uint16_t average = (average_for_sqrt > 0xffff ? 0xffff : average_for_sqrt);

       average_maximum_difference = (uint8_t)(floorSqrt(average));

   }

   else

   {

       average_maximum_difference = 0;

   }


   // Set the pointer to point the first item of the filter.

   // Reset also FIFO reading counter.

   CAD_reset_table();


   // NOTE! Activity value has to be at least 1, otherwise the server does

   // not update the device status.

   if (average_maximum_difference < 1)

   {

       average_maximum_difference = 1;

   }


   // Return the average maximum difference.

   return average_maximum_difference;

}

and here is the code that fills the fifo where the average is calculated:


/**

* @brief   Reads acceleration values of axes and compares them to the previous

*          values. The noise filter is used to remove too low differences in

*          case the device is not move at all.

*

* @param   None

*

* @return  none

*/

void CAD_calculate_activity_by_current_difference(void)

{

   uint16_t difference[3] = {0,0,0};


   volatile uint16_t diff_axis_x = 0;

   volatile uint16_t diff_axis_y = 0;

   volatile uint16_t diff_axis_z = 0;


   static volatile uint16_t maximum_axis_activity;


   // Check that the previous acceleration values have been defined. It is

   // unlike that all axes values are zero after the first previous axes

   // data has been defined.

   if (acceleration_previous[AXIS_X] != 0 ||

        acceleration_previous[AXIS_Y] != 0 ||

         acceleration_previous[AXIS_Z] != 0)

   {

       // Calculate difference values for each axis data by subtracting the

       // previous axis data from the current axis data. If the device has

       // been moved, there has to be difference in axis values.

       difference[AXIS_X] = (uint16_t)abs(acceleration_current[AXIS_X] - acceleration_previous[AXIS_X]);

       difference[AXIS_Y] = (uint16_t)abs(acceleration_current[AXIS_Y] - acceleration_previous[AXIS_Y]);

       difference[AXIS_Z] = (uint16_t)abs(acceleration_current[AXIS_Z] - acceleration_previous[AXIS_Z]);


       // Filter axis noise. Even the device is stable there is little

       // difference in axis values that has to be eliminated.

       if (difference[AXIS_X] < AXIS_NOISE_FILTER_LIMIT_CURRENT) {difference[AXIS_X] = 0;}

       if (difference[AXIS_Y] < AXIS_NOISE_FILTER_LIMIT_CURRENT) {difference[AXIS_Y] = 0;}

       if (difference[AXIS_Z] < AXIS_NOISE_FILTER_LIMIT_CURRENT) {difference[AXIS_Z] = 0;}


       // Calculate maximum values of each axis.

       maximum_axis_activity = difference[AXIS_X];

       maximum_axis_activity += difference[AXIS_Y];

       maximum_axis_activity += difference[AXIS_Z];

   }


   // Shift data sum table data.

   filter_maximum_difference_sum[pointer_filter++] = maximum_axis_activity;

   pointer_filter &= (FILTER_SIZE-1);


   // Set current data as a previous data for next comparison.

   acceleration_previous[AXIS_X] = acceleration_current[AXIS_X];

   acceleration_previous[AXIS_Y] = acceleration_current[AXIS_Y];

   acceleration_previous[AXIS_Z] = acceleration_current[AXIS_Z];


   // Clear maximum activity for the next loop.

   maximum_axis_activity = 0;


   // Increase FIFO reading counter.

   counter_fifo_reading++;

}




/**