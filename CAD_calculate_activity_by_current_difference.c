#include <stdint.h> // Include the header for uint16_t


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