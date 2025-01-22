#include <stdint.h> // Include the header for uint16_t


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


