# Notes of a conversation with Oliver Dickson 22/01/25

## Wait time with normally distributed Everon data

* Wait time to be expressed as number of successive readings where value less than n standard deviations from mean

* Getting data every 1 minute in Everon so don't need to wait for a complete bin

* Use logic of QCC run chart - can run experiments with QCC package

* Need to ensure that data is normally distributed 

## Activity type analysis

* Basic classification of activity eg
  * sleep
  * at rest (during day)
  * walking 
* Collect accelerometer data using mobile phone for various periods
* Process results using logic of C code
* Compare against real world Everon data and assess reasonableness 

 ## Products 

* Assessment report API
* Data stream with webhook for alerts 
  * Routine
    * change in routine settings
    * change in activity
  * Intra-day
    * unusually low activity
    * unusually high activity
    * out of range 
    * back in range 
  * Activity 
    * changepoint in activity type  
* Access to Howz platform for Everon
* Access to Howz platform for Everon customers (very unlikely)