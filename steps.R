library(xml2)
library(dplyr)
library(lubridate)

setwd("/Users/jonathanburr/LocalRepo/Everon/data/apple_health_export/")

# Load the XML file
doc <- read_xml("/Users/jonathanburr/LocalRepo/Everon/data/apple_health_export/export.xml")
records <- xml_find_all(doc, ".//Record[@type='HKQuantityTypeIdentifierStepCount']")


# Step Count
steps_records <- xml_find_all(doc, ".//Record[@type='HKQuantityTypeIdentifierStepCount']")
steps_df <- tibble(
  type = "StepCount",
  start_time = ymd_hms(xml_attr(steps_records, "startDate")),
  end_time = ymd_hms(xml_attr(steps_records, "endDate")),
  value = as.character(xml_attr(steps_records, "value"))
) 

# Heart Rate
heart_records <- xml_find_all(doc, ".//Record[@type='HKQuantityTypeIdentifierHeartRate']")
heart_df <- tibble(
  type = "HeartRate",
  start_time = ymd_hms(xml_attr(heart_records, "startDate")),
  end_time = ymd_hms(xml_attr(heart_records, "endDate")),
  value = as.character(xml_attr(heart_records, "value"))
) |>
  dplyr::filter(start_time >= as.POSIXct('2025-04-23 00:00:00'))

# Sleep Analysis 
sleep_records <- xml_find_all(doc, ".//Record[@type='HKCategoryTypeIdentifierSleepAnalysis']")
sleep_df <- tibble(
  type = "Sleep",
  start_time = ymd_hms(xml_attr(sleep_records, "startDate")),
  end_time = ymd_hms(xml_attr(sleep_records, "endDate")),
  value = xml_attr(sleep_records, "value")) |>
  dplyr::mutate(value = gsub('HKCategoryValueSleepAnalysis','',value))

# Combine all (optional)
combined_df <- bind_rows(steps_df, heart_df, sleep_df) |>
  dplyr::mutate(start_time = as.POSIXct(start_time, tz = 'Europe/London')) |>
  dplyr::mutate(end_time = as.POSIXct(end_time, tz = 'Europe/London')) |>
  dplyr::filter(start_time >= as.POSIXct('2025-04-23 00:00:00'))

# Calculate periods in which Watch has been worn

heart_df <-
  heart_df |>
  dplyr::arrange(start_time) |>
  dplyr::mutate(gap = start_time - dplyr::lag(start_time)) |>
  dplyr::mutate(gap = as.integer(gap))




