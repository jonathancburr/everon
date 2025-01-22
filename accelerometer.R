accelerometer <- read_csv("2025-01-10_23-09-10/Accelerometer.csv") |>
  dplyr::mutate(ts = as.POSIXct(time / 1000000000)) |>
  dplyr::mutate(ts_rounded = lubridate::round_date(ts, unit = 'minute')
  )
  
differences <-
  accelerometer |>
  dplyr::mutate(dplyr::across(c(x, y, z), ~ abs(. - dplyr::lag(.)), .names = "{.col}"))
