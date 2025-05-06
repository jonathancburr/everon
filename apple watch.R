require(ggplot2)

url <- 'https://howzdata.blob.core.windows.net/upload/influx_export.csv'

# Define the URL and destination path
url <- "https://howzdata.blob.core.windows.net/upload/influx_export.csv"
destfile <- "/Users/jonathanburr/LocalRepo/Everon/data/everon/influx_export.csv"

# Download the file
download.file(url, destfile, method = "curl")

unique(everon$tags)

everon <- readr::read_csv("/Users/jonathanburr/LocalRepo/Everon/data/everon/influx_export.csv") |>
  dplyr::mutate(ts = as.POSIXct(time/1000)) |>
  dplyr::rename(device = tags) |>
  dplyr::mutate(date = as.Date(ts))

length(unique(everon$date))

everon |>
  dplyr::filter(ts > as.POSIXct('2025-04-21 10:44:00')) |>
#  dplyr::filter(ts < as.POSIXct('2025-04-21 11:23:00')) |>  
ggplot(aes(ts, value)) + geom_point() + facet_grid(device~.)
  #geom_point(data=sensor_sum, aes(bin, max), color = 'red')


sensor <- read_csv("data/sensor logger/1745228722000/Accelerometer.csv") |>
  dplyr::mutate(ts = as.POSIXct(time / 1e9)) |>
  dplyr::mutate(magnitude = sqrt(z^2 + y^2 +z^2)) |>
  dplyr::mutate(bin = lubridate::floor_date(ts, unit = 'minute')) |>
  dplyr::mutate(delz = z - dplyr::lag(z), dely = y - dplyr::lag(y), delx = dplyr::lag(x)) |>
  dplyr::mutate(sumdel = delz + dely + delx)

sensor_sum <-
sensor |>
  dplyr::group_by(bin) |>
  dplyr::summarise(mean = mean(magnitude), max = max(magnitude))
ggplot(sensor_sum,aes(bin, max)) + geom_point()


