require(ggplot2)

# Source function 
source("~/LocalRepo/Everon/PGCIRC/PGcpt.R")
source("~/LocalRepo/Everon/PGCIRC/SNcirc.R")

# Define the URL and destination path
url <- "https://howzdata.blob.core.windows.net/upload/influx_export.csv"
destfile <- "/Users/jonathanburr/LocalRepo/Everon/data/everon/influx_export.csv"

# Download the file
download.file(url, destfile, method = "curl")

unique(everon$tags)

start <-  as.POSIXct('2025-04-23 00:00:00')
end <- as.POSIXct('2025-05-02 04:00:00')   
bins <- 96
bin.size <- "15 minutes"
full_time <- data.frame(ts = seq(start, end, by = "1 min")) 


data <- readr::read_csv("/Users/jonathanburr/LocalRepo/Everon/data/everon/influx_export.csv") |>
  dplyr::mutate(ts = as.POSIXct(time/1000)) |>
  dplyr::rename(device = tags) |>
  dplyr::mutate(device = gsub(pattern = 'deviceId=','',device)) |>
  dplyr::filter(device == '05301BE3') |>
  dplyr::select(ts, value) |>
  #dplyr::filter(ts >= start) |>
  #dplyr::filter(ts < end) |>
  dplyr::right_join(full_time, by = 'ts') |>
  dplyr::mutate(value = tidyr::replace_na(value, 0)) |>
  dplyr::arrange(ts) |>
  dplyr::mutate(bin = lubridate::floor_date(ts, "15 minutes")) |>
  howzfunc::howzcp(5, 2)

data |>
  ggplot(aes(ts,value)) + geom_point() +
  geom_segment(data = steps, aes(x = tes_start_time, xend = tes_end_time, 
                                 y = 40, yend = 40), color = 'red', size = 2)

bybin <-
  data |>
  dplyr::group_by(bin) |>
  dplyr::summarise(avgbin = mean(value), maxbin = max(value), iqrbin=IQR(value), sdbin=(sd(value))) |>
  dplyr::mutate(date = substr(bin,1,10)) |>
  dplyr::group_by(date) |>
  dplyr::mutate(binnum = dplyr::row_number())

bybin |>
#dplyr::filter(sdbin > 0) |>  
dplyr::filter(bin >= as.POSIXct('2025-05-01 00:00:00')) |>
dplyr::filter(bin < as.POSIXct('2025-05-03 00:00:00')) |>    
ggplot(aes(bin, maxbin)) + geom_point() + geom_label(aes(label=format(bin,"%H:%M")))

bybin |>
  dplyr::group_by(binnum) |>
  dplyr::summarise(avgbin = mean(avgbin)) |>
  ggplot2::ggplot(aes(binnum, avgbin)) + geom_bar(stat='identity')

dat = data_circ(bybin$avgbin,96)

results = pgcpt(dat, period.len=bins, minseglen.periodic=8, minseglen.global= 7*96,
                method.periodic="SNcirc", method.global="PELT", max.periodic.cpts=10, penalty.periodic=3*log(length(dat)),
                penalty.global=1*log(nrow(dat)), dist="Normal mean", restrict=TRUE, circData=FALSE)

bybin |>
  dplyr::group_by(binnum) |>
  dplyr::summarise(avgbin = mean(avgbin)) |>
  ggplot2::ggplot(aes(binnum, avgbin)) + geom_bar(stat='identity') +
  ggplot2::geom_vline(xintercept = as.integer(results$pgcpt.results$Periodic_cpt[[1]]))



data |>
  dplyr::filter(ts >= as.POSIXct('2025-04-26 00:00:00')) |>
  dplyr::filter(ts < as.POSIXct('2025-04-27 00:00:00')) |>  
  ggplot(aes(ts, mean)) + geom_point() +
  geom_segment(data = steps, aes(x = tes_start_time, xend = tes_end_time, 
                                 y = steps_per_second_scaled, yend = steps_per_second_scaled), color = 'red', size = 2)


data |>
  dplyr::filter(ts >= as.POSIXct('2025-05-01 06:00:00')) |>
  dplyr::filter(ts < as.POSIXct('2025-05-03 00:00:00')) |>  
  dplyr::filter(value > 0) |>
  ggplot(aes(ts, value)) + geom_point() + 
  geom_step(aes(ts,mean), color = 'red') +
  geom_label(aes(label=format(ts,"%H:%M")))
