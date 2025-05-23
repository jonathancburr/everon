require(ggplot2)
require(dplyr)

# Source function 
source("~/LocalRepo/Everon/PGCIRC/PGcpt.R")
source("~/LocalRepo/Everon/PGCIRC/SNcirc.R")
Sys.timezone()

# Define the URL and destination path
url <- "https://howzdata.blob.core.windows.net/upload/influx_export.csv"
destfile <- "/Users/jonathanburr/LocalRepo/Everon/data/everon/influx_export.csv"

# Download the file
download.file(url, destfile, method = "curl")

unique(everon$tags)

start <-  as.POSIXct('2025-04-23 00:00:00')
end <- as.POSIXct('2025-05-02 00:00:00')   
bins <- 96
bin.size <- "15 minutes"
full_time <- data.frame(ts = seq(start, end, by = "1 min")) 


data <- readr::read_csv("/Users/jonathanburr/LocalRepo/Everon/data/everon/influx_export.csv") |>
  dplyr::mutate(ts = as.POSIXct(time/1000)) |>
  dplyr::rename(device = tags) |>
  dplyr::mutate(device = gsub(pattern = 'deviceId=','',device)) |>
  dplyr::filter(device == '05301BE3') |>
  dplyr::select(ts, value) |>
  dplyr::filter(ts >= start) |>
  dplyr::filter(ts < end) |>
  dplyr::right_join(full_time, by = 'ts') |>
  dplyr::mutate(value = tidyr::replace_na(value, 0)) |>
  dplyr::arrange(ts) |>
  dplyr::mutate(bin = lubridate::floor_date(ts, "15 minutes")) |>
  howzfunc::howzcp(5, 2)

nrow(data)

bybin <-
  data |>
  dplyr::group_by(bin) |>
  dplyr::summarise(avgbin = mean(value), maxbin = max(value), iqrbin=IQR(value), sdbin=(sd(value))) |>
  dplyr::mutate(date = as.Date(bin)) |>
  dplyr::group_by(date) |>
  dplyr::mutate(binnum = dplyr::row_number()) |>
  dplyr::arrange(date, binnum)
nrow(bybin)
tail(bybin)

bybin |>
  dplyr::group_by(binnum) |>
  dplyr::summarise(avgbin = mean(avgbin)) |>
  ggplot2::ggplot(aes(binnum, avgbin)) + geom_bar(stat='identity')

dat = data_circ(bybin$avgbin,96)
tail(dat)

results = pgcpt(dat, period.len=bins, minseglen.periodic=4, minseglen.global= 7*96,
                method.periodic="SNcirc", method.global="PELT", max.periodic.cpts=10, penalty.periodic=3*log(length(dat)),
                penalty.global=1*log(nrow(dat)), dist="Normal mean", restrict=TRUE, circData=FALSE)

results$pgcpt.results$Periodic_cpt[[1]]

bybin |>
  dplyr::group_by(binnum) |>
  dplyr::summarise(avgbin = mean(avgbin)) |>
  ggplot2::ggplot(aes(binnum, avgbin)) + geom_bar(stat='identity') +
  ggplot2::geom_vline(xintercept = as.integer(results$pgcpt.results$Periodic_cpt[[1]]-4))

bybin |>
  dplyr::group_by(binnum) |>
  dplyr::summarise(mean = mean(avgbin)) |>
  howzfunc::howzcp(seglen = 8, column = 2) |>
  ggplot(aes(binnum, mean)) + geom_line(stat='identity')


dat = data_circ(data$value,1440)
results = pgcpt(dat, period.len=bins, minseglen.periodic= 240, minseglen.global= 7*1440,
                method.periodic="SNcirc", method.global="PELT", max.periodic.cpts=360, penalty.periodic=3*log(length(dat)),
                penalty.global=1*log(nrow(dat)), dist="Normal mean", restrict=TRUE, circData=FALSE)

results$pgcpt.results$Periodic_cpt[[1]]

