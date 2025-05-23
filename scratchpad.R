require('ggplot2')


test<- 12
data <-readr::read_csv2("/Users/jonathanburr/LocalRepo/Everon/data/everon/to_postgresql.csv") |>
  dplyr::mutate(ts = as.POSIXct(datecreated, tz = 'Europe/London')) 


full_time <- tibble::tibble(ts = seq(min(data$ts), max(data$ts)-60, by = "1 min")) |>
  dplyr::cross_join(tibble::tibble(device_id=(unique(data$device_id))))

data <-
  data |>
  dplyr::right_join(full_time, by = c('ts', 'device_id')) |>
  dplyr::mutate(bin = lubridate::floor_date(ts, unit = '15 minutes')) |>
  dplyr::arrange(ts) |>
  dplyr::mutate(filled = is.na(value)) |>
  dplyr::select(device_id, ts, bin, value, filled) |>
  dplyr::group_by(device_id) |>
  dplyr::mutate(value = zoo::na.locf(value)) |>
  dplyr::mutate(oob_obs = value >= test) 

test_data <-
  data |>
  dplyr::filter(ts > '2025-05-22 23:00:00') |>
  dplyr::filter(ts < '2025-05-23 09:00:00') |>
  dplyr::arrange(device_id, bin) |>
  dplyr::group_by(device_id,bin) |>
  dplyr::summarise(value = mean(value)) |>
  dplyr::arrange(device_id, bin) |> 
  dplyr::mutate(oob = as.integer(value >= test)) |>
  dplyr::mutate(oob_lag = as.integer(value >= test & dplyr::lag(value) >= test)) |>
  dplyr::ungroup()

test_data |>
    ggplot(aes(bin, oob)) +geom_step() +facet_grid(device_id~.) + scale_x_datetime(breaks = scales::date_breaks('1 hour'))

test_data |>
  ggplot(aes(bin, value)) +geom_step() +facet_grid(device_id~.) + scale_x_datetime(breaks = scales::date_breaks('1 hour'))

  data |>
  dplyr::filter(bin > '2025-05-22 23:32:00') |>
  dplyr::filter(bin < '2025-05-23 05:37:00') |>
  dplyr::filter(device_id == 18258) |>  
  dplyr::arrange(device_id, bin) |>
  dplyr::summarise(minutes = sum(oob_obs))

test_data |>
  dplyr::group_by(device_id) |>
  dplyr::summarise(awake = sum(oob))


static_data <-
data |>
  dplyr::filter(ts > as.POSIXct('2025-03-09', tz = 'Europe/London')) |>  
  dplyr::filter(ts < as.POSIXct('2025-03-27', tz = 'Europe/London')) 

static_data |>
  ggplot(aes(ts, value)) + geom_point() + facet_grid(device_id~.) 

static_data |>
  dplyr::group_by(device_id) |>
  dplyr::summarise(mean = mean(value), var = var(value), max(max(value), extr = quantile(x = value, .9999)))


static_data_binned <-
  static_data |>
  dplyr::group_by(device_id,bin) |>
  dplyr::summarise(value = mean(value)) |>
  dplyr::ungroup() 

static_data_binned |>
  ggplot(aes(value)) + geom_density() + 
  facet_grid(device_id~.) 

static_data_binned |>
  dplyr::group_by(device_id, bin) |>
  dplyr::summarise(mean = mean(value)) |>
  ggplot(aes(bin, mean)) +geom_step() + facet_grid(device_id~.)

static_data_binned |>
  dplyr::group_by(device_id) |>
  summarise(mean = mean(value), max = max(value), ext = quantile(value, probs = .9999))

test<-3
static_data_binned |>
  arrange(bin) |>
  dplyr::group_by(device_id) |>
  dplyr::mutate(oob = as.integer(value > test & dplyr::lag(value) > test)) |>
  na.omit() |>
  dplyr::summarise(n=n(), oob=sum(oob), prop = oob/n)
  
cluster_data <-
  data |>
  dplyr::filter(device_id == 18258) |>
  dplyr::filter(!filled) |>
  dplyr::filter(ts >= as.POSIXct('2025-05-21 23:00:00', tz='Europe/London'))  |> 
  group_by(bin) |>
  mutate(median = median(value), mean = mean(value), max=max(value), var=var(value)) |>
  ungroup()

km <- kmeans(x = cluster_data$value,centers = 5)

km_df = tibble(centre = km$centers[,1])|>
  dplyr::mutate(cluster = row_number()) |>
  dplyr::mutate(ordered_cluster = row_number(centre))
  
cluster_data <-
  cluster_data |>
  cbind(cluster = km$cluster) |>
  dplyr::inner_join(km_df, by = 'cluster')


cluster_data |>
ggplot(aes(ts, value, color = as.factor(ordered_cluster))) +geom_point()

cluster_data |>
  dplyr::group_by(bin, ordered_cluster) |>
  dplyr::summarise(n=n()) |>
  ggplot(aes(bin, n, group=ordered_cluster, fill=as.factor(ordered_cluster))) +geom_bar(stat='identity')

cluster_data |>
  ggplot(aes(bin, value, group=ordered_cluster, color=as.factor(ordered_cluster))) +geom_point() +geom_point(aes(ts,median), color='black')

cluster_data |>
  ggplot(aes(y=value, x = bin, group = bin)) +geom_boxplot()


