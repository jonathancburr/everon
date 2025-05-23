

url <- "https://howzdata.blob.core.windows.net/upload/influx_export.csv"
destfile <- "/Users/jonathanburr/LocalRepo/Everon/data/everon/influx_export.csv"
download.file(url, destfile, method = "curl")
knitr::opts_chunk$set(fig.width = 10, fig.height = 5)

devices <-
  tibble::tibble(external_id = c('05301BE1','05301BE3'),
         device_id = c(18268,18258),
         hid = c('07OWdScvPuoOEOoMnlTgIjhwr1r9t888', '07OWdScvPuoOEOoMnlTgIjhwr1r9t999'))

data <-
  readr::read_csv("/Users/jonathanburr/LocalRepo/Everon/data/everon/influx_export.csv") |>
  dplyr::rename(datecreated = time) |>
  dplyr::mutate(datecreated = datecreated / 1000) |>
  dplyr::rename(external_id = tags) |>
  dplyr::mutate(external_id = gsub(pattern = 'deviceId=','',external_id)) |>
  dplyr::inner_join(devices, by='external_id') |>
  dplyr::select(device_id,hid,datecreated,value)

write.csv2(data, "/Users/jonathanburr/LocalRepo/Everon/data/everon/to_postgresql.csv",row.names = FALSE)
