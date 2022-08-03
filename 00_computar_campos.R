library(data.table)
library(magrittr)

source(here::here("helpers/globals.R"))
# source(here::here("helpers/login.R"))

request <- list(
  product_type = "monthly_averaged_reanalysis",
  variable = "geopotential",
  pressure_level = gl$levels,
  year = seq(min(lubridate::year(gl$climatology)),
             max(lubridate::year(gl$climatology))),
  month = 1:12,
  time = "00:00",
  format = "netcdf",
  dataset_short_name = "reanalysis-era5-pressure-levels-monthly-means",
  target = "download.nc",
  area = gl$sam_area,
  grid = gl$res
)

out <- ecmwfr::wf_request(request, user = "11343")


set.seed(4)
hgt <- metR::ReadNetCDF(out,
                  subset = list(latitude = c(-90, -20),
                                level = as.list(gl$levels)),
                  vars = c(hgt = "z")) %>%
  normalise_coords() %>%
  .[, hgt := hgt/9.8] %>%
  .[, hgt_a := hgt - mean(hgt), by = .(lon, lat, lev, month(time))]


eof <- hgt %>%
  .[, full := hgt_a*sqrt(cos(lat*pi/180))] %>%
  .[, metR::EOF(full ~ time | lon + lat, n = 1, data = .SD)$right,
    by = lev] %>%
  .[, PC := NULL]

eof[, full := -full]

eof[, c("sym", "asym") := list(mean(full), metR::Anomaly(full)),
    by = .(lat, lev)]

data.table::fwrite(eof, gl$sam_file)

