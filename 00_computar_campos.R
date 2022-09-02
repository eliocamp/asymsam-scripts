library(data.table)
library(magrittr)

source(here::here("helpers/globals.R"))
source(here::here("helpers/helpers.R"))
source(here::here("helpers/login.R"))

path <- here::here("../data/era-monthly")
dir.create(path, FALSE, TRUE)

levels <- gl$levels
requests <- lapply(levels, function(level) {
  file <- paste0(level, "_download.nc")
  if (file.exists(file.path(path, file))) {
    return(NULL)
  }
  list(
    product_type = "monthly_averaged_reanalysis",
    variable = "geopotential",
    pressure_level = level,
    year = seq(min(lubridate::year(gl$climatology)),
               max(lubridate::year(gl$climatology))),
    month = 1:12,
    time = "00:00",
    format = "netcdf",
    dataset_short_name = "reanalysis-era5-pressure-levels-monthly-means",
    target = file,
    area = gl$sam_area,
    grid = gl$res
  )
})

to_download <- Filter(Negate(is.null), requests)

if (length(to_download) > 0) {
  outs <- ecmwfr::wf_request_batch(requests,
                                   user = Sys.getenv("CDSUSER"),
                                   workers = 10,
                                   path = path)
}

eof <- lapply(levels, function(lev) {
  file <- file.path(path, paste0(lev, "_download.nc"))
  set.seed(4)
  hgt <- metR::ReadNetCDF(file,
                          subset = list(latitude = c(-90, -20)),
                          vars = c(hgt = "z")) %>%
    normalise_coords() %>%
    .[, hgt := hgt] %>%
    .[, hgt_a := hgt - mean(hgt), by = .(lon, lat, month(time))]


  eof <- hgt %>%
    .[, full := hgt_a*sqrt(cos(lat*pi/180))] %>%
    metR::EOF(full ~ time | lon + lat, n = 1, data = .) %>%
    .$right %>%
    # metR::denormalise("right") %>%
    .[, PC := NULL]
  eof[, lev := lev]

  eof[]
})

eof <- data.table::rbindlist(eof)


# Asegura consistencia en el signo
# 1. Asegurase de que latitudes polares la anomalía en niveles bajos
# sea negativa
correct <- eof[lev == max(lev)] %>%
  .[lat < -70, sign(weighted.mean(-full, cos(lat*pi/180)))]
eof[, full := full*correct]

# 2. Asegurar continuidad entre niveles
# Esto seguro que no es necesario, pero por las dudas me aseguro
# de que esté ordenado. Sino está todo ordenado,
# el cálculo de la correlación no anda.
eof <- eof[order(lon, lat, lev)]
levels <- sort(levels, decreasing = TRUE)
for (l in seq_along(levels)[-1]) {
  correct <- sign(eof[, cor(full[lev == levels[l]], full[lev == levels[l-1]])])
  eof[lev == levels[l], full := full * correct]
}


eof[, c("sym", "asym") := list(mean(full), metR::Anomaly(full)),
    by = .(lat, lev)]

data.table::fwrite(eof, gl$sam_file)



norm <- eof %>%
  melt(id.vars = c("lon", "lat", "lev"), variable.name = "term") %>%
  .[, .(norm = wvar(value, cos(lat*pi/180))), by = .(term, lev)] %>%
  dcast(lev ~ term, value.var = "norm") %>%
  .[, .(sym = full/sym,
        asym = full/asym,
        full = 1), by = lev] %>%
  melt(id.vars = c("lev"), variable.name = "term", value.name = "norm")
data.table::fwrite(norm, normalizePath(gl$sam_norm_adjust))
