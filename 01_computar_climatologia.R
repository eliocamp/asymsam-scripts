library(ecmwfr)
library(metR)
library(magrittr)
library(data.table)


source(here::here("helpers/globals.R"))

# Descargar los datos -----------------------------------------------------

dates <- seq(as.Date(gl$climatology[1]), as.Date(gl$climatology[2]), "1 day")
path <- here::here("datos")

request <- gl$base_request

request$year <- unique(lubridate::year(dates))
request$month <- 1:12
request$day <- 1:31
request$target <- "era5-hgt-daily-clim.nc"

era5_file <- file.path(path, request$target)
if (!file.exists(era5_file)) {
  out <- ecmwfr::wf_request(request, user = Sys.getenv("CDSUSER"),
                            path = path, time_out = 3600*5)
}

# Calcular climatologia diaria --------------------------------------------

strip_year <- function(time) {
  lubridate::year(time) <- 2000
  time
}

hgt <- metR::ReadNetCDF(file.path(path, request$target)) %>%
  setnames(c("level", "latitude", "longitude"), c("lev", "lat", "lon"))

max_k <- 4  # Hay que revisar esto. ¿Determinar el óptimo por crossvalidation?
hgt[, time2 := strip_year(time[1]), by = time]
mean_hgt <- hgt[, .(mean = mean(z)), by = .(lon, lat, lev, time2) ] %>%
  .[order(time2)] %>%
  .[, mean := FilterWave(mean, seq(0, max_k)), by = .(lon, lat, lev)]

data.table::fwrite(mean_hgt, gl$climatologia_file)


# Calcular desvio estandard climatológico del SAM -------------------------

campos <- data.table::fread(gl$sam_file) %>%
  setkey(lev, lon, lat)

hgt <- mean_hgt[hgt, on = .NATURAL] %>%
  .[, `:=`(anom = z - mean,
           z = NULL,
           mean = NULL)] %>%
  setkey(lev, lon, lat)

hgt <- campos[hgt]

sams <- hgt[, rbind(data.table::as.data.table(metR::FitLm(anom, full, weights = cos(lat*pi/180), r2 = TRUE)),
                    data.table::as.data.table(metR::FitLm(anom,  sym, weights = cos(lat*pi/180), r2 = TRUE)),
                    data.table::as.data.table(metR::FitLm(anom, asym, weights = cos(lat*pi/180), r2 = TRUE))),
    by = .(time, lev)] %>%
  .[term != "(Intercept)"]

# Full se normaliza por su desvio
sd <- sams[term == "full", .(norm = sd(estimate)), by = .(term, lev)]


# Asym y sym se normalizan para que full = asym + sym
norm <- sams %>%
  dcast(time + lev ~ term, value.var = "estimate") %>%
  .[, full := full/sd(full), by = lev] %>%
  .[, FitLm(full, asym, sym), by = lev] %>%
  .[term != "(Intercept)"] %>%
  setnames("estimate", "norm") %>%
  .[, norm := 1/norm]

norm <- rbind(sd, norm, use.names = TRUE)


data.table::fwrite(norm, gl$sam_norm_file)
