library(ecmwfr)
library(metR)
library(magrittr)
library(data.table)


source(here::here("helpers/globals.R"))
source(here::here("helpers/helpers.R"))

path <- here::here("../data/era5")

campos <- data.table::fread(gl$sam_file) %>%
  setkey(lev, lon, lat)

adjust <- data.table::fread(gl$sam_norm_adjust) %>%
  setkey(lev, term)

strip_year <- function(time) {
  lubridate::year(time) <- 2000
  time
}
max_k <- 4

levels <- gl$levels

message("Haciendo cálculos... ")
clim_norm <- lapply(levels, function(level) {
  message("   nivel ", level, " hPa... ")
  file <- file.path(path, paste0("era5_", level, ".nc"))


  # Calcular climatologia diaria --------------------------------------------
  message("      climatología")

  hgt <- metR::ReadNetCDF(file, vars = c(hgt = "z")) %>%
    setnames(c("latitude", "longitude"), c("lat", "lon")) %>%
    na.omit() %>%
    .[, time := as.Date(time)] %>%
    .[, expver := NULL]

  hgt[, time2 := strip_year(time[1]), by = time]

  mean_hgt <- hgt %>%
    .[time %between% gl$climatology] %>%
    .[, .(mean = mean(hgt)), by = .(lon, lat, time2) ] %>%
    .[order(time2)] %>%
    .[, mean := FilterWave(mean, seq(0, max_k)), by = .(lon, lat)]

  # Calcular desvio estandard climatológico del SAM -------------------------
  message("      constantes normalizadoras del SAM")
  hgt <- mean_hgt[hgt, on = .NATURAL] %>%
    .[, `:=`(anom = hgt - mean,
             hgt = NULL,
             mean = NULL)] %>%
    .[, lev := level] %>%
    setkey(lev, lon, lat)

  sam <- campos[hgt] %>%
    .[, rbind(
      as.data.table(metR::FitLm(anom, full, weights = cos(lat*pi/180), r2 = TRUE)),
      as.data.table(metR::FitLm(anom, sym, weights = cos(lat*pi/180), r2 = TRUE)),
      as.data.table(metR::FitLm(anom, asym, weights = cos(lat*pi/180), r2 = TRUE))
    ),
    by = .(time, lev)] %>%
    .[term != "(Intercept)"]


  list(mean_hgt = mean_hgt,
       sam = sam)
})

message("Guardando archivos")
mean_hgt <- data.table::rbindlist(lapply(clim_norm, function(x) x[["mean_hgt"]]))
data.table::fwrite(mean_hgt, normalizePath(gl$climatologia_file))

sam <- data.table::rbindlist(lapply(clim_norm, function(x) x[["sam"]]))

# Normalizar usando la cimatologia

sd <- sam[term == "full"] %>%
  .[time %between% gl$climatology] %>%
  .[, .(sd = sd(estimate)), by = .(lev)]

data.table::fwrite(sd, normalizePath(gl$sam_norm_file))



sam <- sam[sd, on = "lev"] %>%
  .[, estimate := estimate/sd] %>%
  .[, sd := NULL] %>%
  adjust[., on = c("lev", "term")] %>%
  .[is.na(norm), norm := 1] %>%
  .[, estimate := estimate/norm] %>%
  .[, norm := NULL]

rounded_date <- lubridate::floor_date(sam$time, "1 month")
months <- unique(rounded_date)

sink <- lapply(months, function(m) {
  file <- make_sam_file(m)
  sam[rounded_date == m] %>%
    fwrite(file)
})

message("Listo!")


