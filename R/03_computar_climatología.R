compute_climatology_sd <- function(files, fields, date_range = gl$climatology) {
  levels <- as.integer(names(files))
  max_k <- 4
  # Computa la climatología.
  res <- purrr::map2(files, levels, function(file, level) {
    message("   nivel ", level, " hPa... ")

    # Calcular climatologia diaria --------------------------------------------
    message("      climatología")

    hgt <- metR::ReadNetCDF(file, vars = c(hgt = "z"),
                            subset = list(expver = 1,
                                          time = date_range)) %>%
      data.table::setnames(c("latitude", "longitude"),
                           c("lat",      "lon")) %>%
      .[, time := as.Date(time)]

    hgt[, time2 := strip_year(time[1]), by = time]

    mean_hgt <- hgt %>%
      .[, .(mean = mean(hgt)), by = .(lon, lat, time2) ] %>%
      .[order(time2)] %>%
      .[, mean := metR::FilterWave(mean, seq(0, max_k)), by = .(lon, lat)] %>%
      .[, lev := level] %>%
      data.table::setkey(lev, lon, lat)

    # Calcular desvio estandard climatológico del SAM -------------------------
    message("      constantes normalizadoras del SAM")
    hgt <- mean_hgt[hgt, on = .NATURAL] %>%
      .[, `:=`(anom = hgt - mean,
               hgt  = NULL,
               mean = NULL)] %>%
      .[, lev := level] %>%
      data.table::setkey(lev, lon, lat)

    sd <- fields[index == "sam"] %>%
      .[hgt, on = c("lon", "lat", "lev")] %>%
      .[, metR::FitLm(anom, field),
        by = .(time, lev)] %>%
      .[term != "(Intercept)"] %>%
      .[, .(sd = sd(estimate)), by = .(lev)]

    list(mean_hgt = mean_hgt,
         sd = sd)
  })

  climatology <- data.table::rbindlist(lapply(res, function(x) x[["mean_hgt"]]))
  sd <- data.table::rbindlist(lapply(res, function(x) x[["sd"]]))
  list(climatology = climatology,
       sd = sd)
}

write_climatology <- function(climatology) {
  data.table::fwrite(climatology, gl$archivos$climatologia)
  gl$archivos$climatologia
}

compute_normalisation <- function(sd, relvariance) {
  # Normalizar usando la climatología
  relvariance[sd, on = "lev"] %>%
    .[, norm := sd/rel_variance] %>%
    .[, sd := NULL] %>%
    .[, rel_variance := NULL] %>%
    .[]
}

write_normalisation <- function(normalisation) {
  data.table::fwrite(normalisation, gl$archivos$ajuste)
}
