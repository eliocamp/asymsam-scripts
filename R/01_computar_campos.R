download_hgt_monthly <- function(levels, date_range) {
  path <- gl$dirs$era_mensual
  requests <- lapply(levels, function(level) {
    file <- paste0(level, "_download.nc")

    list(
      product_type = "monthly_averaged_reanalysis",
      variable = "geopotential",
      pressure_level = level,
      year = seq(min(lubridate::year(date_range)),
                 max(lubridate::year(date_range))),
      month = 1:12,
      time = "00:00",
      format = "netcdf",
      dataset_short_name = "reanalysis-era5-pressure-levels-monthly-means",
      target = file,
      area = gl$sam_area,
      grid = gl$res
    )
  })

  outs <- file.path(path, lapply(requests, function(x) x[["target"]]))

  to_download <- requests[!file.exists(outs)]

  if (length(to_download) > 0) {
    outs <- ecmwfr::wf_request_batch(requests,
                                     user = Sys.getenv("CDSUSER"),
                                     workers = 10,
                                     path = path)
  }
  names(outs) <- levels
  outs
}


compute_fields <- function(files) {
  eof <- furrr::future_map(files, function(file) {
    set.seed(4)
    hgt <- metR::ReadNetCDF(file,
                            subset = list(latitude = c(-90, -20)),
                            vars = c(hgt = "z")) %>%
      normalise_coords() %>%
      .[, hgt := hgt] %>%
      .[, hgt_a := hgt - mean(hgt), by = .(lon, lat, data.table::month(time))]


    eof <- hgt %>%
      .[, sam := hgt_a*sqrt(cos(lat*pi/180))] %>%
      metR::EOF(sam ~ time | lon + lat, n = 1, data = .) %>%
      .$right %>%
      # metR::denormalise("right") %>%
      .[, PC := NULL]

    eof[]
  })

  eof <- data.table::rbindlist(eof, idcol = "lev") %>%
    .[, lev := as.integer(lev)]

  # Asegura consistencia en el signo
  # 1. Asegurase de que latitudes polares la anomalía en niveles bajos
  # sea negativa
  correct <- eof[lev == max(lev)] %>%
    .[lat < -70, sign(weighted.mean(-sam, cos(lat*pi/180)))]
  eof[, sam := sam*correct]

  # 2. Asegurar continuidad entre niveles
  # Esto seguro que no es necesario, pero por las dudas me aseguro
  # de que esté ordenado. Sino está todo ordenado,
  # el cálculo de la correlación no anda.
  eof <- eof[order(lon, lat, lev)]
  levels <- sort(unique(eof$lev), decreasing = TRUE)
  for (l in seq_along(levels)[-1]) {
    correct <- sign(eof[, cor(sam[lev == levels[l]], sam[lev == levels[l-1]])])
    eof[lev == levels[l], sam := sam * correct]
  }


  eof[, c("ssam", "asam") := list(mean(sam), metR::Anomaly(sam)),
             by = .(lat, lev)] %>%
    data.table::melt(id.vars = c("lon", "lat", "lev"), value.name = "field", variable.name = "index")
}

write_fields <- function(eof) {
  data.table::fwrite(eof, gl$archivos$campos_sam)
  gl$archivos$campos_sam
}

compute_relvariance <- function(eof) {
  eof %>%
    .[, .(variance = var(field)), by = .(index, lev)] %>%
    data.table::dcast(lev ~ index, value.var = "variance") %>%
    .[, .(asam = asam/sam,
          ssam = ssam/sam,
          sam = 1), by = lev] %>%
    data.table::melt(id.vars = c("lev"), variable.name = "index", value.name = "rel_variance")
}
