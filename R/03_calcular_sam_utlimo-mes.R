compute_sam <- function(date, fields, climatology, normalisation = NULL) {
  date <- lubridate::floor_date(date, "1 month")
  file <- make_sam_file(date)

  if (date < as.Date("2022-09-01")) {
    # Hardcode early returns not to get all the historical data everty time
    # something trivial changes
    return(file)
  }
  message("Computando SAM para ", format(date, "%Y-%m"))
  request <- gl$base_request
  today <- as.Date(lubridate::now(tzone = "UTC"))

  last_available <- lubridate::floor_date(today - lubridate::ddays(6), "1 day")
  last_day_in_month <- date + lubridate::ddays(lubridate::days_in_month(lubridate::month(date))-1)

  last_day <- min(last_available, last_day_in_month)


  request$year <- NULL
  request$month <- NULL
  request$day <- NULL

  request_format <- "%Y-%m-%d"

  request$date <- paste0(c(format(date, format = request_format),
                           format(last_day, format = request_format)),
                         collapse = "/")

  message("Descargando datos...")
  data_file <- ecmwfr::wf_request(request, user = Sys.getenv("CDSUSER"))

  message("Computando SAMs...")
  sam <- computar_sam(data_file, fields, climatology, normalisation)

  diagnostics(sam)
  message("Guardando...")
  data.table::fwrite(sam, file)

  file
}



diagnostics <- function(sam) {
  # Verificar que el rango de valores sea razonable
  range <- sam[, .(max = max(abs(estimate))), by = .(index, lev)] %>%
    .[, any(max > 100)]

  # Verificar que sam = asam +  ssam
  sum <- sam %>%
    data.table::dcast(lev + time ~ index, value.var = "estimate") %>%
    .[, any(abs(sam - asam - ssam) > 1e-10)]

  failed <- c("Se fue de rango" = range,
              "SAM != ASAM + SSAM" = sum)

  if (any(failed)) {
    text <- paste(names(failed)[failed], collapse = "\n")
    stop(text)
  }

  invisible(TRUE)
}



write_monthly <- function(sam) {
  sam %>%
    .[-1] %>%   # Last month is never complete.
    lapply(data.table::fread) %>%
    data.table::rbindlist() %>%
    .[, .(mean_estimate = mean(estimate),
          mean_r.squared = mean(r.squared)),
      by = .(lev, index, time = lubridate::floor_date(time, "1 month"))] %>%
    data.table::fwrite(gl$archivos$sam_monthly)
  gl$archivos$sam_monthly
}



write_level <- function(sam) {
  save <- function(data, lev) {
    file <-  file.path(gl$dirs$daily_lev, paste0("sam_", lev, "hPa.csv"))
    data.table::fwrite(data, file)
    file
  }

  files <- sam %>%
    lapply(data.table::fread) %>%
    data.table::rbindlist() %>%
    .[order(lev, time, index)] %>%
    split(by = "lev") %>%
    purrr::imap_chr(save)

  files
}

