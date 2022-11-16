download_historical <- function(levels, dates) {
  message("Descargando datos necesarios..")

  path <- gl$dirs$era_historical

  make_filename <- function(level) {
    paste0("era5_", level, ".nc")
  }

  requests <- lapply(levels, function(level) {
    file <- make_filename(level)

    request <- gl$base_request
    request$day <- request$month <- request$year <- NULL
    request$date <- paste0(format(range(dates), "%Y-%m-%d"), collapse = "/")
    request$pressure_level <- level
    request$target <- file

    request
  })


  outs <- file.path(path, lapply(requests, function(x) x[["target"]]))

  to_download <- requests[!file.exists(outs)]

  if (length(to_download) > 0) {
    outs <- ecmwfr::wf_request_batch(to_download, user = Sys.getenv("CDSUSER"),
                                     workers = 10,
                                     path = path, time_out = 3600*5)
  }

  message("Listo!")

  names(outs) <- levels
  outs
}

