
compute_sam <- function(date, fields, climatology, normalisation) {
  write(date, "date.txt")
  stopifnot(length(date) == 1)
  file <- make_sam_file(date)

  message("Computando SAM para ", format(date, "%Y-%m"))
  request <- gl$base_request

  request$year <- lubridate::year(date)
  request$month <- lubridate::month(date)
  request$day <- seq_len(lubridate::days_in_month(date))


  message("Descargando datos...")
  data_file <- ecmwfr::wf_request(request, user = Sys.getenv("CDSUSER"))

  message("Computando SAMs...")
  sam <- computar_sam(data_file, fields, climatology, normalisation)

  message("Guardando...")
  data.table::fwrite(sam, file)

  file
}



