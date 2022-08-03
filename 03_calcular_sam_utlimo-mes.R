source(here::here("helpers/globals.R"))
source(here::here("helpers/helpers.R"))

today <- Sys.Date()
last_month <- lubridate::floor_date(today - lubridate::dmonths(1), "month")
file_out <- make_sam_file(last_month)

if (!file.exists(file_out)) {
  message("Computando SAM para ", format(last_month, "%Y-%m"))
  request <- gl$base_request

  request$year <- lubridate::year(last_month)
  request$month <- lubridate::month(last_month)
  request$day <- seq_len(lubridate::days_in_month(last_month))

  message("Descargando datos...")
  out <- ecmwfr::wf_request(request)

  message("Computando SAMs...")
  sam <- computar_sam(out)


  data.table::fwrite(sam, file_out)
}

message("SAM ya fue computado para ", format(last_month, "%Y-%m"))

