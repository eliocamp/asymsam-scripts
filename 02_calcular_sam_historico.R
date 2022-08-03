
source(here::here("helpers/globals.R"))
source(here::here("helpers/helpers.R"))
keyring::keyring_unlock(keyring = "ecmwfr", password = Sys.getenv("KEYPASS"))


dates <- seq(as.Date("1959-01-01"), as.Date("2022-06-1"), "1 month")


future::plan("multicore", workers = 10)
sams <- furrr::future_map_chr(dates, function(date) {
  file_out <- file.path(here::here("sam"),
                        paste0(format(date, "%Y-%m-%d"), "_sam.csv"))

  if (!file.exists(file_out)) {
    request <- gl$base_request
    request$year <- lubridate::year(date)
    request$month <- lubridate::month(date)
    request$day <- seq_len(lubridate::days_in_month(date))

    out <- ecmwfr::wf_request(request, user = Sys.getenv("CDSUSER"))
    sam <- computar_sam(out)

    data.table::fwrite(sam, file_out)
  }

  file_out
})


