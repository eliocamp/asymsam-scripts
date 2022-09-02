library(ecmwfr)
library(metR)
library(magrittr)
library(data.table)


source(here::here("helpers/globals.R"))
source(here::here("helpers/login.R"))

# Para cada nivel

levels <- gl$levels

# Descargar los datos -----------------------------------------------------
message("Descargando datos necesarios..")


path <- here::here("../data/era5")
dir.create(path, showWarnings = FALSE, recursive = TRUE)

dates <- seq(as.Date("1959-01-01"), as.Date("2022-07-1"), "1 month")

make_filename <- function(level) {
  paste0("era5_", level, ".nc")
}

requests <- lapply(levels, function(level) {
  file <- make_filename(level)

  if (file.exists(file.path(path, file))) {
    return(NULL)
  }

  request <- gl$base_request
  request$day <- request$month <- request$year <- NULL
  request$date <- paste0(format(range(dates), "%Y-%m-%d"), collapse = "/")
  request$pressure_level <- level
  request$target <- file

  request
})

to_download <- Filter(Negate(is.null), requests)

if (length(to_download) > 0) {
  outs <- ecmwfr::wf_request_batch(to_download, user = Sys.getenv("CDSUSER"),
                                   workers = 10,
                                   path = path, time_out = 3600*5)
}

message("Listo!")
