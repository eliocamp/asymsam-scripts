

get_dates <- function(today = Sys.Date()) {
  last_month <- lubridate::floor_date(today - lubridate::dmonths(1), "month")
  dates <- seq(as.Date("1959-01-01"), as.Date(last_month), "1 month")
  rev(dates)[1:12]
}



make_sam_file <- function(date) {
  file.path(gl$dirs$indices,
            paste0(format(date, "%Y-%m-%d"), "_sam.csv"))
}



decode_sam_file <- function(file) {
  as.POSIXct(basename(file), tz = "UTC",
             format = paste0("%Y-%m-%d_sam.csv"))
}



normalise_coords <- function(data,
                             rules =  list(lev = c("level"),
                                           lat = c("latitude"),
                                           lon = c("longitude", "long"),
                                           time = c("date")),
                             extra = list()) {
  rules <- c(rules, extra)

  for (f in seq_along(rules)) {
    old <- colnames(data)[colnames(data) %in% rules[[f]]]

    if (length(old) != 0) {
      data.table::setnames(data,
                           old,
                           names(rules)[[f]],
                           skip_absent = TRUE)
    }
  }
  return(invisible(data))
}


strip_year <- function(time) {
  lubridate::year(time) <- 2000
  time
}


computar_sam <- function(file, fields, climatology, normalisation) {
  hgt <- metR::ReadNetCDF(file, vars = c(hgt = "z"),
                          subset = list(latitude = c(-90, -20))) %>%
    normalise_coords() %>%
    .[, time2 := data.table::as.IDate(strip_year(time[1])), by = time]


  sam <- climatology[hgt, on = c("lon", "lat", "lev", "time2")] %>%
    .[, anom := hgt - mean] %>%
    fields[., on = c("lon", "lat", "lev"), allow.cartesian = TRUE] %>%
    .[, metR::FitLm(anom, field, r2 = TRUE),
      by = .(index, lev, time)] %>%
    .[term != "(Intercept)"] %>%
    .[, term := NULL]

  if (!is.null(normalisation)) {
    sam <- sam %>%
      normalisation[., on = c("lev", "index")] %>%
      .[, estimate := estimate*norm] %>%
      .[, norm := NULL]
  }


  sam[]
}



wvar <- function(x, w) {
  w <- w/sum(w)
  xm <- weighted.mean(x, w)
  sum(w * (x - xm)^2)

}
