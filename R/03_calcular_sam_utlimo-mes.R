compute_sam <- function(date, fields, climatology, normalisation = NULL) {
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
