#!/usr/bin/env Rscript

# This is a helper script to run the pipeline.
# Choose how to execute the pipeline below.
# See https://books.ropensci.org/targets/hpc.html
# to learn about your options.

targets::tar_make()
# targets::tar_make_clustermq(workers = 2) # nolint
# targets::tar_make_future(workers = 2) # nolint


errors <- targets::tar_errored()

if (length(errors) == 0) {
  title <-  "✅ SAM"
  mensaje <- "SAM corrió bien."
  topic <- "elio-logs"
} else {
  title <- "❌ SAM"
  mensaje <-  paste0("Errores:\n  ", paste0(errors, collapse = "\n  "))
  topic <- "elio-critical"
}

httr::POST(paste0("ntfy.sh/", topic),
  body = mensaje,
  httr::add_headers(`X-Title` = title))
