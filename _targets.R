# Created by use_targets().
# Follow the comments below to fill in this target script.
# Then follow the manual to check and run the pipeline:
#   https://books.ropensci.org/targets/walkthrough.html#inspect-the-pipeline # nolint

# Load packages required to define the pipeline:
library(targets)
# library(tarchetypes) # Load other packages as needed. # nolint

# Set target options:
tar_option_set(
  packages = c("tibble"), # packages that your targets need to run
  format = "rds" # default storage format
  # Set other options as needed.
)

# tar_make_clustermq() configuration (okay to leave alone):
options(clustermq.scheduler = "multicore")

# tar_make_future() configuration (okay to leave alone):
future::plan("multicore", workers = 5)

# Run the R scripts in the R/ folder with your custom functions:
tar_source()
# source("other_functions.R") # Source other scripts as needed. # nolint

dates_historical <- seq(gl$climatology[1], gl$climatology[2], "1 month")

today <- lubridate::now(tzone = "UTC")
# Replace the target list below with your own:
list(
  tar_target(
    name = monthly_files,
    command = download_hgt_monthly(levels = gl$levels, date_range = gl$climatology)
  ),
  tar_target(
    name = fields,
    command = compute_fields(monthly_files)
  ),

  tar_target(
    name = plot_fields,
    command = plot_sam_fields(fields),
    format = "file"
  ),

  tar_target(
    name = fields_file,
    command = write_fields(fields),
    format = "file"
  ),
  tar_target(
    name = relvariance,
    command = compute_relvariance(fields)
  ),

  tar_target(
    name = historical_files,
    command = download_historical(levels = gl$levels, dates = dates_historical)
  ),

  tar_target(
    name = climatology_sd,
    command = compute_climatology_sd(files = historical_files, fields = fields,
                                     date_range = gl$climatology)
  ),

  tar_target(
    name = climatology_file,
    command = write_climatology(climatology_sd$climatology),
    format = "file"
  ),

  tar_target(
    name = normalisation,
    command = compute_normalisation(climatology_sd$sd, relvariance)
  ),

  tar_target(
    name = normalisation_file,
    command = write_normalisation(normalisation),
    format = "file"
  ),

  tar_target(
    name = dates,
    command = get_dates(today = today)
  ),

  tar_target(
    name = sam,
    command = compute_sam(dates, fields,
                          climatology_sd$climatology,
                          normalisation),
    pattern = map(dates),
    format = "file"
  ),

  tar_target(
    name = sam_monthly,
    command = write_monthly(sam),
    format = "file",
    cue = tar_cue("always")
  ),

  tar_target(
    name = move_monthly_command,
    command = move_to_web(sam_monthly,
                          "data")
  ),

  tar_target(
    name = sam_level,
    command = write_level(sam),
    format = "file",
    cue = tar_cue("always")
  ),

  tar_target(
    name = move_level_command,
    command = move_to_web(sam_level,
                          "data/sam_level")
  ),

  tar_target(
    name = meses,
    command = c(3, 6, 12)
  ),

  tar_target(
    name = plot_lines_file,
    command = plot_lines(sam, meses),
    pattern = map(meses),
    format = "file",
    cue = tar_cue("always")
  ),

  tar_target(
    name = plot_vertical_file,
    command = plot_vertical(sam, meses),
    pattern = map(meses),
    format = "file",
    cue = tar_cue("always")
  )

)
