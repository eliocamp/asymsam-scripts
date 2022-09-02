# Crea un environment llamado gl donde guardar variables globales
# comunes a todo el sistema de monitoreo.
gl <- list()

gl$climatology <- as.Date(c("1979-01-01", "2000-12-31"))
# gl$levels <- c(700, 50)
gl$levels <- c(1, 2, 3, 5, 7, 10, 20, 30, 50, 70, 100, 125, 150, 175, 200, 225, 250, 300, 350, 400, 450, 500, 550, 600, 650, 700, 750, 775, 800, 825, 850, 875, 900, 925, 950, 975, 1000)
gl$sam_area <- c(-20, 0, -90, 360)
gl$res <- c(2.5, 2.5)


gl$base_request <- list(
  product_type = "reanalysis",
  format = "netcdf",
  variable = "geopotential",
  dataset_short_name = "reanalysis-era5-pressure-levels",
  pressure_level = as.character(gl$levels),
  year = NA,
  month = NA,
  day = NA,
  time = c("00:00"),
  area = gl$sam_area,
  grid = gl$res,
  target = "download.nc"
)



gl$sam_file <- here::here("../data/campos.csv")
gl$climatologia_file <- here::here("../data/climatologia.gz")
gl$sam_norm_file <- here::here("../data/sam_norm.csv")
gl$sam_norm_adjust <- here::here("../data/sam_norm_adjust.csv")
gl$sam_sd_file <- here::here("../data/sam_sd.csv")

gl$plots <- list(
  sam_campos  = here::here("../web/images/plots/fields.png"),

  sam_latest12 = here::here("../web/images/plots/latest12.png"),
  sam_latest12_vertical = here::here("../web/images/plots/latest12-vertical.png"),

  sam_latest6 = here::here("../web/images/plots/latest6.png"),
  sam_latest6_vertical = here::here("../web/images/plots/latest6-vertical.png"),

  sam_latest3 = here::here("../web/images/plots/latest3.png"),
  sam_latest3_vertical = here::here("../web/images/plots/latest3-vertical.png")
)
