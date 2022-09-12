library(ggplot2)
library(data.table)
library(ggperiodic)
library(magrittr)
library(metR)
library(ggbraid)

source(here::here("helpers/globals.R"))
source(here::here("helpers/helpers-graficos.R"))

theme_set(theme_asymsam(base_size = 12))
ZeroBreaks <- AnchorBreaks(0, NULL, 0)
lev.lab <- function(x) paste0(x, " hPa")
levs <- c(50, 700)

# Campos medios -----------------------------------------------------------
# Esta es estática. Sólo se corre una vez.

if (!file.exists(gl$plots$sam_campos)) {
  sam_campos <- fread(gl$sam_file) %>%
    .[lev %in% levs] %>%
    melt(id.vars = c("lev", "lon", "lat")) %>%
    .[, variable := factor_sam(variable)] %>%
    .[, value := value/sd(value), by = .(lev, variable)] %>%
    periodic(lon = c(0, 360)) %>%
    ggplot(aes(lon, lat)) +
    geom_contour_fill(aes(z = value), global.breaks = FALSE, breaks = ZeroBreaks) +
    geom_contour_tanaka2(aes(z = value), global.breaks = FALSE, breaks = ZeroBreaks) +
    geom_qmap() +
    geom_coords() +
    scale_x_longitude(labels = NULL) +
    scale_y_latitude(limits = c(NA, -20), labels = NULL) +
    scale_fill_divergent(guide = "none", high = "#731C1F", low = "#323071") +
    coord_polar() +
    facet_grid(lev~variable, labeller = labeller(lev = lev.lab)) +
    axis_labs_smol +
    no_grid +
    theme(panel.spacing = grid::unit(-1, "lines"))

  ggsave(gl$plots$sam_campos, sam_campos, units = "px", height = 500*3, width = 700*3,
         bg = "white")
}


# Serie temporal ----------------------------------------------------------
rojo <- "#c6262e"
azul <- "#0d52bf"
escala_signo <- scale_fill_manual(NULL,
                                  values = c("TRUE" = rojo,
                                             "FALSE" = azul),
                                  guide = "none")

guide_fill <- guide_colorsteps(barwidth =30, barheight = 0.5, frame.colour = "black",
                               even.steps = FALSE, show.limits = FALSE)


files <- rev(sort(list.files(here::here("../data/sam"), full.names = TRUE)))
# Grafico los últimos 2 meses


make_plots <- function(meses = 12) {
  files <- files[seq_len(meses)]

  sam <- rbindlist(lapply(files, fread))[, term := factor_sam(term)]

  if (meses == 12) {
    date_breaks <- waiver()
  } else if (meses == 6) {
    date_breaks <- "1 month"
  } else if (meses == 3) {
    date_breaks <- "15 days"
  }

  g <- sam %>%
    .[lev %in% levs] %>%
    ggplot(aes(as.Date(time), estimate)) +
    geom_braid(aes(ymin = estimate, ymax = 0, fill = estimate > 0)) +
    geom_line(size = 0.2) +
    scale_y_continuous(NULL,breaks = scales::breaks_extended(10)) +
    scale_x_date(NULL, date_labels = "%b\n%d", date_breaks = date_breaks)  +
    escala_signo +
    facet_grid(lev ~ term, labeller = labeller(lev = lev.lab),
               scales = "free_y")

  file <- gl$plots[[paste0("sam_latest", meses)]]
  ggsave(file, g, units = "px", height = 400*3, width = 700*3,
         bg = "white")



  breaks <- seq(0.5, 1, length.out = 8) %>%
    qnorm() %>%
    round(digits = 2)

  breaks <- sort(unique(c(-breaks, breaks)))
  breaks <- breaks[breaks != 0]

  g <- sam %>%
    ggplot(aes(as.Date(time), lev)) +
    geom_contour_fill(aes(z = estimate, fill = stat(level)), breaks = breaks) +
    geom_contour_tanaka2(aes(z = estimate), breaks = breaks) +
    scale_fill_divergent_discretised(NULL, guide = guide_fill,
                                     labels = scales::number_format()) +
    scale_y_level() +
    scale_x_date(NULL, date_labels = "%b\n%d", date_breaks = date_breaks) +
    facet_grid(term~.)

  file <- gl$plots[[paste0("sam_latest", meses, "_vertical")]]
  ggsave(file, g, units = "px", height = 400*3, width = 700*3,
         bg = "white")

}

meses <- c(12, 6, 3)

sink <- lapply(meses, make_plots)
