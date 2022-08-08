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


# Campos medios -----------------------------------------------------------
# Esta es estática. Sólo se corre una vez.

if (!file.exists(gl$plots$sam_campos)) {
  sam_campos <- fread(gl$sam_file) %>%
    melt(id.vars = c("lev", "lon", "lat")) %>%
    .[, variable := factor_sam(variable)] %>%
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

files <- rev(sort(list.files(here::here("../data/sam"), full.names = TRUE)))
# Grafico los últimos 2 meses
n <- 12
files <- files[seq_len(n)]

sam <- rbindlist(lapply(files, fread))[, term := factor_sam(term)]

g <- ggplot(sam, aes(as.Date(time), estimate)) +
  geom_braid(aes(ymin = estimate, ymax = 0, fill = estimate > 0)) +
  geom_line(size = 0.2) +
  scale_y_continuous(NULL,breaks = scales::breaks_extended(10)) +
  scale_x_date(NULL, date_labels = "%b\n%d") +
  escala_signo +
  facet_grid(lev ~ term, labeller = labeller(lev = lev.lab),
             scales = "free_y")

ggsave(gl$plots$sam_latest12, g, units = "px", height = 400*3, width = 700*3,
       bg = "white")


# Grafico los últimos 6 meses
files <- rev(sort(list.files(here::here("../data/sam"), full.names = TRUE)))
n <- 6
files <- files[seq_len(n)]

sam <- rbindlist(lapply(files, fread)) %>%
  .[, term := factor_sam(term)]

g <- ggplot(sam, aes(as.Date(time), estimate)) +
  geom_braid(aes(ymin = estimate, ymax = 0, fill = estimate > 0)) +
  geom_line(size = 0.2) +
  scale_y_continuous(NULL,breaks = scales::breaks_extended(10)) +
  escala_signo +
  scale_x_date(NULL, date_labels = "%b\n%d", date_breaks = "1 month") +
  facet_grid(lev ~ term, labeller = labeller(lev = lev.lab))

ggsave(gl$plots$sam_latest6, g, units = "px", height = 400*3, width = 700*3,
       bg = "white")


# Grafico los últimos 3 meses
files <- rev(sort(list.files(here::here("../data/sam"), full.names = TRUE)))
n <- 3
files <- files[seq_len(n)]

sam <- rbindlist(lapply(files, fread)) %>%
  .[, term := factor_sam(term)]

g <- ggplot(sam, aes(as.Date(time), estimate)) +
  geom_braid(aes(ymin = estimate, ymax = 0, fill = estimate > 0)) +
  geom_line(size = 0.2) +
  scale_y_continuous(NULL,breaks = scales::breaks_extended(10)) +
  escala_signo +
  scale_x_date(NULL, date_labels = "%b\n%d", date_breaks = "15 days") +
  facet_grid(lev ~ term, labeller = labeller(lev = lev.lab))

ggsave(gl$plots$sam_latest3, g, units = "px", height = 400*3, width = 700*3,
       bg = "white")
