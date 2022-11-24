library(magrittr)
library(ggbraid)

library(ggplot2)

theme_set(theme_asymsam(base_size = 12))
ZeroBreaks <- metR::AnchorBreaks(0, NULL, 0)
lev.lab <- function(x) paste0(x, " hPa")
levs <- c(50, 700)

# Campos medios -----------------------------------------------------------
# Esta es estática. Sólo se corre una vez.

plot_sam_fields <- function(fields, levels = c(50, 700)) {
  sam_campos <- fields %>%
    .[lev %in% levels] %>%
    .[, index := factor_sam(index)] %>%
    .[, field := field/sd(field)/(sqrt(cos(lat*pi/180))), by = .(lev, index)] %>%
    .[lat != -90] %>%
    ggperiodic::periodic(lon = c(0, 360)) %>%
    ggplot(aes(lon, lat)) +
    metR::geom_contour_fill(aes(z = field), global.breaks = FALSE, breaks = ZeroBreaks) +
    geom_contour_tanaka2(aes(z = field), global.breaks = FALSE, breaks = ZeroBreaks) +
    geom_qmap() +
    geom_coords() +
    metR::scale_x_longitude(labels = NULL) +
    metR::scale_y_latitude(limits = c(NA, -20), labels = NULL) +
    metR::scale_fill_divergent(guide = "none", high = "#731C1F", low = "#323071") +
    coord_polar() +
    facet_grid(lev~index, labeller = labeller(lev = lev.lab)) +
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

last_updated <- function(now = lubridate::now(tzone = "UTC")) {
  paste0("Last update: ", as.character(now, format = "%F %R UTC"))
}

plot_lines <- function(files, meses = 12) {
  files <- files[seq_len(meses)]
  sam <- data.table::rbindlist(lapply(files, data.table::fread))[, index := factor_sam(index)]

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
    ggbraid::geom_braid(aes(ymin = estimate, ymax = 0, fill = estimate > 0)) +
    geom_line(size = 0.2) +
    scale_y_continuous(NULL,breaks = scales::breaks_extended(10)) +
    scale_x_date(NULL, date_labels = "%b\n%d", date_breaks = date_breaks,
                 expand = c(0, 0))  +
    escala_signo +
    facet_grid(lev ~ index, labeller = labeller(lev = lev.lab),
               scales = "free_y") +
    labs(caption = last_updated())

  file <- gl$plots[[paste0("sam_latest", meses)]]
  ggsave(file, g, units = "px", height = 400*3, width = 700*3,
         bg = "white")

}

plot_vertical <- function(files, meses = 12) {
  files <- files[seq_len(meses)]

  sam <- data.table::rbindlist(lapply(files, data.table::fread))[, index := factor_sam(index)]

  breaks <- seq(0.5, 1, length.out = 10) %>%
    qnorm() %>%
    round(digits = 2)

  breaks <- sort(unique(c(-breaks, breaks)))
  breaks <- breaks[breaks != 0]


  if (meses == 12) {
    date_breaks <- waiver()
  } else if (meses == 6) {
    date_breaks <- "1 month"
  } else if (meses == 3) {
    date_breaks <- "15 days"
  }

  g <- sam %>%
    ggplot(aes(as.Date(time), lev)) +
    metR::geom_contour_fill(aes(z = estimate, fill = stat(level)), breaks = breaks) +
    geom_contour_tanaka2(aes(z = estimate), breaks = breaks, smooth = 1) +
    metR::scale_fill_divergent_discretised(NULL, guide = guide_fill,
                                     labels = scales::number_format()) +
    metR::scale_y_level(trans = metR::reverselog_trans()) +
    scale_x_date(NULL, date_labels = "%b\n%d", date_breaks = date_breaks,
                 expand = c(0, 0)) +
    facet_grid(index ~ .) +
    labs(caption = last_updated())

  file <- gl$plots[[paste0("sam_latest", meses, "_vertical")]]
  ggsave(file, g, units = "px", height = 400*3, width = 700*3,
         bg = "white")

}

move_to_web <- function(files, to_folder) {

  files <- paste0(normalizePath(files), collapse = " ")
  destination <- paste0("elio.campitelli@portal.cima.fcen.uba.ar:~/wwwuser/asymsam/", to_folder, "/")
  command <- paste("rsync -avz", files, destination)

  system(command)
}
