
factor_sam <- function(x) {
  factor(x, levels = c("sam", "ssam", "asam"), labels = c("SAM", "S-SAM", "A-SAM"))
}

theme_asymsam <- function(base_size = 12) {
  ggplot2::theme_minimal(base_size = base_size) +
    ggplot2::theme(
      legend.position = "bottom",
      legend.box = "vertical",
      strip.text = ggplot2::element_text(size = 12),
      panel.grid = ggplot2::element_line(color = scales::alpha("gray60", 0.5),
                                         size = 0.1),
      panel.ontop = TRUE)
}

geom_coords <- function() {
  list(
    ggplot2::geom_segment(data = data.frame(xstart =  seq(0, 360 - 30, by = 30),
                                   xend =  seq(0, 360 - 30, by = 30),
                                   ystart = -90 + 15,
                                   yend = Inf),
                          ggplot2::aes(x = xstart, xend = xend, y = ystart, yend = yend),
                 size = 0.1, alpha = 0.5),

    ggplot2::geom_hline(yintercept = seq(-90, 0, by = 15), size = 0.1, alpha = 0.5),
    shadowtext::geom_shadowtext(data = data.frame(x = 0, y = seq(-90 + 15, 0, by = 15)),
                                aes(x, y, label = metR::LatLabel(y)), size = 1.5, alpha = 0.7,
                                colour = "black",
                                bg.colour = "white" )
  )
}



geom_qmap <- function(subset = identity, color = "gray50", size = 0.2,
                      fill = NA, wrap = c(0, 360), weighting = 0.7,
                      keep = 0.015, ...) {
  lon <- lat <- group <- NULL
  data <- map_simple(wrap = wrap, keep  = keep, weighting = weighting)

  data <- data %>%
    fortify() %>%
    data.table::as.data.table() %>%
    .[, c("long", "lat", "group")] %>%
    data.table::setnames("long", "lon")
  subset <- purrr::as_mapper(subset)
  data <- subset(data)

  ggplot2::geom_polygon(data = data,
                        ggplot2::aes(lon, lat, group = group),
                        color = color,
                        size = size,
                        fill = fill,
                        ...)
}

map_simple <- function(wrap = c(0, 360), keep = 0.015, weighting = 0.7) {
  map <- maps::map("world", fill = TRUE,
                   col = "transparent", plot = FALSE, wrap = wrap)
  IDs <- vapply(strsplit(map$names, ":"), function(x) x[1],
                "")
  proj <- sp::CRS("+proj=longlat +datum=WGS84")
  map <- maptools::map2SpatialPolygons(map, IDs = IDs,
                                       proj4string = proj)

  if (keep != 1) {
    map <- rmapshaper::ms_simplify(map, keep = keep, weighting = weighting)
  }
  map
}


no_grid <- ggplot2::theme(panel.grid = ggplot2::element_blank())

axis_labs_smol <- ggplot2::theme(axis.text = ggplot2::element_text(size = 6))

geom_contour_tanaka2 <- purrr::partial(metR::geom_contour_tanaka, range = c(0.01, 0.3))

