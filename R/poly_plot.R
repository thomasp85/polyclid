#' @importFrom graphics plot plot.new plot.window Axis title box
#' @export
plot.polyclid_geometry <- function(x, y, xlim = NULL, ylim = NULL, add = FALSE, axes = TRUE, frame.plot = axes, ...) {
  grDevices::dev.hold()
  on.exit(grDevices::dev.flush())
  if (!add) {
    if (is.null(xlim) || is.null(ylim)) {
      cbox <- as_bbox(x)
      cbox <- as.matrix(sum(cbox, na.rm = TRUE))
      if (is.null(xlim)) xlim <- cbox[c(1, 3)]
      if (is.null(ylim)) ylim <- cbox[c(2, 4)]
      if (xlim[1] == xlim[2]) xlim <- xlim + c(-0.5, 0.5)
      if (ylim[1] == ylim[2]) ylim <- ylim + c(-0.5, 0.5)
    }
    plot.new()
    plot.window(xlim, ylim, asp = 1)
  }
  euclid_plot(x, ...)
  if (axes) {
    Axis(xlim, side = 1)
    Axis(ylim, side = 2)
    title(xlab = "x", ylab = "y")
  }
  if (frame.plot) {
    box()
  }
  invisible(NULL)
}

#' @importFrom euclid euclid_plot
#' @export
euclid_plot.polyclid_polygon <- function(x, ..., force_valid = TRUE, mapping_plane = "z") {
  pars <- lapply(list2(...), function(p) rep_len(p, length(x)))
  pars$y <- NULL
  if (!is.null(pars$rule) && any(pars$rule != "winding")) {
    cli_warn("ignoring {.arg rule} as polyclid polygons are defined by winding")
    pars$rule <- NULL
  }
  plot_lim <- graphics::par("usr")
  plot_lim <- matrix(plot_lim[c(1,2,2,1,3,3,4,4)], ncol = 2)
  for (i in seq_along(x)) {
    if (is.na(x[i])) next
    poly <- x[i]
    if (force_valid) {
      poly <- try(make_valid(poly))
      if (is.na(poly)) {
        cli_warn("Ignoring invalid polygon")
        next
      }
    }
    p <- lapply(pars, `[`, i)
    coords <- as.matrix(poly)
    holes <- cardinality(poly, split = TRUE)
    if (!is_bounded(poly)) {
      coords <- rbind(plot_lim, coords)
      holes <- c(4, holes)
    }
    holes <- rep(seq_along(holes), holes)
    index <- unlist(lapply(split(seq_len(nrow(coords)), holes), function(i) c(NA_integer_, i)))[-1]
    coords <- coords[index, ]
    inject(graphics::polypath(coords, !!!p))
  }
  invisible(NULL)
}

#' @importFrom euclid euclid_plot
#' @export
euclid_plot.polyclid_polyline <- function(x, ..., mapping_plane = "z") {
  pars <- lapply(list2(...), function(p) rep_len(p, length(x)))
  pars$y <- NULL
  if (!is.null(pars$type) && any(pars$type != "l")) {
    cli_warn("ignoring {.arg type} as polyclid polylines are drawn as lines")
    pars$type <- NULL
  }
  for (i in seq_along(x)) {
    p <- lapply(pars, `[`, i)
    coords <- as.matrix(x[i])
    inject(graphics::lines(coords, !!!p))
  }
  invisible(NULL)
}

#' @importFrom euclid euclid_plot
#' @export
euclid_plot.polyclid_polygon_set <- function(x, ..., mapping_plane = "z") {
  n <- n_polygons(x)
  pars <- lapply(list2(...), function(p) rep(rep_len(p, length(x)), n))
  polygons <- as_polygon(x)
  euclid_plot(polygons, !!!pars)
}

#' @importFrom euclid euclid_plot
#' @export
euclid_plot.polyclid_polyline_set <- function(x, ..., mapping_plane = "z") {
  n <- n_polylines(x)
  pars <- lapply(list2(...), function(p) rep(rep_len(p, length(x)), n))
  polylines <- as_polyline(x)
  euclid_plot(polylines, !!!pars)
}

#' @importFrom euclid euclid_grob
#' @importFrom grid pathGrob gpar
#' @export
euclid_grob.polyclid_polygon <- function(x, force_valid = TRUE, ..., unit = "native", name = NULL, gp = gpar(), vp = NULL, mapping_plane = "z") {
  x <- x[!is.na(x)]
  if (force_valid) {
    x <- make_valid(x)
    if (anyNA(x)) {
      cli_warn("Ignoring invalid polygon")
      x <- x[!is.na(x)]
    }
  }
  coords <- as.matrix(x)
  path <- cardinality(x)
  path <- rep(seq_along(path), path)
  hole <- cardinality(x, split = TRUE)
  hole <- rep(seq_along(hole), hole)
  pathGrob(
    x = coords[, 1],
    y = coords[, 2],
    id = hole,
    pathId = path,
    rule = "evenodd",
    default.units = unit,
    name = name,
    gp = gp,
    vp = vp
  )
}

#' @importFrom euclid euclid_grob
#' @importFrom grid polylineGrob gpar
#' @export
euclid_grob.polyclid_polyline <- function(x, ..., unit = "native", name = NULL, gp = gpar(), vp = NULL, mapping_plane = "z") {
  coords <- as.matrix(x)
  line <- cardinality(x)
  line <- rep(seq_along(line), line)
  polylineGrob(
    x = coords[, 1],
    y = coords[, 2],
    id = line,
    default.units = unit,
    name = name,
    gp = gp,
    vp = vp
  )
}

#' @export
euclid_grob.polyclid_polygon_set <- function(x, ..., unit = "native", name = NULL, gp = gpar(), vp = NULL, mapping_plane = "z") {
  n <- n_polygons(x)
  for (par in names(gp)) {
    if (length(gp[[par]]) > 1) {
      gp[[par]] <- rep(rep_len(gp, length(x)), n)
    }
  }
  p <- as_polygon(x)
  euclid_grob(p, ..., unit = unit, name = name, gp = gp, vp = vp)
}

#' @export
euclid_grob.polyclid_polyline_set <- function(x, ..., unit = "native", name = NULL, gp = gpar(), vp = NULL, mapping_plane = "z") {
  n <- n_polylines(x)
  for (par in names(gp)) {
    if (length(gp[[par]]) > 1) {
      gp[[par]] <- rep(rep_len(gp, length(x)), n)
    }
  }
  p <- as_polyline(x)
  euclid_grob(p, ..., unit = unit, name = name, gp = gp, vp = vp)
}
