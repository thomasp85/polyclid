#' Sets of polygons
#'
#' A polygon set is a collections of disjoint polygons with holes.
#' Their main use is as intermediary representations during boolean operations
#' and other operations that may result in more than one polygon. A polygon set
#' is always valid as the polygons that are used during construction are
#' validated beforehand. It is important to note that a polygon set created from
#' two or more polygons may be topologically different from the polygons as they
#' will be merged if they touch or overlap (effectively taking the union of
#' them).
#'
#' @param polygons A `polyclid_polygon` vector or a list of these. In the latter
#' case the union of the polygons in each vector is used for the set.
#' @param x A `polyclid_polygon_set` vector
#' @param location A `euclid_point2` vector giving locations
#' @param ... Arguments passed on
#'
#' @return a `polyclid_polygon_set` vector
#'
#' @export
#' @family polygons
#' @family sets
#'
#' @examples
#' # Polygon sets are created from polygons
#' rects <- as_polygon(euclid::iso_rect(
#'   euclid::point(0:1, 0),
#'   euclid::point(1:2, 1)
#' ))
#' circs <- as_polygon(euclid::circle(euclid::point(c(0, 2), 1), 0.3))
#'
#' ps <- polygon_set(c(rects, circs))
#' plot(ps, col = "grey")
#'
#' # If a list of polygons are provided, the polygons in each element are
#' # combined into one polygon set
#' ps <- polygon_set(list(rects, circs))
#' plot(ps, col = c("grey", "red"))
#'
#' # As can be seen, the two rects have been merged as they were touching at the
#' # edge
#'
#' # You can get the number of polygons contained in a set as well as convert it
#' # back into polygons
#' n_polygons(ps)
#' as_polygon(ps)
#'
#' # You can also extract a specific polygon from a set
#' as_polygon(ps, which = 1)
#'
#' # Alternatively by specifying a point to locate the polygon to get
#' locate_polygon(ps, euclid::point(2, 1))
#'
#' # When using boolean operations on polygons (or polygon sets) the result is a
#' # polygon set
#' p_sym_dif <- symmetric_difference(rects, circs)
#' plot(p_sym_dif, col = c("grey", "red"))
polygon_set <- function(polygons) {
  if (is_bare_list(polygons)) {
    if (!all(vapply(polygons, is_polygon, logical(1)))) {
      cli_abort(c(
        "Malformed list input when constructing polygon_set",
        i = "input must be a list of polygon vectors"
      ))
    }
    new_poly_vector(create_polygonset_list(lapply(polygons, get_ptr)))
  } else if (is_polygon(polygons)) {
    new_poly_vector(create_polygonset_polygon(get_ptr(polygons)))
  } else {
    cli_abort("Can't create polygon_set from the given input")
  }
}

#' @rdname polygon_set
#' @export
is_polygon_set <- function(x) inherits(x, "polyclid_polygon_set")
#' @rdname polygon_set
#' @export
as_polygon_set <- function(x, ...) {
  UseMethod("as_polygon_set")
}
#' @export
as_polygon_set.default <- function(x, ...) {
  as_polygon_set(as_polygon(x))
}
#' @export
as_polygon_set.polyclid_polygon <- function(x, ...) {
  polygon_set(x)
}
#' @export
as_polygon_set.polyclid_polygon_set <- function(x, ...) {
  x
}
#' @export
as_polygon.polyclid_polygon_set <- function(x, which = NULL, ...) {
  if (is.null(which)) {
    new_poly_vector(polygonset_get_all_polygon(get_ptr(x)))
  } else {
    which <- as.integer(which) - 1L
    if (anyNA(which)) {
      cli_abort("{.arg which} must be an integer vector")
    }
    new_poly_vector(polygonset_get_polygon(get_ptr(x), which))
  }
}
#' @rdname polygon_set
#' @export
n_polygons <- function(x) {
  if (!is_polygon_set(x)) {
    cli_abort("{.arg x} must be a polygon_set")
  }
  polygonset_n_polygons(get_ptr(x))
}
#' @rdname polygon_set
#' @export
locate_polygon <- function(x, location) {
  if (!is_polygon_set(x)) {
    cli_abort("{.arg x} must be a polygon_set")
  }
  if (!is_2d_point(location)) {
    cli_abort("{.arg location} must be a vector of 2D points")
  }
  new_poly_vector(polygonset_locate(get_ptr(x), location))
}
