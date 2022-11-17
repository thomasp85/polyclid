#' Polygon orientation
#'
#' Polygon rings can have two different orientations: clockwise and
#' counterclockwise. In a clockwise oriented polygon the polygon lies to the
#' right as you traverse the boundary in order, while the reverse is true for
#' a counterclockwise oriented polygon. Both orientations are basically valide
#' representations of a polygon but many algorithms assumes a certain
#' orientation of the boundary and holes in order to work. The outer boundary
#' must be counterclockwise and all holes must be clockwise. This will ensure
#' that the polygon always lies to the left as you traverse all the borders of
#' the polygon. `reverse_orientation()` reverses the orientation of both the
#' outer boundary and the holes. `is_clockwise()` and `is_counterclockwise()`
#' only reports the orientation of the outer boundary. To get the orientation
#' of a hole, first extract it with [hole()].
#'
#' @param x A `polyclid_polygon` vector
#'
#' @return A new `polyclid_polygon` vector for `reverse_orientation()` and a
#' logical vector for `is_clockwise()` and `is_conterclockwise()`
#'
#' @export
#'
#' @examples
#' poly <- polygon(
#'   euclid::point(
#'     c(0, 10, 10, 0),
#'     c(10, 10, 0, 0)
#'   )
#' )
#'
#' is_clockwise(poly)
#' poly <- reverse_orientation(poly)
#' is_counterclockwise(poly)
#'
reverse_orientation <- function(x) {
  if (!is_polygon(x) && !is_polyline(x)) {
    cli_abort("{.arg x} must be a polygon or polyline")
  }
  new_poly_vector(poly_reverse_orientation(get_ptr(x)))
}
#' @rdname reverse_orientation
#' @export
is_clockwise <- function(x) {
  if (!is_polygon(x)) {
    cli_abort("{.arg x} must be a polygon")
  }
  polygon_is_clockwise(get_ptr(x))
}
#' @rdname reverse_orientation
#' @export
is_counterclockwise <- function(x) {
  if (!is_polygon(x)) {
    cli_abort("{.arg x} must be a polygon")
  }
  polygon_is_counterclockwise(get_ptr(x))
}
