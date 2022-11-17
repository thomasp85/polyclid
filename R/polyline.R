#' Vector of polylines
#'
#' A polyline is a set of points defining the trajectory of a path.
#'
#' @param points A vector of points, or a list. See the *Constructors* sections
#' @param id An integer vector of the same length as `points`, dividing the
#' points into separate polylines (only used if `points` is a point vector)
#'
#' @return An `polyclid_polyline` vector
#'
#' @section Constructors:
#' - Providing a points vector and no `id` will construct a single polyline with
#'   no holes
#' - Providing a points vector and an `id` vector will create a vector of
#'   polylines
#' - Providing a list of point vectors will construct a vector of polylines
#'
#' @export
#' @importFrom euclid is_point
#'
#' @examples
#' sine <- polyline(
#'   euclid::point(
#'     seq(0, 2*pi, length.out = 20),
#'     sin(seq(0, 2*pi, length.out = 20))
#'   )
#' )
#' loop <- polyline(
#'   euclid::point(
#'     c(0, 5, 6, 5, 0),
#'     c(-1, 1, 0, -1, 1)
#'   )
#' )
#' polylines <- c(sine, loop)
#' plot(polylines, lty = c(1, 2))
#'
#' is_selfintersecting(polylines)
#'
#' # Polylines can also be reversed so they are traversed in the opposite
#' # direction
#' reverse_orientation(polylines)
#'
polyline <- function(points, id = NULL) {
  if (is_point(points)) {
    if (is.null(id)) {
      return(new_poly_vector(create_polyline_single(points)))
    }
    points <- split(points, id)
  }
  new_poly_vector(create_polyline_list(points))
}
#' @rdname polyline
#' @export
is_polyline <- function(x) inherits(x, "polyclid_polyline")

#' @rdname polyline
#' @export
as_polyline <- function(x, ...) {
  UseMethod("as_polyline")
}
#' @export
as_polyline.euclid_segment2 <- function(x, ...) {
  new_poly_vector(create_polyline_segment(x))
}

#' @export
is_selfintersecting <- function(x) {
  if (!is_polyline(x)) {
    cli_abort("{.arg x} must be a polyline geometry")
  }
  polyline_is_selfintersecting(get_ptr(x))
}
