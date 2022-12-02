#' Vector of polylines
#'
#' A polyline is a set of points defining the trajectory of a path.
#'
#' @param ... Various input for construction. See the *Constructors* sections
#' @param id An integer vector of the same length as `points`, dividing the
#' points into separate polylines (only used if `points` is a point vector)
#' @param x A `polyclid_polyline` vector
#'
#' @return A `polyclid_polyline` vector
#'
#' @section Constructors:
#' - Providing a 2D points vector and no `id` will construct a single polyline
#' - Providing a 2D points vector and an `id` vector will create a vector of
#'   polylines
#' - Instead of a 2D point vector above you can provide x and y coordinates for
#'   the points directly.
#' - Providing a list of 2D point vectors will construct a vector of polylines
#'
#' @export
#' @importFrom euclid is_point
#' @family polylines
#' @seealso To get a complete overview of the boolean operations possible with
#' polylines see the [dedicated help page on the topic][poly_bool]
#'
#' @examples
#' sine <- polyline(
#'   seq(0, 2*pi, length.out = 20),
#'   sin(seq(0, 2*pi, length.out = 20))
#' )
#' loop <- polyline(
#'   c(0, 5, 6, 5, 0),
#'   c(-1, 1, 0, -1, 1)
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
polyline <- function(..., id = NULL) {
  args <- list(...)
  if (is_bare_list(args[[1]])) {
    points <- args[[1]]
    if (!all(vapply(points, is_2d_point, logical(1)))) {
      cli_abort(c(
        "Malformed list input when constructing polyline",
        i = "input must be a list of 2D point vectors"
      ))
    }
  } else if ((is.numeric(args[[1]]) || is_exact_numeric(args[[1]])) &&
             (is.numeric(args[[2]]) || is_exact_numeric(args[[2]]))) {
    points <- point(args[[1]], args[[2]])
  } else if (is_2d_point(args[[1]])) {
    points <- args[[1]]
  } else {
    cli_abort("Don't know how to construct polylines from the given input")
  }
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

#' @rdname polyline
#' @export
is_selfintersecting <- function(x) {
  if (!is_polyline(x)) {
    cli_abort("{.arg x} must be a polyline geometry")
  }
  polyline_is_selfintersecting(get_ptr(x))
}
