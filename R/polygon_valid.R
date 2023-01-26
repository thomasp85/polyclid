#' Polygon validity
#'
#' While a polygon vector can hold more or less any combination of rings, only
#' a certain subset of these are valid and will produce correct answer to the
#' various functions. Algorithms that require a valid polygon will attempt to
#' fix invalid polygons beforehand, but it is often better to do it once after
#' construction to avoid repeated repairs every time an algorithm is called on
#' the polygon.
#'
#' @details
#' The requirement for a valid polygon in polyclid are as follow:
#'
#' * The outer boundary must be [relatively simple][is_relatively_simple] and
#'   all holes must be [simple][is_simple]
#' * The outer boundary must be oriented counter-clockwise and all holes must be
#'   clockwise
#' * The holes and the outer boundary must be pairwise disjoint except at
#'   vertices. All holes must be contained inside the outer boundary and holes
#'   must not partly or fully overlap
#'
#' `make_valid()` attempts to correct an invalid polygon in the following way:
#'
#' 1. Checks if the outer boundary is relatively simple. If not it returns `NA`.
#' 2. Checks and corrects the orientation of the outer boundery.
#' 3. Checks if holes are simple. If any are not it returns `NA`.
#' 4. Checks and fixes the orientation of all holes.
#' 5. One by one uses boolean difference to cut out holes in the boundary. This
#'    means that holes outside the boundary are ignored, overlapping holes are
#'    merged, and holes intersecting with the boundary will become part of the
#'    boundary.
#' 6. If *5* has resulted in cutting up the polygon in multiple disjoint parts
#'    only one will be returned. If the polygon remains connected then that is
#'    of course returned.
#'
#' When calling `is_valid()` the result is cached in the polygon to avoid
#' repeated checks. Any modification of the polygon will of course clear the
#' cache. If a polygon has a cached result that indicates that the polygon is
#' valid it is returned as is.
#'
#' @param x A `polyclid_polygon` vector
#'
#' @return `is_valid()` returns a logical vector of the same length as `x`.
#' `make_valid()` returns a `polyclid_polygon()` vector of the same length as
#' `x`.
#'
#' @export
#'
#' @examples
#'
#' poly <- polygon(
#'   c(0, 10, 10, 0),
#'   c(10, 10, 0, 0)
#' )
#'
#' # Outer boundary must be counterclockwise for the polygon to be valid
#' is_clockwise(poly)
#' is_valid(poly)
#'
#' poly <- reverse_orientation(poly)
#' is_valid(poly)
#'
#' h <- polygon(
#'   c(2, 4, 2),
#'   c(2, 2, 4)
#' )
#'
#' # Holes must be clockwise
#' is_counterclockwise(h)
#' hole(poly) <- h
#' is_valid(poly)
#'
#' hole(poly, 1) <- reverse_orientation(h)
#' is_valid(poly)
#'
#' # Holes must be completely contained and non-overlapping with the boundary
#' h <- as_polygon(circle(point(8, 8), 15))
#' plot(c(poly, h))
#'
#' hole(poly) <- h
#' is_valid(poly)
#'
#' # Use make_valid() to resolve holes (and orientation)
#' poly <- make_valid(poly)
#' plot(poly)
#' is_valid(poly)
#'
#' # not all polygons can be made valid, specifically rings that are not
#' # relatively simple cannot be fixed
#' poly <- polygon(
#'   c(0, 10, 10, 0),
#'   c(0, 10, 0, 10)
#' )
#' is_valid(poly)
#' plot(poly, force_valid = FALSE)
#'
#' # Trying to make them valid will return NA
#' make_valid(poly)
is_valid <- function(x) {
  if (!is_polygon(x)) {
    cli_abort("{.arg x} must be a polygon")
  }
  polygon_is_valid(get_ptr(x))
}

#' @rdname is_valid
#' @export
make_valid <- function(x) {
  if (!is_polygon(x)) {
    cli_abort("{.arg x} must be a polygon")
  }
  new_poly_vector(polygon_make_valid(get_ptr(x)))
}
