#' @importFrom euclid is_degenerate
#' @export
is_degenerate.polyclid_geometry <- function(x) {
  poly_is_degenerate(get_ptr(x))
}

#' @importFrom euclid has_on as_point
#' @export
has_on.polyclid_geometry <- function(x, y) {
  poly_has_point_on(get_ptr(x), as_point(y))
}

#' @importFrom euclid has_inside as_point
#' @export
has_inside.polyclid_geometry <- function(x, y) {
  poly_has_point_inside(get_ptr(x), as_point(y))
}

#' @importFrom euclid has_outside as_point
#' @export
has_outside.polyclid_geometry <- function(x, y) {
  poly_has_point_outside(get_ptr(x), as_point(y))
}

#' @importFrom euclid has_on_positive_side as_point
#' @export
has_on_positive_side.polyclid_geometry <- function(x, y) {
  poly_has_point_on_positive(get_ptr(x), as_point(y))
}

#' @importFrom euclid has_on_negative_side as_point
#' @export
has_on_negative_side.polyclid_geometry <- function(x, y) {
  poly_has_point_on_negative(get_ptr(x), as_point(y))
}
