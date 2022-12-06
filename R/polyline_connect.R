#' Connect polylines together to a single line
#'
#' Taking the `union()` of polylines doesn't connect them together, but places
#' them in the same arrangement (it might even break them up depending on their
#' monotonicity). `connect_polylines()` does exactly what it advertises. it
#' takes a vector of polylines (or a list of polyline vectors) and combines them
#' into a single polyline by connecting the endpoint of each polyline with the
#' start point of the next. If the end and the start are equal the duplicate
#' vertex will be removed.
#'
#' @param x A `polyclid_polyline` vector or a list of these
#' @param na.rm Should `NA` values be removed when connecting. If `FALSE` and
#' the input contains `NA`, the result will be `NA`.
#'
#' @return A `polyclid_polyline` vector
#'
#' @export
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
#' pl <- c(sine, loop)
#'
#' plot(connect_polylines(pl))
#'
#' sine1 <- sine[1:10]
#' sine2 <- sine[10:20]
#'
#' sine == connect_polylines(c(sine1, sine2))
#'
connect_polylines <- function(x, na.rm = TRUE) {
  if (!rlang::is_bare_list(x)) {
    if (!is_polyline(x)) {
      cli_abort("{.arg x} must be a polyline vector or a list thereof")
    }
    x <- list(get_ptr(x))
  } else {
    if (!all(vapply(x, is_polyline, logical(1)))) {
      cli_abort("{.arg x} must be a polyline vector or a list thereof")
    }
    x <- lapply(x, get_ptr)
  }
  new_poly_vector(polyline_glue(x, na.rm))
}
