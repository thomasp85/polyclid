#' Polyline monotonicity
#'
#' Monotonicity describes the property that values along a line is ever
#' increasing or decreasing, i.e. `f(x) > f(x + 1) || f(x) < f(x + 1)`. The weak
#' version allows equality as well, i.e. `f(x) >= f(x + 1) || f(x) <= f(x + 1)`.
#'
#' @param x A `polyclid_polyline` vector
#' @param dim Either `"x"` or `"y"` to indicate in which dimension to test
#' monotonicity in
#'
#' @return A logical vector with the same length as `x`
#'
#' @export
#'
#' @examples
#' sine <- polyline(
#'   seq(0, 2*pi, length.out = 20),
#'   sin(seq(0, 2*pi, length.out = 20))
#' )
#' is_monotone(sine, "x")
#' is_monotone(sine, "y")
#'
#' poly <- polyline(
#'   c(0, 1, 1, 2),
#'   c(0, 1, 2, 3)
#' )
#'
#' is_monotone(sine, "x")
#' is_weakly_monotone(sine, "x")
#'
is_monotone <- function(x, dim = "both") {
  if (!is_polyline(x)) {
    cli_abort("{.arg x} must be a polyline geometry")
  }
  switch(
    dim,
    x = polyline_is_x_monotone(get_ptr(x)),
    y = polyline_is_y_monotone(get_ptr(x)),
    cli_abort("{.arg dim} must be either {.val x} or {.val y}")
  )
}
#' @rdname is_monotone
#' @export
is_weakly_monotone <- function(x, dim = "x") {
  if (!is_polyline(x)) {
    cli_abort("{.arg x} must be a polyline geometry")
  }
  switch(
    dim,
    x = polyline_is_x_weakly_monotone(get_ptr(x)),
    y = polyline_is_y_weakly_monotone(get_ptr(x)),
    cli_abort("{.arg dim} must be either {.val x} or {.val y}")
  )
}
