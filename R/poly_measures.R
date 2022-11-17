#' @importFrom euclid approx_length
#' @export
approx_length.polyclid_geometry <- function(x) {
  poly_approx_length(get_ptr(x))
}

#' @importFrom euclid approx_area
#' @export
approx_area.polyclid_geometry <- function(x) {
  poly_approx_area(get_ptr(x))
}

#' @importFrom euclid approx_volume
#' @export
approx_volume.polyclid_geometry <- function(x) {
  rep_len(NA_real_, length(x))
}
