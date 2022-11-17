get_ptr <- function(x) .subset2(x, 1L)

restore_poly_vector <- function(x, old) {
  x <- list(x)
  attributes(x) <- attributes(old)
  x
}
