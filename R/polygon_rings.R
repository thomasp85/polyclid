#' Polygon rings
#'
#' A polygon consists of 1 or more rings. The first ring denotes the outer
#' boundary of the polygon whereas any additional rings marks holes within that
#' boundary. If the outer boundary is empty (i.e. doesn't consist of any
#' vertices) the polygon is said to be unbounded. The different rings can be
#' extracted, modified, or, in the case of holes, added or removed. A special
#' function, `connect_holes()` exist to remove holes by connecting them with the
#' outer boundary, by tracing upwards from the top vertex in the hole until the
#' boundary is reached. Connecting holes will make any polygon invalid as the
#' part connecting the hole with the boundary will consist of two overlapping
#' edges.
#'
#' @param x,value `polyclid_polygon` vector
#' @param which integer vector recycled to the length of x referencing the hole
#' to get or set
#'
#' @return A new `polyclid_polygon` vector or, in the case of `n_holes()` an
#' integer vector
#'
#' @rdname polygon_rings
#' @name polygon_rings
#'
#' @examples
#' poly <- polygon(
#'   c(0, 10, 10, 0, 1, 5, 1),
#'   c(0, 0, 10, 10, 1, 1, 5),
#'   hole_id = c(1, 1, 1, 1, 2, 2, 2)
#' )
#' plot(poly, col = "grey")
#'
#' n_holes(poly)
#'
#' plot(boundary(poly), col = "grey")
#' plot(hole(poly, 1), col = "grey")
#'
#' # Change the boundary
#' boundary(poly) <- euclid::circle(point(5, 5), 40)
#' plot(poly, col = "grey")
#'
#' # Add a hole
#' hole(poly) <- euclid::circle(point(7, 7), 9)
#' plot(poly, col = "grey")
#'
#' # Change a hole (by using a combination of hole and def setters)
#' def(hole(poly, 1), "x") <- def(hole(poly, 1), "x") + 2
#' plot(poly, col = "grey")
#'
#' # Or remove it
#' hole(poly, 1) <- NULL
#' plot(poly, col = "grey")
#'
#' # Holes can also be removed by connecting them to the boundary
#' poly <- connect_holes(poly)
#' plot(poly, col = "grey", force_valid = FALSE)
#'
NULL

#' @rdname polygon_rings
#' @export
n_holes <- function(x) {
  if (!is_polygon(x)) {
    abort("`x` must be a polygon")
  }
  polygon_n_holes(get_ptr(x))
}
#' @rdname polygon_rings
#' @export
is_bounded <- function(x) {
  if (!is_polygon(x)) {
    abort("`x` must be a polygon")
  }
  polygon_has_boundary(get_ptr(x))
}
#' @rdname polygon_rings
#' @export
boundary <- function(x) {
  if (!is_polygon(x)) {
    abort("`x` must be a polygon geometry")
  }
  new_poly_vector(polygon_get_boundary(get_ptr(x)))
}
#' @rdname polygon_rings
#' @export
`boundary<-` <- function(x, value) {
  if (!is_polygon(x)) {
    abort("`x` must be a polygon geometry")
  }
  value <- as_polygon(value)
  new_poly_vector(polygon_set_boundary(get_ptr(x), get_ptr(value)))
}
#' @rdname polygon_rings
#' @export
hole <- function(x, which) {
  if (!is_polygon(x)) {
    abort("`x` must be a polygon")
  }
  which <- rep_len(as.integer(which) - 1L, length(x))
  if (any(which > n_holes(x))) {
    cli_abort("{.arg which} must not be larger than the number of holes")
  }
  if (any(which < 0)) {
    cli_abort("{.arg which} must be positive")
  }
  new_poly_vector(polygon_get_hole(get_ptr(x), which))
}
#' @rdname polygon_rings
#' @export
`hole<-` <- function(x, which = NULL, value) {
  if (!is_polygon(x)) {
    abort("`x` must be a polygon")
  }
  if (!is.null(which)) {
    which <- rep_len(as.integer(which) - 1L, length(x))
    if (any(which > n_holes(x))) {
      cli_abort("{.arg which} must not be larger than the number of holes")
    }
    if (any(which < 0)) {
      cli_abort("{.arg which} must be positive")
    }
  }
  if (is.null(value)) {
    if (is.null(which)) {
      boundary(x)
    } else {
      new_poly_vector(polygon_remove_hole(get_ptr(x), which))
    }
  } else {
    value <- as_polygon(value)
    if (any(n_holes(value) != 0)) {
      warn("only the boundary of `value` is used when assigning as a hole")
    }
    if (is.null(which)) {
      new_poly_vector(polygon_add_hole(get_ptr(x), get_ptr(value)))
    } else {
      new_poly_vector(polygon_set_hole(get_ptr(x), which, get_ptr(value)))
    }
  }
}

#' @rdname polygon_rings
#' @export
connect_holes <- function(x) {
  if (!is_polygon(x)) {
    cli_abort("{.arg x} must be a polygon")
  }
  new_poly_vector(polygon_connect_holes(get_ptr(x)))
}
