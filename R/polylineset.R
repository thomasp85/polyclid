#' Sets of polylines
#'
#' A polyline set is a collections of interior disjoint x-monotone polylines. As
#' with [polygon_set] they are used as containers for the result of boolean
#' operations on polylines. As they are made up of disjoint polylines any
#' overlapping polylines will be cut at their intersection. The requirement for
#' x-monotonicity also means that a polyline may be split into several even if
#' it doesn't (self)intersect. polyline sets may give rise to polygons if they
#' have closed interior. These faces of the set can be extracted as polygons
#' using [as_polygon()]. Further, a polyline set can be thought of as a graph
#' structure with the end vertices of the polylines being nodes, and the
#' polylines being edges. The number of connections each node has can be
#' extracted with `vert_degree()` and its neighbors with `vert_neighbors()`.
#' Further the vertices that each polyline is associated with can be obtained
#' with `polyline_verts()`.
#'
#' @param polylines A `polyclid_polygon` vector or a list of these. In the latter
#' case the union of the polygons in each vector is used for the set.
#' @param x A `polyclid_polyline_set` vector
#' @param simplify Should the extracted polylines be simplified by joining lines
#' where possible
#' @param ... Arguments passed on
#'
#' @return a `polyclid_polygon_set` vector
#'
#' @export
#' @family polylines
#' @family sets
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
#' ps <- polyline_set(c(sine, loop))
#'
#' # Vertices are only defined for end points - not points interior to the
#' # polylines
#' euclid_plot(vert(ps))
#'
#' # You can get back the polylines as they are represented or simplified by
#' # joining lines that meet at vertices with a degree of 2
#' plot(
#'   as_polyline(ps[2]),
#'   col = c("black", "red", "green", "blue")
#' )
#' plot(
#'   as_polyline(ps[2], simplify = TRUE),
#'   col = c("black", "red", "green", "blue")
#' )
#'
#' # If a polyline_set defines a closed area, that can be extracted as a polygon
#' n_faces(ps)
#' as_polygon(ps)
#'
#' # A polyline set can be seen as a graph and the relevant info can be obtained
#' vert_degree(ps[2])
#' vert_neighbors(ps[2])
#' polyline_verts(ps[2])
#'
polyline_set <- function(polylines) {
  if (is_bare_list(polylines)) {
    if (!all(vapply(polylines, is_polyline, logical(1)))) {
      cli_abort(c(
        "Malformed list input when constructing polyline_set",
        i = "input must be a list of polyline vectors"
      ))
    }
    new_poly_vector(create_polylineset_list(lapply(polylines, get_ptr)))
  } else if (is_polyline(polylines)) {
    new_poly_vector(create_polylineset_polyline(get_ptr(polylines)))
  } else {
    cli_abort("Can't create polyline_set from the given input")
  }
}

#' @rdname polyline_set
#' @export
is_polyline_set <- function(x) inherits(x, "polyclid_polyline_set")
#' @rdname polyline_set
#' @export
as_polyline_set <- function(x, ...) {
  UseMethod("as_polyline_set")
}
#' @export
as_polyline_set.default <- function(x, ...) {
  as_polyline_set(as_polyline(x))
}
#' @export
as_polyline_set.polyclid_polyline <- function(x, ...) {
  polyline_set(x)
}
#' @export
as_polyline_set.polyclid_polyline_set <- function(x, ...) {
  x
}
#' @export
as_polyline.polyclid_polyline_set <- function(x, which = NULL, simplify = FALSE, ...) {
  if (is.null(which)) {
    if (simplify) {
      new_poly_vector(polylineset_get_all_polylines_simplified(get_ptr(x)))
    } else {
      new_poly_vector(polylineset_get_all_polylines(get_ptr(x)))
    }
  } else {
    which <- as.integer(which) - 1L
    if (anyNA(which)) {
      cli_abort("{.arg which} must be an integer vector")
    }
    if (simplify) {
      new_poly_vector(polylineset_get_polylines_simplified(get_ptr(x), which))
    } else {
      new_poly_vector(polylineset_get_polylines(get_ptr(x), which))
    }
  }
}
#' @export
as_polygon.polyclid_polyline_set <- function(x, ...) {
  new_poly_vector(polylineset_get_faces(get_ptr(x)))
}
#' @rdname polyline_set
#' @export
n_polylines <- function(x, simplify = FALSE) {
  if (!is_polyline_set(x)) {
    cli_abort("{.arg x} must be a polyline_set")
  }
  if (simplify) {
    polylineset_n_polylines_simplified(get_ptr(x))
  } else {
    polylineset_n_polylines(get_ptr(x))
  }
}
#' @rdname polygon_set
#' @export
n_faces <- function(x) {
  if (!is_polyline_set(x)) {
    cli_abort("{.arg x} must be a polyline_set")
  }
  polylineset_n_faces(get_ptr(x))
}
#' @rdname polygon_set
#' @export
vert_degree <- function(x) {
  if (!is_polyline_set(x)) {
    cli_abort("{.arg x} must be a polyline_set")
  }
  polylineset_vert_degree(get_ptr(x))
}
#' @rdname polygon_set
#' @export
vert_neighbors <- function(x) {
  if (!is_polyline_set(x)) {
    cli_abort("{.arg x} must be a polyline_set")
  }
  polylineset_vert_neighbors(get_ptr(x))
}
#' @rdname polygon_set
#' @export
polyline_verts <- function(x) {
  if (!is_polyline_set(x)) {
    cli_abort("{.arg x} must be a polyline_set")
  }
  polylineset_polyline_verts(get_ptr(x))
}
