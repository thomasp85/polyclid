#' Vector of polygons
#'
#' A polygon is a set of points defining the boundary of a region. A polygon can
#' contain one or more holes. They must be fully contained inside the outer
#' boundary.
#'
#' @param ... Vector of points, numerics, or a list. See the *Constructors*
#' sections
#' @param id An integer vector of the same length as `points`, dividing the
#' points into separate polygons (only used if `points` is a point vector)
#' @param hole_id An integer vector of the same length as `points`, dividing
#' the points into boundary and separate holes (only used if `points` is a point
#' vector).
#' @param x An object convertible to a polygon vector or a polygon vector
#'
#' @return An `polyclid_polygon` vector
#'
#' @section Constructors:
#' - Providing a 2D points vector and no `id` and `holes_id` will construct a
#'   single polygon with no holes
#' - Providing a 2D points vector and an `id` vector will create a vector of
#'   polygons with no holes
#' - Providing a 2D points vector and a `holes_id` vector will construct a single
#'   polygon with holes
#' - Providing a 2D points vector, an `id` vector, and a `holes_id` vector will
#'   construct a vector of polygons with holes
#' - For all above, instead of a 2D point vector the x and y coordinates can be
#'   supplied directly
#' - Providing a list of 2D point vectors will construct a vector of polygons
#'   with no holes
#' - Providing a list of list of 2D point vectors will construct a vector of
#'   polygons with holes
#'
#' Further, polygons can also be constructed from 2D segments and triangles
#' using the `as_polygon()` function.
#'
#' @export
#' @importFrom euclid point is_exact_numeric
#' @family polygons
#'
#' @examples
#' points <- euclid::point(
#'   c(1, 0, -1, -0.5, 0.5),
#'   c(0, 1, 0, -1, -1)
#' )
#'
#' # Construct a single polygon from a vector of points
#' poly <- polygon(points)
#' poly
#'
#' plot(poly, col = "grey")
#'
#' # Or directly from x and y coordinates
#' poly <- polygon(
#'   c(1, 0, -1, -0.5, 0.5),
#'   c(0, 1, 0, -1, -1)
#' )
#'
#' # Use id to split points into multiple polygons
#' poly <- polygon(points[c(1, 2, 3, 4, 5, 1)], id = rep(1:2, each = 3))
#' poly
#' plot(poly, col = "grey")
#'
#' # Use a list of lists to define polygons with holes
#' poly <- polygon(list(
#'   list(
#'     points,
#'     euclid::point(c(0, 0.5, -0.5), c(0.5, -0.5, -0.5))
#'   )
#' ))
#' plot(poly, col = "grey")
#'
#' # or use hole_id to similar effect (same polygon as above)
#' poly2 <- polygon(
#'   c(points, euclid::point(c(0, 0.5, -0.5), c(0.5, -0.5, -0.5))),
#'   hole_id = c(1, 1, 1, 1, 1, 2, 2, 2)
#' )
#' poly == poly2
#'
#' # Equality of polygons doesn't care about where on the ring the vertices start
#' poly <- polygon(points)
#' poly2 <- polygon(points[c(2:5, 1)])
#' poly == poly2
#'
#' # It cares about orientation though
#' poly == reverse_orientation(poly)
#'
#' # This have implications for unqiue and duplicated
#' polys <- c(poly, poly2, reverse_orientation(poly))
#' unique(polys)
#' duplicated(polys)
#'
polygon <- function(..., id = NULL, hole_id = NULL) {
  args <- list(...)
  points <- args[[1]]
  if (is_bare_list(points)) {
    holes <- !all(vapply(points, is_2d_point, logical(1)))
    need_split <- FALSE
    if (holes) {
      if (!all(vapply(points, function(x) all(vapply(x, is_2d_point, logical(1))), logical(1)))) {
        cli_abort(c(
          "Malformed list input when constructing polygon.",
          i = "input must be a list of 2D point vectors or a list of lists of 2D point vectors"
        ))
      }
    }
  } else if ((is.numeric(args[[1]]) || is_exact_numeric(args[[1]])) &&
             (is.numeric(args[[2]]) || is_exact_numeric(args[[2]]))) {
    need_split <- TRUE
    points <- point(args[[1]], args[[2]])
  } else if (is_2d_point(points)) {
    need_split <- TRUE
  } else {
    cli_abort("Don't know how to construct polygons from the given input")
  }

  if (need_split) {
    if (is.null(hole_id)) {
      if (is.null(id)) {
        return(new_poly_vector(create_polygon_single(points)))
      } else {
        points <- split(points, id)
      }
      holes <- FALSE
    } else {
      if (is.null(id)) {
        id <- rep.int(1L, length(points))
      }
      points <- split(points, id)
      hole_id <- split(hole_id, id)
      points <- Map(split, points, hole_id)
      holes <- TRUE
    }
  }
  if (holes) {
    new_poly_vector(create_polygon_list_list(points))
  } else {
    new_poly_vector(create_polygon_list(points))
  }
}
#' @rdname polygon
#' @export
is_polygon <- function(x) inherits(x, "polyclid_polygon")

#' @rdname polygon
#' @export
as_polygon <- function(x, ...) {
  UseMethod("as_polygon")
}
#' @export
as_polygon.polyclid_polygon <- function(x, ...) {
  x
}
#' @export
as_polygon.euclid_triangle2 <- function(x, ...) {
  new_poly_vector(create_polygon_triangle(x))
}
#' @export
as_polygon.euclid_iso_rect <- function(x, ...) {
  polygon(vert(x), id = rep(seq_along(x), each = 4))
}
#' @export
#' @importFrom euclid vert approx_radius vec
as_polygon.euclid_circle2 <- function(x, n = 100, ...) {
  center <- vert(x)
  radius <- approx_radius(x)
  boundary <- rep(seq(0, 2*pi, length.out = n + 1)[-(n + 1)], length(x))
  boundary <- vec(cos(boundary), sin(boundary)) * rep(radius, each = n)
  polys <- split(rep(center, each = n) + boundary, rep(seq_along(x), each = n))
  new_poly_vector(create_polygon_list(polys))
}

#' @importFrom euclid is_point
is_2d_point <- function(x) is_point(x) && dim(x) == 2
