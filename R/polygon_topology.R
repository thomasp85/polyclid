#' Polygon topology
#'
#' These functions checks certain aspects of a polygons topology. They all
#' operate only on the outer boundary of a polygon and thus ignores all holes it
#' may have. A convex polygon is one whose boundary exclusively have the same
#' turn direction as it is traversed. A simply polygon is a polygon with no
#' intersections between its edges except at the end points of neighboring
#' edges). A relatively simple polygon is like a simple polygon except it
#' allows a shared end point between non-neighboring edges, with the requirement
#' that it remains orientable (it must not "cross over" at a vertex). A
#' relatively simple polygon is always simple. Relative simplicity is one of the
#' requirements of rings in a valid polygon.
#'
#' @param x A `polyclid_polygon` vector
#'
#' @return a logical vector of the same length as `x`
#'
#' @export
#'
#' @rdname polygon_topology
#' @name polygon_topology
#'
#' @examples
#'
#' square <- polygon(
#'   euclid::point(
#'     c(0, 10, 10, 0),
#'     c(0, 0, 10, 10)
#'   )
#' )
#' cross <- polygon(
#'   euclid::point(
#'     c(0, 10, 10, 0),
#'     c(0, 10, 0, 10)
#'   )
#' )
#' butterfly <- polygon(
#'   euclid::point(
#'     c(0, 5, 10, 10, 5, 0),
#'     c(0, 5, 0, 10, 5, 10)
#'   )
#' )
#' cross2 <- polygon(
#'   euclid::point(
#'     c(0, 5, 10, 10, 5, 0),
#'     c(0, 5, 10, 0, 5, 10)
#'   )
#' )
#' is_convex(square)
#' is_convex(cross)
#' is_convex(butterfly)
#' is_convex(cross2)
#'
#' is_simple(square)
#' is_simple(cross)
#' is_simple(butterfly)
#' is_simple(cross2)
#'
#' is_relatively_simple(square)
#' is_relatively_simple(cross)
#' is_relatively_simple(butterfly)
#' is_relatively_simple(cross2)
is_convex <- function(x) {
  if (!is_polygon(x)) {
    cli_abort("{.arg x} must be a polygon")
  }
  polygon_is_convex(get_ptr(x))
}

#' @rdname polygon_topology
#' @export
is_simple <- function(x) {
  if (!is_polygon(x)) {
    cli_abort("{.arg x} must be a polygon")
  }
  polygon_is_simple(get_ptr(x))
}
#' @rdname polygon_topology
#' @export
is_relatively_simple <- function(x) {
  if (!is_polygon(x)) {
    cli_abort("{.arg x} must be a polygon")
  }
  polygon_is_relatively_simple(get_ptr(x))
}
