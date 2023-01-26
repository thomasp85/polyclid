#' Insert vertices into a polyline or polygon
#'
#' You can alter polygons and polylines be inserting additional vertices after a
#' specific vertex. The new vertices can be given as either a point, a segment,
#' or a polyline. While inserting new vertices at the correct location in
#' polylines are quite easy, figuring out the correct location in polygons are
#' much harder because of their ring structure, as it essentially requires you
#' to have knowledge about which vertex in the ring is the first in the internal
#' representation. Further, if the polygon contains holes the indexing continues
#' into the wholes, making it even harder to target the correct position in a
#' hole. In these situations it is often better to extract and replace the hole,
#' rather than work with the full polygon.
#'
#' @param x A `polyclid_polygon` or `polyclid_polyline` vector
#' @param vert A `euclid_point2`, `euclid_segment2`, or `polyclid_polyline`
#' vector
#' @param at A integer vector giving the index of the vertex after which the new
#' vertices should be inserted. `0` means insert in the front
#'
#' @return A `polyclid_polygon` or `polyclid_polyline` vector of the same length
#' as the input
#'
#' @export
#'
#' @examples
#'
#' ## Polylines
#' loop <- polyline(
#'   c(0, 5, 6, 5, 0),
#'   c(-1, 1, 0, -1, 1)
#' )
#' # Insert point after the first vertex
#' loop2 <- vert_insert(loop, point(1, -1), 1)
#' plot(loop2)
#'
#' # Insert segment after the last vertex
#' loop3 <- vert_insert(loop, segment(point(0, 0), point(1, -1)), cardinality(loop))
#' plot(loop3)
#'
#' # Insert polyline after 4th vertex
#' loop4 <- vert_insert(loop, polyline(4:1, c(-1, 1, -1, 1)), 4)
#' plot(loop4)
#'
#' ## Polygons
#'
vert_insert <- function(x, vert, at = 0L) {
  if (!is_polygon(x) && !is_polyline(x)) {
    cli_abort("{.arg x} must be a polygon or polyline vector")
  }
  if (!is_polyline(vert)) {
    if (dim(vert) != 2) {
      cli_abort("{.arg vert} must be in 2 dimensions")
    }
    if (is_point(vert)) {
      vert <- polyline(vert, id = seq_along(vert))
    } else if (is_segment(vert)) {
      vert <- as_polyline(vert)
    } else {
      cli_abort("{.arg vert} must be a polyline, point, or segment vector")
    }
  }
  at <- rep_len(as.integer(at), length(x))
  if (any(at < 0L)) {
    cli_warn("Negative {.arg at} coerced to 0 (insert at front)")
    at[at < 0L] <- 0L
  }
  card <- cardinality(x)
  if (any(at > card)) {
    cli_warn("{.arg at} higher than the cardinality are coerced to the cardinality (insert at back)")
    at[at > card] <- card[at > card]
  }
  new_poly_vector(poly_insert_vert(get_ptr(x), get_ptr(vert), at))
}
