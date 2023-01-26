#' Calculate angle at vertices of polylines or polygons
#'
#' This function will calculate the angle at each vertex of a polygon or
#' polyline. The measured angle is that on the left side of the line, ie if it
#' turns left the angle will be between 0 and pi, and if it turns right it will
#' be between pi and 2pi.
#'
#' @param x A `polyclid_polyline` or `polyclid_polygon` vector
#' @param ... Arguments passed on to methods
#'
#' @return A numeric vector of matchng the vertices returned by [euclid::vert()]
#'
#' @export
#'
#' @examples
#' # Polylines
#' sine <- polyline(
#'   seq(0, 2*pi, length.out = 20),
#'   sin(seq(0, 2*pi, length.out = 20))
#' )
#' approx_vert_angle(sine)
#'
#' # Polygons
#' poly <- polygon(
#'   c(0, 10, 10, 0, 1, 5, 1),
#'   c(0, 0, 10, 10, 1, 1, 5),
#'   hole_id = c(1, 1, 1, 1, 2, 2, 2)
#' )
#' approx_vert_angle(poly)
#'
approx_vert_angle <- function(x, ...) {
  UseMethod("approx_vert_angle")
}
#' @importFrom euclid approx_angle turns_right
#' @export
approx_vert_angle.polyclid_polyline <- function(x, ...) {
  n_segs <- edge_count(x)
  segs <- edge(x)
  unlist(lapply(split(segs, rep(seq_along(x), n_segs)), function(seg) {
    s1 <- seg[seq_len(length(seg) - 1)]
    s2 <- seg[1 + seq_len(length(seg) - 1)]
    a <- approx_angle(s1, s2)
    right <- turns_right(vert(s1, 1), vert(s1, 2), vert(s2, 2))
    a <- pi + ifelse(right, 1, -1) * a[right]
    c(NA, a, NA)
  }))
}
#' @importFrom euclid approx_angle turns_right
#' @export
approx_vert_angle.polyclid_polygon <- function(x, ...) {
  x <- rings(x)
  n_segs <- edge_count(x)
  segs <- edge(x)
  unlist(lapply(split(segs, rep(seq_along(x), n_segs)), function(seg) {
    s1 <- c(seg[length(seg)], seg[-length(seg)])
    a <- approx_angle(seg, s1)
    right <- turns_right(vert(seg, 1), vert(seg, 2), vert(s1, 2))
    pi + ifelse(right, 1, -1) * a
  }), use.names = FALSE)
}

#' Calculate vertex normals of a polyline or polygon
#'
#' This function allows you to retrieve the vertex normals (mean of normals from
#' the adjacent line segments). As this requires normalising the normals it
#' cannot be done exactle hence the `approx_` prefix. For polylines the normal
#' is oriented in the same way as it is for the individual segments, whereas for
#' polygons they are oriented outwards assuming a valid polygon.
#'
#' @param x A `polyclid_polyline` or `polyclid_polygon` vector
#' @param ... Arguments passed on to methods
#'
#' @return A `euclid_direction2` vector of normals matchng the vertices returned
#' by [euclid::vert()]
#'
#' @export
#'
#' @examples
#' # Polylines
#' sine <- polyline(
#'   seq(0, 2*pi, length.out = 20),
#'   sin(seq(0, 2*pi, length.out = 20))
#' )
#' normals <- approx_vert_normal(sine)
#' normals
#'
#' plot(sine)
#' euclid_plot(segment(vert(sine), as_vec(normals)))
#'
#' # Polygons
#' poly <- polygon(
#'   c(0, 10, 10, 0, 1, 5, 1),
#'   c(0, 0, 10, 10, 1, 1, 5),
#'   hole_id = c(1, 1, 1, 1, 2, 2, 2)
#' )
#' normals <- approx_vert_normal(poly)
#' normals
#'
#' plot(poly)
#' euclid_plot(segment(vert(poly), as_vec(normals)))
#'
approx_vert_normal <- function(x, ...) {
  UseMethod("approx_vert_normal")
}
#' @rdname approx_vert_normal
#' @importFrom euclid normal as_vec as_direction
#' @export
approx_vert_normal.polyclid_polyline <- function(x, ...) {
  n_segs <- edge_count(x)
  normals <- as_vec(normal(edge(x)))
  normals <- normals/approx_length(normals)
  normals <- lapply(split(normals, rep(seq_along(x), n_segs)), function(n) {
    mean_normal <- (n[seq_len(length(n) - 1)] + n[1 + seq_len(length(n) - 1)]) / 2
    c(n[1], mean_normal, n[length(n)])
  })
  as_direction(do.call(c, normals))
}
#' @rdname approx_vert_normal
#' @importFrom euclid normal as_vec as_direction
#' @export
approx_vert_normal.polyclid_polygon <- function(x, ...) {
  x <- rings(x)
  n_segs <- edge_count(x)
  normals <- -as_vec(normal(edge(x)))
  normals <- normals/approx_length(normals)
  normals <- lapply(split(normals, rep(seq_along(x), n_segs)), function(n) {
    n1 <- c(n[length(n)], n[-length(n)])
    (n + n1) / 2
  })
  as_direction(do.call(c, normals))
}
