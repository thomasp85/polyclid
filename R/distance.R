#' @importFrom euclid distance_squared
#' @export
distance_squared.polyclid_polyline <- function(x, y, ...) {
  max_length <- max(length(x), length(y))
  x <- rep_len(x, max_length)
  y <- rep_len(y, max_length)
  n_edges <- edge_count(x)
  y <- rep(y, n_edges)
  x <- edge(x)
  d <- distance_squared(x, y)
  d <- lapply(split(d, rep(seq_along(n_edges), n_edges)), min)
  do.call(c, d)
}
#' @importFrom euclid distance_squared exact_numeric
#' @export
distance_squared.polyclid_polygon <- function(x, y, ...) {
  max_length <- max(length(x), length(y))
  x <- rep_len(x, max_length)
  y <- rep_len(y, max_length)
  # TODO: This part only works for points
  inside <- has_inside(x, y) || has_on(x, y)
  d <- rep_len(exact_numeric(0), length(x))
  x <- x[!inside]
  y <- y[!inside]
  n_edges <- edge_count(x)
  y <- rep(y, n_edges)
  x <- edge(x)
  d2 <- distance_squared(x, y)
  d2 <- lapply(split(d2, rep(seq_along(n_edges), n_edges)), min)
  d2 <- do.call(c, d2)
  d[!inside] <- d2
  d
}
