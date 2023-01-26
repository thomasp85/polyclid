#' Dataset of boundaries for Denmark
#'
#' This function returns a polygon vector encoding the various geographical
#' areas of Denmark at a high resolution.
#'
#' @return A `polyclid_polygon` vector
#'
#' @export
#'
#' @examples
#'
#' dk <- denmark()
#'
#' plot(dk)
#'
denmark <- function() {
  make_valid(polygon(denmark_internal$x, denmark_internal$y, id = denmark_internal$id))
}
