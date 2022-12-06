new_poly_vector <- function(x) {
  cl <- paste0("polyclid_", poly_primitive_type(x))
  x <- list(x)
  class(x) <- c(cl, "polyclid_geometry")
  x
}

is_poly_geom <- function(x) inherits(x, "polyclid_geometry")

#' @importFrom euclid is_geometry
#' @export
is_geometry.polyclid_geometry <- function(x) TRUE

#' @importFrom euclid geometry_type
#' @export
geometry_type.polyclid_geometry <- function(x) {
  if (is_polygon(x)) "polygon"
  else "polyline"
}
#' @importFrom euclid cardinality
#' @export
cardinality.polyclid_geometry <- function(x, split = FALSE, ...) {
  if (split && is_polygon(x)) {
    polygon_sub_cardinality(get_ptr(x))
  } else {
    poly_cardinality(get_ptr(x))
  }
}
#' @export
as.matrix.polyclid_geometry <- function(x, ...) {
  poly_to_matrix(get_ptr(x))
}
#' @export
as.character.polyclid_geometry <- function(x, ...) {
  format(x, ...)
}
#' @export
as.list.polyclid_geometry <- function(x, ...) {
  lapply(seq_along(x), function(i) x[i])
}
#' @export
format.polyclid_geometry <- function(x, ...) {
  poly_format(get_ptr(x))
}
#' @export
print.polyclid_geometry <- function(x, ...) {
  cat("<", dim(x), "D ", sub("polyclid_(.*)\\d", "\\1", class(x)[1]), "s [", length(x), "]>\n", sep = "")
  if (length(x) == 0) {
    cat("[empty]")
  } else {
    print(format(x), quote = FALSE)
  }
  invisible(x)
}
#' @importFrom utils str
#' @export
str.polyclid_geometry <- function(object, ...) {
  show <- min(5, length(object))
  cat(
    sub("polyclid_(.*)\\d", "\\1", class(object)[1]), "{", dim(object), "}",
    if (length(object) == 0) " [0]" else paste0(" [1:", length(object), "] "),
    if (length(object) == 0) "" else paste(format(object)[seq_len(show)], collapse = " "),
    if (show < length(object)) " ..." else "",
    sep = ""
  )
  invisible(object)
}
#' @export
length.polyclid_geometry <- function(x) {
  poly_length(get_ptr(x))
}
#' @export
rep.polyclid_geometry <- function(x, ...) {
  index <- rep(seq_along(x), ...)
  x[index]
}
#' @export
dim.polyclid_geometry <- function(x) {
  2L
}
#' @export
`[.polyclid_geometry` <- function(x, i, j, ..., drop = TRUE) {
  index <- seq_along(x)[i]
  restore_poly_vector(poly_subset(get_ptr(x), index), x)
}
#' @export
`[[.polyclid_geometry` <- function(x, i) {
  if (length(i) != 1) {
    cli_abort("attempt to select more than one element in vector")
  }
  x[i]
}
#' @export
`[<-.polyclid_geometry` <- function(x, i, j, ..., value) {
  if (!is_poly_geom(value)) {
    cli_abort("Only geometries can be assigned to geometry vectors")
  }
  if (is.numeric(i) && all(i >= 0)) {
    index <- seq_len(max(i))[i]
  } else {
    index <- seq_along(x)[i]
  }
  if (length(index) == 0) {
    return(x)
  }
  if (anyNA(index)) {
    cli_abort("Trying to assign to non-existing element")
  }
  value <- rep_len(value, length(index))
  restore_poly_vector(poly_assign(get_ptr(x), index, get_ptr(value)), x)
}
#' @export
`[[<-.polyclid_geometry` <- function(x, i, value) {
  if (length(i) != 1) {
    cli_abort("attempt to assign to more than one element in vector")
  }
  x[i] <- value
  x
}
#' @export
`$.polyclid_geometry` <- function(x, name) {
  cli_abort("{.code $} is not defined for geometries")
}
#' @export
`$<-.polyclid_geometry` <- function(x, name, value) {
  cli_abort("{.code $<-} is not defined for geometries")
}
#' @export
c.polyclid_geometry <- function(..., recursive = FALSE) {
  input <- list(...)
  if (any(!vapply(input, inherits, logical(1), class(input[[1]])[1]))) {
    cli_abort("Geometries can only be combined with other geometries of the same type")
  }
  input <- lapply(input, get_ptr)
  res <- poly_combine(input[[1]], input[-1])
  restore_poly_vector(res, ..1)
}
#' @export
is.na.polyclid_geometry <- function(x) {
  poly_is_na(get_ptr(x))
}
#' @export
`is.na<-.polyclid_geometry` <- function(x, value) {
  x[is.na(x)] <- value
  x
}
#' @export
anyNA.polyclid_geometry <- function(x, recursive) {
  poly_any_na(get_ptr(x))
}
#' @export
as.data.frame.polyclid_geometry <- function(x, row.names = NULL, optional = FALSE, ...) {
  df <- list(x)
  class(df) <- "data.frame"
  attr(df, "row.names") <- .set_row_names(length(x))
  df
}
#' @importFrom euclid as_affine_transformation
#' @export
transform.polyclid_geometry <- function(`_data`, transformation, ...) {
  transformation <- as_affine_transformation(transformation)
  restore_poly_vector(poly_transform(get_ptr(`_data`), transformation), `_data`)
}
#' @importFrom euclid project
#' @export
project.polyclid_geometry <- function(x, target, ...) {
  vert(x) <- project(vert(x), target, ...)
  x
}
#' @importFrom euclid as_bbox
#' @export
as_bbox.polyclid_geometry <- function(x) {
  poly_bbox(get_ptr(x))
}
#' @importFrom euclid def
#' @export
def.polyclid_geometry <- function(x, name, which = NULL, ...) {
  def_names <- definition_names(x)
  if (length(name) != 1) {
    cli_abort("Can't get more than a single definition at a time")
  }
  if (is.character(name)) {
    index <- match(name, def_names)
    if (is.na(index)) {
      cli_abort(c(
        "{.val {name}} does not name a definition of the geometry",
        i = "Use one of {.or {.val {def_names}}}"
      ))
    }
  } else {
    index <- as.integer(name)
    if (is.na(index)) {
      cli_abort(paste0("{.arg name} must be either a string or a value convertible to a scalar integer"))
    }
  }
  if (index < 1 || index > length(def_names)) {
    cli_abort(c(
      "{.arg name} must reference a definitions for the geometry",
      i = "Use a a positive integer less than or equal to {length(def_names)}"
    ))
  }
  which <- as.integer(which)
  if (anyNA(which)) {
    # Hack to make NULL appear as value
    cli_abort("{.arg which} must be either {.val {factor('NULL')}} or a vector of finite integers")
  }
  if (any(which > cardinality(x))) {
    cli_abort("{.arg which} cannot be larger than the cardinality of the geometry")
  }
  poly_definition(get_ptr(x), index - 1L, which - 1L)
}
#' @importFrom euclid def<- as_exact_numeric
#' @export
`def<-.polyclid_geometry` <- function(x, name, which = NULL, ..., value) {
  def_names <- definition_names(x)
  if (length(name) != 1) {
    cli_abort("Can't set more than a single definition at a time")
  }
  if (is.character(name)) {
    index <- match(name, def_names)
    if (is.na(index)) {
      cli_abort(c(
        "{.val {name}} does not name a definition of the geometry",
        i = "Use one of {.or {.val {def_names}}}"
      ))
    }
  } else {
    index <- as.integer(name)
    if (is.na(index)) {
      cli_abort(paste0("{.arg name} must be either a string or a value convertible to a scalar integer"))
    }
  }
  if (index < 1 || index > length(def_names)) {
    cli_abort(c(
      "{.arg name} must reference a definitions for the geometry",
      i = "Use a a positive integer less than or equal to {length(def_names)}"
    ))
  }

  which <- as.integer(which)
  if (anyNA(which)) {
    # Hack to make NULL appear as value
    cli_abort("{.arg which} must be either {.val {factor('NULL')}} or a vector of finite integers")
  }
  if (any(which > cardinality(x))) {
    cli_abort("{.arg which} cannot be larger than the cardinality of the geometry")
  }

  new_poly_vector(
    poly_set_definition(get_ptr(x), index - 1L, which - 1L, as_exact_numeric(value))
  )
}
#' @importFrom euclid definition_names
#' @export
definition_names.polyclid_geometry <- function(x, ...) {
  poly_definition_names(get_ptr(x))
}
#' @importFrom euclid vert
#' @export
vert.polyclid_geometry <- function(x, which = NULL, ...) {
  which <- as.integer(which)
  if (anyNA(which)) {
    # Hack to make NULL appear as value
    cli_abort("{.arg which} must be either {.val {factor('NULL')}} or a vector of finite integers")
  }
  if (any(which > cardinality(x))) {
    cli_abort("{.arg which} cannot be larger than the cardinality of the geometry")
  }
  which <- which - 1L
  poly_vertex(get_ptr(x), which)
}
#' @importFrom euclid vert<- is_point
#' @export
`vert<-.polyclid_geometry` <- function(x, which = NULL, ..., value) {
  if (!is_point(value)) {
    cli_abort("New vertices must be given as points")
  }
  if (dim(x) != dim(value)) {
    cli_abort("{.arg x} and {.arg value} must have the same number of dimensions")
  }
  which <- as.integer(which)
  if (anyNA(which)) {
    # Hack to make NULL appear as value
    cli_abort("{.arg which} must be either {.val {factor('NULL')}} or a vector of finite integers")
  }
  if (any(which > cardinality(x))) {
    cli_abort("{.arg which} cannot be larger than the cardinality of the geometry")
  }
  which <- which - 1L
  new_poly_vector(poly_set_vertex(get_ptr(x), which, value))
}
#' @importFrom euclid edge
#' @export
edge.polyclid_geometry <- function(x, which = NULL, ...) {
  which <- as.integer(which)
  if (anyNA(which)) {
    # Hack to make NULL appear as value
    cli_abort("{.arg which} must be either {.val {factor('NULL')}} or a vector of finite integers")
  }
  if (any(which > edge_count(x))) {
    cli_abort("{.arg which} cannot be larger than the edge count of the geometry")
  }
  which <- which - 1L
  poly_edges(get_ptr(x), which)
}
#' @importFrom euclid edge_count
#' @export
edge_count.polyclid_geometry <- function(x) {
  poly_n_edges(get_ptr(x))
}
#' @export
unique.polyclid_geometry <- function(x, incomparables = FALSE, ...) {
  restore_poly_vector(poly_unique(get_ptr(x)), x)
}
#' @export
duplicated.polyclid_geometry <- function(x, incomparables = FALSE, ...) {
  poly_duplicated(get_ptr(x))
}
#' @export
anyDuplicated.polyclid_geometry <- function(x, incomparables = FALSE, ...) {
  poly_any_duplicated(get_ptr(x))
}
#' @export
Ops.polyclid_geometry <- function(e1, e2) {
  if (.Generic %in% c("==", "!=") && (length(e1) == 0 || length(e2) == 0)) {
    return(e1[integer(0)])
  }
  switch(.Generic,
    "==" = poly_is_equal(get_ptr(e1), get_ptr(e2)),
    "!=" = !poly_is_equal(get_ptr(e1), get_ptr(e2)),
    cli_abort("The {.code {.Generic}} operator is not defined for {.cls polyclid_geometry} vectors")
  )
}
