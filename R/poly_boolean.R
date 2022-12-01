#' Boolean set operations on polygons and polylines
#'
#' polyclid extents the set of boolean set operations from euclid to work with
#' polylines and polygons or sets of these. As boolean operations has the
#' capacity to produce multiple polygons or poylines from a single one, the
#' output from boolean operations will always be a [polygon_set] or
#' [polyline_set] even if the input are [polygon] or [polyline] vectors. In
#' fact the polygon and polyline versions of these operations are simple
#' wrappers that converts their input to their respective set versions first.
#'
#' # Operations between polygons
#' When set operations are performed between polygons and/or polygon sets (or
#' euclid geometries that can be considered polygons: circles, triangles and iso
#' rectangles), the output is always a [polygon_set], even if the result is a
#' single polygon. This is done both to ensure a known output type, but also
#' because polygon sets are the internal representation used during boolean
#' operations and returning the native representation is thus more performant.
#'
#' - `union()` will combine two polygons into one (if they touch or overlaps) or
#'   insert them into the same set if they are disjoint.
#' - `intersection()` will return the area where the two inputs are interior
#'   joint (overlapping).
#' - `difference()` will remove the overlapping areas from the polygon(s) first
#'   given in the first argument.
#' - `symmetric_difference()` will return the areas of the inputs that are not
#'   overlapping.
#' - `complement()` returns the inverse of the input, i.e. the areas that were
#'   outside of the input polygon(s) will be inside the returned polygon.
#'
#' If only one argument is given, the cumulative operation is performed on the
#' input. For `union()` and `intersection()` this can be done in a single
#' aggregate operation whereas for `difference()` and `symmetric_difference()`
#' are done one at a time.
#'
#' # Operations between polylines
#' Boolean set operations between polylines and/or polyline sets (or segments)
#' will always return polyline sets. These sets can be a mix of polylines and
#' isolated vertices depending on the input.
#'
#' - `union()` will combine the polylines into a single set. Lines that are
#'   meeting at their end points will not be merged, but can be extracted with
#'   `as_polyline(..., simplify = TRUE)`. The reason for this is that merging
#'   these lines eagerly is bad for performance and will not alter the topology
#'   of the set. Further, the set already splits polylines into x monotone parts
#'   so there is not a 1-to-1 relationship between polylines and polyline sets
#'   either way.
#' - `intersection()` will return the points where the inputs are crossing along
#'   with the line segments where they are overlapping.
#' - `difference()` will insert vertices in the first polyline (set) where the
#'   inputs are crossing and remove line segments that overlap.
#' - `symmetric_difference()` will return those parts from either input that do
#'   not overlap
#'
#' contrary to polygons, there is not a single argument version for polylines,
#' mainly because the need is not really there.
#'
#' # Operations between polylines and polygons
#' When doing operations between polygons and polylines the interpretation of
#' what should happen is unclear. The following definitions are therefore mainly
#' what makes sense in the context of this package. The return value is always a
#' polyline set, but depending on the operation, it may be the faces of this set
#' that has interpretational relevance.
#'
#' - `union()` will combine the polylines and polygons into a single set. Where
#'   they overlap the polyline interior to the polygon will be removed as it's
#'   point set is covered by that of the polygon
#' - `intersection()` will return the polyline part that lies inside the polygon
#'   as that represent the shared point set.
#' - `difference()` if the polyline (set) is the first argument then the parts
#'   overlapping the polygon will be removed. If the polygon (set) is the first
#'   argument the polygon(s) will be cut by the into two or more by the
#'   polyline(s).
#' - `symmetric_difference()` will return the polylines and polygons overlayed
#'   into a single polygon set. For many this is what they assume the union to
#'   be.
#'
#' @name poly_bool
#' @rdname poly_bool
NULL

## Polygon Set

#' @importFrom euclid complement
#' @export
complement.polyclid_polygon_set <- function(x, ...) {
  new_poly_vector(polygonset_complement(get_ptr(x)))
}
#' @importFrom euclid union is_segment
#' @export
union.polyclid_polygon_set <- function(x, y, ...) {
  if (missing(y)) {
    new_poly_vector(polygonset_cum_union(get_ptr(x)))
  } else {
    if (is_polyline(y) || is_polyline_set(y) || is_segment(y)) {
      union(y, x, ...)
    } else {
      y <- as_polygon_set(y)
      new_poly_vector(polygonset_union(get_ptr(x), get_ptr(y)))
    }
  }
}
#' @importFrom euclid intersection is_segment
#' @export
intersection.polyclid_polygon_set <- function(x, y, ...) {
  if (missing(y)) {
    new_poly_vector(polygonset_cum_intersection(get_ptr(x)))
  } else {
    if (is_polyline(y) || is_polyline_set(y) || is_segment(y)) {
      union(y, x, ...)
    } else {
      y <- as_polygon_set(y)
      new_poly_vector(polygonset_intersection(get_ptr(x), get_ptr(y)))
    }
  }
}
#' @importFrom euclid difference is_segment
#' @export
difference.polyclid_polygon_set <- function(x, y, ...) {
  if (missing(y)) {
    new_poly_vector(polygonset_cum_difference(get_ptr(x)))
  } else {
    if (is_polyline(y) || is_polyline_set(y) || is_segment(y)) {
      y <- as_polyline_set(y)
      new_poly_vector(polygonset_polylineset_difference(get_ptr(y), get_ptr(x)))
    } else {
      y <- as_polygon_set(y)
      new_poly_vector(polygonset_difference(get_ptr(x), get_ptr(y)))
    }
  }
}
#' @importFrom euclid symmetric_difference
#' @export
symmetric_difference.polyclid_polygon_set <- function(x, y, ...) {
  if (missing(y)) {
    new_poly_vector(polygonset_cum_symmetric_difference(get_ptr(x)))
  } else {
    if (is_polyline(y) || is_polyline_set(y) || is_segment(y)) {
      symmetric_difference(y, x, ...)
    } else {
      y <- as_polygon_set(y)
      new_poly_vector(polygonset_symmetric_difference(get_ptr(x), get_ptr(y)))
    }
  }
}
#' @importFrom euclid has_intersection
#' @export
has_intersection.polyclid_polygon_set <- function(x, y, ...) {
  y <- as_polygon_set(y)
  new_poly_vector(polygonset_do_intersect(get_ptr(x), get_ptr(y)))
}

## Polygon

#' @importFrom euclid complement
#' @export
complement.polyclid_polygon <- function(x, ...) {
  complement(as_polygon_set(x))
}
#' @importFrom euclid union
#' @export
union.polyclid_polygon <- function(x, y, ...) {
  union(as_polygon_set(x), y)
}
#' @importFrom euclid intersection
#' @export
intersection.polyclid_polygon <- function(x, y, ...) {
  intersection(as_polygon_set(x), y)
}
#' @importFrom euclid difference
#' @export
difference.polyclid_polygon <- function(x, y, ...) {
  difference(as_polygon_set(x), y)
}
#' @importFrom euclid symmetric_difference
#' @export
symmetric_difference.polyclid_polygon <- function(x, y, ...) {
  symmetric_difference(as_polygon_set(x), y)
}
#' @importFrom euclid has_intersection
#' @export
has_intersection.polyclid_polygon <- function(x, y, ...) {
  has_intersection(as_polygon_set(x), y)
}

## Polyline Set

#' @importFrom euclid union is_triangle is_iso_rect is_circle
#' @export
union.polyclid_polyline_set <- function(x, y, ...) {
  if (missing(y)) {
    new_poly_vector(polylineset_cum_union(get_ptr(x)))
  } else {
    if (is_polygon(y) || is_polygon_set(y) || is_triangle(y) || is_iso_rect(y) || is_circle(y)) {
      y <- as_polygon_set(y)
      new_poly_vector(polylineset_polygonset_union(get_ptr(x), get_ptr(y)))
    } else {
      y <- as_polyline_set(y)
      new_poly_vector(polylineset_union(get_ptr(x), get_ptr(y)))
    }
  }
}
#' @importFrom euclid intersection is_triangle is_iso_rect is_circle
#' @export
intersection.polyclid_polyline_set <- function(x, y, ...) {
  if (missing(y)) {
    new_poly_vector(polylineset_cum_intersection(get_ptr(x)))
  } else {
    if (is_polygon(y) || is_polygon_set(y) || is_triangle(y) || is_iso_rect(y) || is_circle(y)) {
      y <- as_polygon_set(y)
      new_poly_vector(polylineset_polygonset_intersection(get_ptr(x), get_ptr(y)))
    } else {
      y <- as_polyline_set(y)
      new_poly_vector(polylineset_intersection(get_ptr(x), get_ptr(y)))
    }
  }
}
#' @importFrom euclid difference is_triangle is_iso_rect is_circle
#' @export
difference.polyclid_polyline_set <- function(x, y, ...) {
  if (missing(y)) {
    new_poly_vector(polylineset_cum_difference(get_ptr(x)))
  } else {
    if (is_polygon(y) || is_polygon_set(y) || is_triangle(y) || is_iso_rect(y) || is_circle(y)) {
      y <- as_polygon_set(y)
      new_poly_vector(polylineset_polygonset_difference(get_ptr(x), get_ptr(y)))
    } else {
      y <- as_polyline_set(y)
      new_poly_vector(polylineset_difference(get_ptr(x), get_ptr(y)))
    }
  }
}
#' @importFrom euclid symmetric_difference
#' @export
symmetric_difference.polyclid_polyline_set <- function(x, y, ...) {
  if (missing(y)) {
    new_poly_vector(polylineset_cum_symmetric_difference(get_ptr(x)))
  } else {
    if (is_polygon(y) || is_polygon_set(y) || is_triangle(y) || is_iso_rect(y) || is_circle(y)) {
      y <- as_polygon_set(y)
      new_poly_vector(polylineset_polygonset_symmetric_difference(get_ptr(x), get_ptr(y)))
    } else {
      y <- as_polyline_set(y)
      new_poly_vector(polylineset_symmetric_difference(get_ptr(x), get_ptr(y)))
    }
  }
}
#' @importFrom euclid has_intersection
#' @export
has_intersection.polyclid_polyline_set <- function(x, y, ...) {
  y <- as_polyline_set(y)
  new_poly_vector(polygonset_do_intersect(get_ptr(x), get_ptr(y)))
}

## Polyline

#' @importFrom euclid union
#' @export
union.polyclid_polyline <- function(x, y, ...) {
  union(as_polyline_set(x), y)
}
#' @importFrom euclid intersection
#' @export
intersection.polyclid_polyline <- function(x, y, ...) {
  intersection(as_polyline_set(x), y)
}
#' @importFrom euclid difference
#' @export
difference.polyclid_polyline <- function(x, y, ...) {
  difference(as_polyline_set(x), y)
}
#' @importFrom euclid symmetric_difference
#' @export
symmetric_difference.polyclid_polyline <- function(x, y, ...) {
  symmetric_difference(as_polyline_set(x), y)
}
#' @importFrom euclid has_intersection
#' @export
has_intersection.polyclid_polyline <- function(x, y, ...) {
  has_intersection(as_polyline_set(x), y)
}
