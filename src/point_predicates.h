#include <cpp11/R.hpp>
#pragma once

#include "cgal_types.h"
#include "polygon_predicates.h"

#include <CGAL/Boolean_set_operations_2.h>
#include <CGAL/enum.h>

inline CGAL::Oriented_side get_location(Polygon& geo, const Point_2& p) {
  if (!polygon_is_valid_impl(geo)) {
    cpp11::stop("input geometry is not a valid polygon");
  }
  Polygon_set S;
  S.insert(geo);
  return S.oriented_side(p);
}

// has_inside_impl -------------------------------------------------------------

template<typename T>
inline int has_inside_impl(const T& geo, const Point_2& p) {
  return NA_LOGICAL;
}
template<>
inline int has_inside_impl<Polygon>(const Polygon& geo, const Point_2& p) {
  if ((geo.get_flag(VALIDITY_CHECKED) && !geo.get_flag(IS_VALID)) || !geo.outer_boundary().is_simple()) {
    return NA_LOGICAL;
  }
  bool is_inside = geo.is_unbounded() || geo.outer_boundary().has_on_bounded_side(p);
  if (!is_inside) return 0;
  for (auto hole = geo.holes_begin(); hole != geo.holes_end(); hole++) {
    if (!hole->is_simple()) return NA_LOGICAL;
    if (hole->has_on_bounded_side(p)) return 0;
  }
  return 1;
}

// has_on_impl -----------------------------------------------------------------

template<typename T>
inline int has_on_impl(T& geo, const Point_2& p) {
  return NA_LOGICAL;
}
template<>
inline int has_on_impl<Polygon>(Polygon& geo, const Point_2& p) {
  return get_location(geo, p) == CGAL::ON_ORIENTED_BOUNDARY;
}
template<>
inline int has_on_impl<Polyline>(Polyline& geo, const Point_2& p) {
  if (geo.is_empty()) return 0;
  Bbox_2 bbox = geo.bbox();
  if (bbox.xmin() > p.x() || bbox.xmax() < p.x() || bbox.ymin() > p.y() || bbox.ymax() < p.y()) {
    return 0;
  }
  for (auto iter = geo.edges_begin(); iter != geo.edges_end(); iter++) {
    if (iter->has_on(p)) return 1;
  }
  return 0;
}
template<>
inline int has_on_impl<Polygon_set>(Polygon_set& geo, const Point_2& p) {
  return geo.oriented_side(p) == CGAL::ON_ORIENTED_BOUNDARY;
}

// has_outside_impl ------------------------------------------------------------

template<typename T>
inline int has_outside_impl(const T& geo, const Point_2& p) {
  return NA_INTEGER;
}
template<>
inline int has_outside_impl<Polygon>(const Polygon& geo, const Point_2& p) {
  if (geo.number_of_holes() != 0) {
    cpp11::warning("Ignoring holes when checking location. Use `has_on_positive_side()` to include them");
  }
  if ((geo.get_flag(VALIDITY_CHECKED) && geo.get_flag(IS_VALID)) || !geo.outer_boundary().is_simple()) {
    return NA_LOGICAL;
  }
  return geo.outer_boundary().has_on_unbounded_side(p);
}

// has_on_positive_impl --------------------------------------------------------

template<typename T>
inline int has_on_positive_impl(T& geo, const Point_2& p) {
  return NA_LOGICAL;
}
template<>
inline int has_on_positive_impl<Polygon>(Polygon& geo, const Point_2& p) {
  return get_location(geo, p) == CGAL::ON_POSITIVE_SIDE;
}
template<>
inline int has_on_positive_impl<Polygon_set>(Polygon_set& geo, const Point_2& p) {
  return geo.oriented_side(p) == CGAL::ON_POSITIVE_SIDE;
}

// has_on_negative_impl --------------------------------------------------------

template<typename T>
inline int has_on_negative_impl(T& geo, const Point_2& p) {
  return NA_LOGICAL;
}
template<>
inline int has_on_negative_impl<Polygon>(Polygon& geo, const Point_2& p) {
  return get_location(geo, p) == CGAL::ON_NEGATIVE_SIDE;
}
template<>
inline int has_on_negative_impl<Polygon_set>(Polygon_set& geo, const Point_2& p) {
  return geo.oriented_side(p) == CGAL::ON_NEGATIVE_SIDE;
}
