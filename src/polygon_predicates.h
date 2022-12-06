#pragma once

#include <cpp11/R.hpp>
#include <CGAL/Boolean_set_operations_2.h>
#include "cgal_types.h"

inline bool polygon_is_valid_impl(Polygon& pol) {
  if (pol.is_na()) return false;
  if (pol.get_flag(VALIDITY_CHECKED)) {
    return pol.get_flag(IS_VALID);
  }
  Segment_trait tr;
  bool valid = CGAL::is_valid_polygon_with_holes(pol, tr);
  pol.set_flag(VALIDITY_CHECKED, true);
  pol.set_flag(IS_VALID, valid);
  return valid;
}

inline int polygon_is_clockwise_impl(const Polygon& poly) {
  if (poly.is_na() || poly.outer_boundary().is_empty()) {
    return NA_LOGICAL;
  }
  if (!((poly.get_flag(VALIDITY_CHECKED) && poly.get_flag(IS_VALID)) || poly.outer_boundary().is_simple())) {
    return NA_LOGICAL;
  }
  return poly.outer_boundary().is_clockwise_oriented();
}

inline int polygon_is_counterclockwise_impl(const Polygon& poly) {
  if (poly.is_na() || poly.outer_boundary().is_empty()) {
    return NA_LOGICAL;
  }
  if (!((poly.get_flag(VALIDITY_CHECKED) && poly.get_flag(IS_VALID)) || poly.outer_boundary().is_simple())) {
    return NA_LOGICAL;
  }
  return poly.outer_boundary().is_counterclockwise_oriented();
}

inline int polygon_is_convex_impl(const Polygon& poly) {
  if (poly.is_na() || poly.outer_boundary().is_empty()) {
    return NA_LOGICAL;
  }
  return poly.outer_boundary().is_convex();
}
template<typename T>
inline int polygon_is_simple_impl(const T& poly) {
  return 0;
}
template<>
inline int polygon_is_simple_impl(const Polygon& poly) {
  if (poly.is_na()) {
    return NA_LOGICAL;
  }
  return poly.outer_boundary().is_simple();
}
template<>
inline int polygon_is_simple_impl(const Polyline& poly) {
  if (poly.is_na()) {
    return NA_LOGICAL;
  }
  return poly.is_simple();
}

template<typename T>
inline int polygon_is_relatively_simple_impl(const T& poly) {
  return NA_LOGICAL;
}
template<>
inline int polygon_is_relatively_simple_impl(const Polygon& poly) {
  if (poly.is_na()) {
    return NA_LOGICAL;
  }
  const Segment_trait tr;
  return CGAL::is_relatively_simple_polygon(poly.outer_boundary(), tr);
}
template<>
inline int polygon_is_relatively_simple_impl(const Polyline& poly) {
  if (poly.is_na()) {
    return NA_LOGICAL;
  }
  const Segment_trait tr;
  return CGAL::is_relatively_simple_polygon(poly, tr);
}

inline int polygon_is_collinear_impl(const Polygon& poly) {
  if (poly.is_na() || poly.outer_boundary().is_empty()) {
    return NA_LOGICAL;
  }
  if (!((poly.get_flag(VALIDITY_CHECKED) && poly.get_flag(IS_VALID)) || poly.outer_boundary().is_simple())) {
    return NA_LOGICAL;
  }
  return poly.outer_boundary().is_collinear_oriented();
}
