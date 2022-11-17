#pragma once

#include "cgal_types.h"

// length_impl -----------------------------------------------------------------
template<typename IT>
double path_length(IT start, IT end, bool close) {
  double len = 0;
  double last = 0;
  for (IT it = start; it != end; it++) {
    last = CGAL::sqrt(CGAL::to_double(it->squared_length().exact()));
    len += last;
  }
  if (!close) {
    len -= last;
  }
  return len;
}

template<typename T>
inline double length_impl(const T& geo) {
  return R_NaReal;
}
template<>
inline double length_impl<Polyline>(const Polyline& geo) {
  return path_length(geo.edges_begin(), geo.edges_end(), false);
}
template<>
inline double length_impl<Polygon>(const Polygon& geo) {
  double len = path_length(geo.outer_boundary().edges_begin(), geo.outer_boundary().edges_end(), true);
  for (auto it = geo.holes_begin(); it != geo.holes_end(); it++) {
    len += path_length(it->edges_begin(), it->edges_end(), true);
  }
  return len;
}

// area_impl -------------------------------------------------------------------

template<typename T>
inline double area_impl(const T& geo) {
  return R_NaReal;
}
template<>
inline double area_impl<Polygon>(const Polygon& geo) {
  auto area = geo.outer_boundary().area();
  for (auto it = geo.holes_begin(); it != geo.holes_end(); it++) {
    area -= it->area();
  }
  return CGAL::to_double(area.exact());
}
