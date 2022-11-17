#pragma once

#include "cgal_types.h"
#include <cpp11/protect.hpp>

template<typename T>
inline T set_vertex_impl(const T& geometry, int which, const Point_2& value) {
  return geometry;
}

template<typename T>
inline T set_vertex_all_impl(const T& geometry, const std::vector<Point_2>& values) {
  return geometry;
}

template<>
inline Polyline set_vertex_impl(const Polyline& geometry, int which, const Point_2& value) {
  Polyline new_line(geometry);
  new_line[which] = value;
  return new_line;
}
template<>
inline Polyline set_vertex_all_impl(const Polyline& geometry, const std::vector<Point_2>& values) {
  return Polyline(values.begin(), values.end());
}

template<>
inline Polygon set_vertex_impl(const Polygon& geometry, int which, const Point_2& value) {
  Polygon new_poly(geometry);
  new_poly.set_flag(VALIDITY_CHECKED, false);
  if (which < new_poly.outer_boundary().size()) {
    new_poly.outer_boundary()[which] = value;
    return new_poly;
  }
  which -= new_poly.outer_boundary().size();
  for (auto iter = new_poly.holes_begin(); iter != new_poly.holes_end(); iter++) {
    if (which < iter->size()) {
      (*iter)[which] = value;
      break;
    }
    which -= iter->size();
  }
  return new_poly;
}
template<>
inline Polygon set_vertex_all_impl(const Polygon& geometry, const std::vector<Point_2>& values) {
  auto iter = values.begin();
  Polygon new_poly({iter, iter + geometry.outer_boundary().size()});
  new_poly.set_flag(VALIDITY_CHECKED, false);
  iter += geometry.outer_boundary().size();
  for (auto h_iter = geometry.holes_begin(); h_iter != geometry.holes_end(); h_iter++) {
    new_poly.add_hole({iter, iter + h_iter->size()});
    iter += h_iter->size();
  }
  return new_poly;
}
