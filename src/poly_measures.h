#pragma once

#include "cgal_types.h"

// length_impl -----------------------------------------------------------------
template<typename IT>
double path_length(IT start, IT end) {
  double len = 0;
  for (IT it = start; it != end; it++) {
    len += CGAL::sqrt(CGAL::to_double(it->squared_length().exact()));
  }
  return len;
}

template<typename T>
inline double length_impl(const T& geo) {
  return R_NaReal;
}
template<>
inline double length_impl<Polyline>(const Polyline& geo) {
  return path_length(geo.edges_begin(), geo.edges_end());
}
template<>
inline double length_impl<Polygon>(const Polygon& geo) {
  double len = path_length(geo.outer_boundary().edges_begin(), geo.outer_boundary().edges_end());
  for (auto it = geo.holes_begin(); it != geo.holes_end(); it++) {
    len += path_length(it->edges_begin(), it->edges_end());
  }
  return len;
}
template<>
inline double length_impl<Polygon_set>(const Polygon_set& geo) {
  double len = 0;
  for (auto iter = geo.arrangement().edges_begin(); iter != geo.arrangement().edges_end(); iter++) {
    Segment_2 s(iter->curve());
    len += CGAL::sqrt(CGAL::to_double(s.squared_length().exact()));
  }
  return len;
}
template<>
inline double length_impl<Polyline_set>(const Polyline_set& geo) {
  double len = 0;
  for (auto iter = geo.edges_begin(); iter != geo.edges_end(); iter++) {
    for (auto s_iter = iter->curve().subcurves_begin(); s_iter != iter->curve().subcurves_end(); s_iter++) {
      Segment_2 s(*s_iter);
      len += CGAL::sqrt(CGAL::to_double(s.squared_length().exact()));
    }
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
template<>
inline double area_impl<Polygon_set>(const Polygon_set& geo) {
  std::vector<Polygon> ps;
  geo.polygons_with_holes(std::back_inserter(ps));
  double area = 0;
  for (auto iter = ps.begin(); iter != ps.end(); iter++) {
    area += area_impl(*iter);
  }
  return area;
}
