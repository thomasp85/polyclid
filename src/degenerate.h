#pragma once

#include "cgal_types.h"
#include <cpp11/protect.hpp>

template<typename T>
inline bool poly_is_degenerate_impl(T& poly) {
  return false;
}

template<>
inline bool poly_is_degenerate_impl(Polygon& poly) {
  for (auto iter = poly.outer_boundary().edges_begin(); iter != poly.outer_boundary().edges_end(); iter++) {
    if (iter->is_degenerate()) return true;
  }
  for (auto h_iter = poly.holes_begin(); h_iter != poly.holes_end(); h_iter++) {
    for (auto iter = h_iter->edges_begin(); iter != h_iter->edges_end(); iter++) {
      if (iter->is_degenerate()) return true;
    }
  }
  return false;
}

template<>
inline bool poly_is_degenerate_impl(Polyline& poly) {
  for (auto iter = poly.edges_begin(); iter != poly.edges_end(); iter++) {
    if (iter->is_degenerate()) return true;
  }
  return false;
}
