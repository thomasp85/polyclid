#pragma once

#include <cpp11/R.hpp>
#include "cgal_types.h"

inline int polyline_is_x_monotone_impl(const Polyline& poly) {
  Exact_number prev = poly.vertex(0).x();
  Exact_number curr = poly.vertex(1).x();
  if (prev == curr) {
    return false;
  }
  if (poly.size() < 3) return true;

  bool is_increasing = curr > prev;
  prev = curr;
  for (size_t i = 2; i < poly.size(); ++i) {
    curr = poly.vertex(i).x();
    if (prev == curr || (curr > prev) != is_increasing) {
      return false;
    }
    prev = curr;
  }
  return true;
}
inline int polyline_is_y_monotone_impl(const Polyline& poly) {
  Exact_number prev = poly.vertex(0).y();
  Exact_number curr = poly.vertex(1).y();
  if (prev == curr) {
    return false;
  }
  if (poly.size() < 3) return true;

  bool is_increasing = curr > prev;
  prev = curr;
  for (size_t i = 2; i < poly.size(); ++i) {
    curr = poly.vertex(i).y();
    if (prev == curr || (curr > prev) != is_increasing) {
      return false;
    }
    prev = curr;
  }
  return true;
}
inline int polyline_is_x_weakly_monotone_impl(const Polyline& poly) {
  Exact_number prev = poly.vertex(0).x();
  Exact_number curr = poly.vertex(1).x();
  if (poly.size() < 3) return true;

  size_t i = 2;
  while (prev == curr) {
    prev = curr;
    curr = poly.vertex(i).x();
    i++;
  }
  bool is_increasing = curr > prev;
  prev = curr;
  for (; i < poly.size(); ++i) {
    curr = poly.vertex(i).x();
    if (prev == curr) continue;
    if ((curr > prev) != is_increasing) {
      return false;
    }
    prev = curr;
  }
  return true;
}
inline int polyline_is_y_weakly_monotone_impl(const Polyline& poly) {
  Exact_number prev = poly.vertex(0).y();
  Exact_number curr = poly.vertex(1).y();
  if (poly.size() < 3) return true;

  size_t i = 2;
  while (prev == curr) {
    prev = curr;
    curr = poly.vertex(i).y();
    i++;
  }
  bool is_increasing = curr > prev;
  prev = curr;
  for (; i < poly.size(); ++i) {
    curr = poly.vertex(i).y();
    if (prev == curr) continue;
    if ((curr > prev) != is_increasing) {
      return false;
    }
    prev = curr;
  }
  return true;
}

inline int polyline_is_selfintersecting_impl(const Polyline& poly) {
  Polyline_set ps(poly.vertices_begin(), poly.vertices_end());
  return ps.number_of_faces() > 1;
}
