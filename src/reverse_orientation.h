#pragma once

#include <cpp11/R.hpp>
#include "cgal_types.h"

template<typename T>
inline T poly_reverse_orientation_impl(const T& pol) {
  return pol;
}

template<>
inline Polygon poly_reverse_orientation_impl(const Polygon& pol) {
  Polygon new_pol(pol);
  new_pol.set_flag(VALIDITY_CHECKED, false);
  new_pol.outer_boundary().reverse_orientation();
  for (auto iter = new_pol.holes_begin(); iter != new_pol.holes_end(); iter++) {
    iter->reverse_orientation();
  }
  return new_pol;
}

template<>
inline Polyline poly_reverse_orientation_impl(const Polyline& pol) {
  Polyline new_pol(pol);
  new_pol.reverse_orientation();
  return new_pol;
}
