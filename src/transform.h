#pragma once

#include "cgal_types.h"

template<typename T>
inline T transform_impl(const T& geo, const Aff_transformation_2& trans) {
  return geo;
}

template<>
inline Polyline transform_impl<Polyline>(const Polyline& geo, const Aff_transformation_2& trans) {
  return CGAL::transform(trans, geo);
}

template<>
inline Polygon transform_impl<Polygon>(const Polygon& geo, const Aff_transformation_2& trans) {
  Polygon transformed(CGAL::transform(trans, geo.outer_boundary()));
  for (auto it = geo.holes_begin(); it != geo.holes_end(); ++it) {
    transformed.add_hole(CGAL::transform(trans, *it));
  }
  return transformed;
}
