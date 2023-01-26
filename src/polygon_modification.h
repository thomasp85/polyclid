#pragma once

#include <algorithm>

#include <cpp11/R.hpp>
#include "cgal_types.h"
#include "polygon_predicates.h"
#include "degenerate.h"

#include <CGAL/Boolean_set_operations_2.h>
#include <CGAL/connect_holes.h>

inline Polyline remove_degenerate(const Polyline& pol) {
  std::vector<Point_2> p_vec(pol.vertices_begin(), pol.vertices_end());
  p_vec.erase(std::unique(p_vec.begin(), p_vec.end()), p_vec.end());
  if (p_vec.front() == p_vec.back()) p_vec.pop_back();
  return {p_vec.begin(), p_vec.end()};
}

inline Polygon polygon_make_valid_impl(Polygon& pol) {
  if (polygon_is_valid_impl(pol) || pol.is_na()) {
    return pol;
  }
  Polygon_set S;
  Polyline p = remove_degenerate(pol.outer_boundary());
  if (!p.is_empty()) {
    if (!polygon_is_relatively_simple_impl(p)) {
      return Polygon::NA_value();
    }
    if (p.is_clockwise_oriented()) {
      p.reverse_orientation();
    }
    S.insert(p);
  }
  for (auto iter = pol.holes_begin(); iter != pol.holes_end(); iter++) {
    if (iter->is_empty()) continue;
    p = remove_degenerate(*iter);
    if (!polygon_is_relatively_simple_impl(p)) {
      return Polygon::NA_value();
    }
    if (p.is_clockwise_oriented()) {
      p.reverse_orientation();
    }
    S.difference(p);
  }
  if (S.number_of_polygons_with_holes() > 1) {
    cpp11::warning("Making this polygon valid splits it into multiple polygons. Only returning the first");
  }
  std::vector<Polygon> res;
  S.polygons_with_holes(std::back_inserter(res));
  res[0].set_flag(VALIDITY_CHECKED, true);
  res[0].set_flag(IS_VALID, true);

  return res[0];
}

inline Polygon polygon_reverse_orientation_impl(const Polygon& pol) {
  Polygon new_pol(pol);
  new_pol.set_flag(VALIDITY_CHECKED, false);
  new_pol.outer_boundary().reverse_orientation();
  for (auto iter = new_pol.holes_begin(); iter != new_pol.holes_end(); iter++) {
    iter->reverse_orientation();
  }
  return new_pol;
}

inline Polygon polygon_connect_holes_impl(Polygon& pol) {
  if (pol.is_na() || pol.is_unbounded()) {
    return Polygon::NA_value();
  }
  Polygon pol2 = pol;
  if (!polygon_is_valid_impl(pol)) {
    pol2 = polygon_make_valid_impl(pol);
  }
  std::vector<Point_2> res;
  CGAL::connect_holes(pol2, std::back_inserter(res));
  Polyline new_boundary(res.begin(), res.end());
  Polygon new_poly(new_boundary);
  return new_poly;
}
