#pragma once

#include "cgal_types.h"
#include <cpp11/protect.hpp>

template<typename T>
inline T set_definition_impl(const T& geometry, int which, int element, const Exact_number& value) {
  return geometry;
}

template<typename T>
inline T set_definition_all_impl(const T& geometry, int which, const std::vector<Exact_number>& values) {
  return geometry;
}

template<>
inline Polyline set_definition_impl(const Polyline& geometry, int which, int element, const Exact_number& value) {
  Polyline new_line(geometry);
  Point_2 mod_point = new_line[element];
  switch(which) {
  case 0:
    new_line[element] = Point_2(value.base(), mod_point.y());
    break;
  case 1:
    new_line[element] = Point_2(mod_point.x(), value.base());
    break;
  }
  return new_line;
}
template<>
inline Polyline set_definition_all_impl(const Polyline& geometry, int which, const std::vector<Exact_number>& values) {
  std::vector<Point_2> new_verts;
  new_verts.reserve(geometry.size());
  for (size_t i = 0; i < geometry.size(); ++i) {
    switch(which) {
    case 0:
      new_verts.emplace_back(values[i].base(), geometry[i].y());
      break;
    case 1:
      new_verts.emplace_back(geometry[i].x(), values[i].base());
      break;
    }
  }
  return Polyline(new_verts.begin(), new_verts.end());
}

template<>
inline Polygon set_definition_impl(const Polygon& geometry, int which, int element, const Exact_number& value) {
  Polygon new_poly(geometry);
  new_poly.set_flag(VALIDITY_CHECKED, false);
  Point_2 mod_point;
  if (element < new_poly.outer_boundary().size()) {
    mod_point = new_poly.outer_boundary()[element];
    switch(which) {
    case 0:
      new_poly.outer_boundary()[element] = Point_2(value.base(), mod_point.y());
      break;
    case 1:
      new_poly.outer_boundary()[element] = Point_2(value.base(), mod_point.y());
      break;
    }
    return new_poly;
  }
  element -= new_poly.outer_boundary().size();
  for (auto iter = new_poly.holes_begin(); iter != new_poly.holes_end(); iter++) {
    if (element < iter->size()) {
      mod_point = (*iter)[element];
      switch(which) {
      case 0:
        (*iter)[element] = Point_2(value.base(), mod_point.y());
        break;
      case 1:
        (*iter)[element] = Point_2(value.base(), mod_point.y());
        break;
      }
      break;
    }
    element -= iter->size();
  }
  return new_poly;
}
template<>
inline Polygon set_definition_all_impl(const Polygon& geometry, int which, const std::vector<Exact_number>& values) {
  size_t i = 0;
  std::vector<Point_2> container;
  for (; i < geometry.outer_boundary().size(); ++i) {
    switch(which) {
    case 0:
      container.emplace_back(values[i].base(), geometry.outer_boundary()[i].y());
      break;
    case 1:
      container.emplace_back(geometry.outer_boundary()[i].x(), values[i].base());
      break;
    }
  }
  Polygon new_poly({container.begin(), container.end()});
  new_poly.set_flag(VALIDITY_CHECKED, false);
  for (auto iter = geometry.holes_begin(); iter != geometry.holes_end(); iter++) {
    container.clear();
    for (size_t j = 0; j < iter->size(); ++i) {
      switch(which) {
      case 0:
        container.emplace_back(values[i + j].base(), (*iter)[i].y());
        break;
      case 1:
        container.emplace_back((*iter)[i].x(), values[i + j].base());
        break;
      }
    }
    new_poly.add_hole({container.begin(), container.end()});
    i += iter->size();
  }
  return new_poly;
}
