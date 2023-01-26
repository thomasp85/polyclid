#pragma once

#include "cgal_types.h"
#include <cpp11/protect.hpp>

template<typename T>
inline T vert_insert_impl(const T& geo, const Polyline& verts, int at) {
  return geo;
}

template<>
inline Polyline vert_insert_impl(const Polyline& geo, const Polyline& verts, int at) {
  if (at < 0 || size_t(at) > geo.size()) return geo;
  Polyline new_geo(geo);
  new_geo.insert(std::next(new_geo.vertices_begin(), at), verts.vertices_begin(), verts.vertices_end());
  return new_geo;
}

template<>
inline Polygon vert_insert_impl(const Polygon& geo, const Polyline& verts, int at) {
  if (at < 0) return geo;
  Polygon new_geo(geo);
  auto iter = new_geo.outer_boundary().vertices_begin();
  if (at < new_geo.outer_boundary().size()) {
    new_geo.outer_boundary().insert(std::next(iter, at), verts.vertices_begin(), verts.vertices_end());
  } else if (!new_geo.has_holes()) {
    iter = new_geo.outer_boundary().vertices_end();
    new_geo.outer_boundary().insert(iter, verts.vertices_begin(), verts.vertices_end());
  } else {
    at -= new_geo.outer_boundary().size();
    for (auto h_iter = new_geo.holes_begin(); h_iter != new_geo.holes_end(); h_iter++) {
      if (at < h_iter->size()) {
        iter = h_iter->vertices_begin();
        h_iter->insert(std::next(iter, at), verts.vertices_begin(), verts.vertices_end());
      } else {
        at -= h_iter->size();
      }
    }
  }
  return new_geo;
}
