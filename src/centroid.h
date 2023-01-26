#pragma once

#include <cpp11/R.hpp>
#include <CGAL/partition_2.h>
#include <CGAL/Partition_traits_2.h>
#include "cgal_types.h"
#include "polygon_modification.h"

typedef CGAL::Partition_traits_2<Kernel> Traits;

template<typename T>
inline Point_2 centroid_impl(T& poly) {
  return Point_2::NA_value();
}

template<>
inline Point_2 centroid_impl(Polygon& poly) {
  bool has_holes = poly.has_holes();
  std::vector<Traits::Polygon_2> partitions;
  if(!has_holes && poly.outer_boundary().is_convex()) {
    partitions.emplace_back(poly.outer_boundary().vertices_begin(), poly.outer_boundary().vertices_end());
  } else {
    Polyline input = poly.outer_boundary();
    if (has_holes) {
      input = polygon_connect_holes_impl(poly).outer_boundary();
    }
    CGAL::greene_approx_convex_partition_2(input.vertices_begin(), input.vertices_end(), std::back_inserter(partitions));
  }
  Vector_2 center(0.0, 0.0);
  Exact_number total_weight = 0.0;
  for (auto iter = partitions.begin(); iter != partitions.end(); iter++) {
    auto source = iter->vertices_begin();
    auto end = std::prev(iter->vertices_end(), 1);
    for (auto v_iter = std::next(source, 1); v_iter != end; v_iter++) {
      Triangle_2 tri(*source, *v_iter, *(std::next(v_iter, 1)));
      Point_2 centroid = CGAL::centroid(tri);
      Exact_number area = tri.area();
      center += Vector_2(centroid.x(), centroid.y()) * area;
      total_weight += area;
    }
  }
  center /= total_weight;
  return {center.x(), center.y()};
}

template<>
inline Point_2 centroid_impl(Polyline& poly) {
  std::vector<double> lengths = {0};
  for (auto iter = poly.edges_begin(); iter != poly.edges_end(); iter++) {
    lengths.push_back(lengths.back() + CGAL::sqrt(CGAL::to_double(iter->squared_length().exact())));
  }
  double mid = lengths.back() * 0.5;
  size_t i = 1;
  for (auto iter = poly.edges_begin(); iter != poly.edges_end(); iter++) {
    if (lengths[i] > mid) {
      return iter->source() + Vector_2(*iter) * Exact_number((lengths[i] - mid) / (lengths[i] - lengths[i - 1]));
    }
    ++i;
  }
  return Point_2::NA_value();
}
