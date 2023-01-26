#pragma once

#include <cpp11/R.hpp>
#include <CGAL/partition_2.h>
#include <CGAL/Partition_traits_2.h>
#include "cgal_types.h"
#include "polygon_modification.h"

typedef CGAL::Partition_traits_2<Kernel> Traits;

inline Polyline_set convex_partition(Polygon& poly, bool optimal, bool triangulate) {
  if (!(poly.get_flag(VALIDITY_CHECKED) && poly.get_flag(IS_VALID))) {
    cpp11::stop("Input polygons must be valid");
  }
  bool has_holes = poly.has_holes();
  std::vector<Traits::Polygon_2> partitions;
  if(!has_holes && poly.outer_boundary().is_convex()) {
    partitions.emplace_back(poly.outer_boundary().vertices_begin(), poly.outer_boundary().vertices_end());
  } else {
    Polyline input = poly.outer_boundary();
    if (has_holes) {
      input = polygon_connect_holes_impl(poly).outer_boundary();
    }
    if (optimal) {
      CGAL::optimal_convex_partition_2(input.vertices_begin(), input.vertices_end(), std::back_inserter(partitions));
    } else {
      CGAL::greene_approx_convex_partition_2(input.vertices_begin(), input.vertices_end(), std::back_inserter(partitions));
    }
  }
  std::vector<Segment_2> segs;
  for (auto iter = partitions.begin(); iter != partitions.end(); iter++) {
    for (auto seg_iter = iter->edges_begin(); seg_iter != iter->edges_end(); seg_iter++) {
      segs.emplace_back(*seg_iter);
    }
    if (iter->size() > 3 && triangulate) {
      auto start = iter->vertices_begin();
      auto end = std::prev(iter->vertices_end(), 1);
      for (auto iter = std::next(start, 2); iter != end; iter++) {
        segs.emplace_back(*start, *iter);
      }
    }
  }
  Polyline_set res;
  res.insert_separate(segs.begin(), segs.end());
  return res;
}

inline Polyline_set monotone_partition(Polygon& poly) {
  if (!(poly.get_flag(VALIDITY_CHECKED) && poly.get_flag(IS_VALID))) {
    cpp11::stop("Input polygons must be valid");
  }
  bool has_holes = poly.has_holes();
  std::vector<Traits::Polygon_2> partitions;
  Polyline input = poly.outer_boundary();
  if (has_holes) {
    input = polygon_connect_holes_impl(poly).outer_boundary();
  }
  CGAL::y_monotone_partition_2(input.vertices_begin(), input.vertices_end(), std::back_inserter(partitions));
  std::vector<Segment_2> segs;
  for (auto iter = partitions.begin(); iter != partitions.end(); iter++) {
    for (auto seg_iter = iter->edges_begin(); seg_iter != iter->edges_end(); seg_iter++) {
      segs.push_back(*seg_iter);
    }
  }
  Polyline_set res;
  res.insert_separate(segs.begin(), segs.end());
  return res;
}
