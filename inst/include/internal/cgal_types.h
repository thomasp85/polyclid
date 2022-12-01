#pragma once

#include <euclid.h>

#include <CGAL/Polygon_2.h>
#include <CGAL/Polygon_with_holes_2.h>
#include <CGAL/Boolean_set_operations_2.h>
#include <CGAL/Polygon_set_2.h>
#include <CGAL/Arr_segment_traits_2.h>
#include <CGAL/Arr_polyline_traits_2.h>
#include <CGAL/Arrangement_2.h>
#include <CGAL/Arr_extended_dcel.h>

enum Polygon_Flags {
  VALIDITY_CHECKED,
  IS_VALID
};

typedef CGAL::Gps_segment_traits_2<Kernel> Segment_trait;
class Polyline : public with_NA<Segment_trait::Polygon_2> {
public:
  using with_NA::with_NA;
  bool operator==(const Polyline& other) const {
    if (this == &other) return true;
    if (size() == 0 && other.size() == 0) return true;
    if (size() != other.size()) return false;
    for (size_t i = 0; i < size(); ++i) {
      if (vertex(i) != other.vertex(i)) return false;
    }
    return true;
  }
  Edge_const_iterator edges_end() const {
    return with_NA<Segment_trait::Polygon_2>::edges_end() - 1;
  }
  void reverse_orientation() {
    if (size() <= 1) return;
    std::reverse(container().begin(), container().end());
  }
};
typedef with_NA< Segment_trait::Polygon_with_holes_2 > Polygon;
class Polygon_set : public with_NA< CGAL::Polygon_set_2<Kernel> > {
public:
  using with_NA::with_NA;
  bool operator==(const Polygon_set& other) const {
    return false;
  }
  Bbox_2 bbox() const {
    if (is_na() || is_empty() || is_plane()) {
      return Bbox_2::NA_value();
    }
    std::vector<Polygon> polygons;
    size_t n = number_of_polygons_with_holes();
    polygons.reserve(n);
    polygons_with_holes(std::back_inserter(polygons));
    Bbox_2 bb = polygons[0].bbox();
    for (size_t i = 1; i < n; ++i) {
      bb += polygons[i].bbox();
    }
    return bb;
  }
};
typedef CGAL::Arr_polyline_traits_2< CGAL::Arr_segment_traits_2<Kernel> > Polyline_trait;
typedef CGAL::Arr_extended_dcel<Polyline_trait, int, int, int> Dcel;
typedef CGAL::Arrangement_2<Polyline_trait, Dcel> Polyline_2;
class Polyline_set : public with_NA<Polyline_2> {
public:
  Polyline_set() : with_NA<Polyline_2>() {}
  Polyline_set(const Polyline_2& copy) : with_NA<Polyline_2>(copy) {}
  template<typename Iter>
  Polyline_set(Iter begin, Iter end) {
    auto constructor = this->traits()->construct_curve_2_object();
    auto pl = constructor(begin, end);
    CGAL::insert(*this, pl);
  }
  bool operator==(const Polyline_set& other) const {
    return false;
  }
  Bbox_2 bbox() const {
    if (is_na() || is_empty()) {
      return Bbox_2::NA_value();
    }
    Bbox_2 bb;
    for (auto iter = edges_begin(); iter != edges_end(); iter++) {
      bb += iter->curve().bbox();
    }
    return bb;
  }
  template<typename Iter>
  void insert(Iter begin, Iter end) {
    auto constructor = this->traits()->construct_curve_2_object();
    auto pl = constructor(begin, end);
    CGAL::insert(*this, pl);
  }
  void set_flag(int flag) {
    for (auto iter = halfedges_begin(); iter != halfedges_end(); iter++) {
      iter->set_data(flag);
    }
    for (auto iter = vertices_begin(); iter != vertices_end(); iter++) {
      iter->set_data(flag);
    }
    for (auto iter = faces_begin(); iter != faces_end(); iter++) {
      iter->set_data(iter->is_unbounded() ? -1 : flag);
    }
  }
  void remove_with_flag(int flag) {
    for (auto iter = edges_begin(); iter != edges_end(); iter++) {
      if (iter->data() == flag) remove_edge(iter);
    }
    for (auto iter = vertices_begin(); iter != vertices_end(); iter++) {
      if (iter->is_isolated() && iter->data() == flag) remove_isolated_vertex(iter);
    }
  }
  void remove_without_flag(int flag) {
    for (auto iter = edges_begin(); iter != edges_end(); iter++) {
      if (iter->data() != flag) remove_edge(iter);
    }
    for (auto iter = vertices_begin(); iter != vertices_end(); iter++) {
      if (iter->is_isolated() && iter->data() != flag) remove_isolated_vertex(iter);
    }
  }
};
