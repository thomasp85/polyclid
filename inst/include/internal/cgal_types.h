#pragma once

#include <euclid.h>

#include <CGAL/Polygon_2.h>
#include <CGAL/Polygon_with_holes_2.h>
#include <CGAL/Boolean_set_operations_2.h>
#include <CGAL/Polygon_set_2.h>
#include <CGAL/Arr_segment_traits_2.h>
#include <CGAL/Arr_polyline_traits_2.h>
#include <CGAL/Arrangement_2.h>

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
typedef with_NA< CGAL::Polygon_set_2<Kernel> > Polygon_set;
typedef CGAL::Arr_polyline_traits_2< CGAL::Arr_segment_traits_2<Kernel> > Polyline_trait;
typedef CGAL::Arrangement_2<Polyline_trait> Polyline_2;
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
};
