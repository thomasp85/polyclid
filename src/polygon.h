#pragma once

#include <cpp11/strings.hpp>
#include "poly_vector.h"
#include "polygon_predicates.h"
#include "polygon_modification.h"

class polygon : public poly_vector<Polygon> {
public:
  using poly_vector::poly_vector;
  ~polygon() = default;

  Primitive poly_type() const { return POLYGON; }

  cpp11::writable::strings def_names() const {
    return {"x", "y"};
  }

  std::string format_elem(int i) const {
    Polygon p = _storage[i];
    if (p.is_na()) {
      return "<NA>";
    }
    size_t b_size = p.outer_boundary().size();
    size_t n_holes = p.number_of_holes();
    if (n_holes == 0 && b_size == 0) {
      return "[empty]";
    }
    std::ostringstream f;
    f << std::setprecision(3);
    f << "[Boundary: " << b_size;
    if (b_size != 0) {
      Bbox_2 bb = p.outer_boundary().bbox();
      f << ", Range: <<" << bb.xmin() << ", " << bb.ymin() << ">, <" <<bb.xmax() << ", " << bb.ymax() << ">>";
    }
    f << ", Holes: " << n_holes << "]";
    return f.str();
  }

  size_t cardinality(size_t i) const {
    Polygon p = _storage[i];
    Polyline pl = p.outer_boundary();
    size_t n = pl.size();
    for (auto it = p.holes_begin(); it != p.holes_end(); ++it) {
      n += it->size();
    }
    return n;
  }

  cpp11::writable::integers sub_cardinality() const {
    cpp11::writable::integers res;
    for (int i = 0; i < size(); ++i) {
      Polygon p = _storage[i];
      Polyline pl = p.outer_boundary();
      res.push_back(pl.size());
      for (auto it = p.holes_begin(); it != p.holes_end(); ++it) {
        res.push_back(it->size());
      }
    }
    return res;
  }

  size_t n_edges(size_t i) const {
    return cardinality(i);
  }

  Point_2 get_elem(size_t i, size_t j) const {
    Polygon p = _storage[i];
    Polyline pl = p.outer_boundary();
    Point_2 elem = Point_2::NA_value();
    if (j < pl.size()) {
      elem = pl.vertex(j);
    } else {
      j -= pl.size();
      for (auto iter = p.holes_begin(); iter != p.holes_end(); iter++) {
        if (j < iter->size()) {
          elem = iter->vertex(j);
          break;
        }
        j -= iter->size();
      }
    }
    return elem;
  }

  Segment_2 get_edge(size_t i, size_t j) const {
    Polygon p = _storage[i];
    Polyline pl = p.outer_boundary();
    if (j >= pl.size()) {
      j -= pl.size();
      auto pl_p = p.holes_begin();
      while (j - pl_p->size() >= 0) {
        j -= pl_p->size();
        pl_p++;
      }
      pl = *pl_p;
    }
    return pl.edge(j);
  }

  cpp11::writable::integers n_holes() const {
    cpp11::writable::integers res;
    res.reserve(size());
    for (int i = 0; i < size(); ++i) {
      res.push_back(_storage[i].number_of_holes());
    }
    return res;
  }

  cpp11::writable::logicals is_unbounded() const {
    cpp11::writable::logicals res;
    for (size_t i = 0; i < size(); ++i) {
      if (_storage[i].is_na()) {
        res.push_back(NA_LOGICAL);
      } else {
        res.push_back((Rboolean) _storage[i].is_unbounded());
      }
    }
    return res;
  }

  std::vector<Polygon> get_boundary() const {
    std::vector<Polygon> res;
    res.reserve(size());
    for (int i = 0; i < size(); ++i) {
      if (_storage[i].is_unbounded()) {
        res.push_back(with_NA<Polygon>::NA_value());
        continue;
      }
      Polygon p(_storage[i].outer_boundary());
      res.push_back(p);
    }
    return res;
  }

  std::vector<Polygon> set_boundary(const std::vector<Polygon>& new_boundary) const {
    std::vector<Polygon> res;
    if (size() == 0) {
      return res;
    }
    res.reserve(size());
    for (int i = 0; i < size(); ++i) {
      if (_storage[i].is_na() || new_boundary[i % new_boundary.size()].is_na()) {
        res.push_back(with_NA<Polygon>::NA_value());
        continue;
      }
      Polygon p(new_boundary[i % new_boundary.size()].outer_boundary());
      for (auto iter = _storage[i].holes_begin(); iter != _storage[i].holes_end(); iter++) {
        p.add_hole(*iter);
      }
      p.set_flag(VALIDITY_CHECKED, false);
      res.push_back(p);
    }
    return res;
  }
  std::vector<Polygon> get_hole(const cpp11::integers& which) const {
    std::vector<Polygon> res;
    if (size() == 0) {
      return res;
    }
    res.reserve(size());
    for (int i = 0; i < size(); ++i) {
      size_t index = which[i % which.size()];
      if (_storage[i].is_na() || index == R_NaInt) {
        res.push_back(with_NA<Polygon>::NA_value());
        continue;
      }
      if (_storage[i].number_of_holes() <= index) {
        res.push_back(with_NA<Polygon>::NA_value());
        continue;
      }
      Polygon hole(_storage[i].holes()[index]);
      res.push_back(hole);
    }
    return res;
  }
  std::vector<Polygon> remove_hole(const cpp11::integers& which) const {
    std::vector<Polygon> res;
    if (size() == 0) {
      return res;
    }
    res.reserve(size());
    for (int i = 0; i < size(); ++i) {
      size_t index = which[i % which.size()];
      if (_storage[i].is_na() || index == R_NaInt) {
        res.push_back(with_NA<Polygon>::NA_value());
        continue;
      }
      Polygon p(_storage[i]);
      if (p.number_of_holes() <= index) {
        res.push_back(with_NA<Polygon>::NA_value());
        continue;
      }
      auto h_it = p.holes_begin() + index;
      p.erase_hole(h_it);
      p.set_flag(VALIDITY_CHECKED, false);
      res.push_back(p);
    }
    return res;
  }
  std::vector<Polygon> add_hole(const std::vector<Polygon>& new_hole) const {
    std::vector<Polygon> res;
    if (size() == 0) {
      return res;
    }
    res.reserve(size());
    for (int i = 0; i < size(); ++i) {
      if (_storage[i].is_na()) {
        res.push_back(with_NA<Polygon>::NA_value());
        continue;
      }
      Polygon p(_storage[i % size()]);
      if (new_hole[i % new_hole.size()]) {
        p.add_hole(new_hole[i % new_hole.size()].outer_boundary());
      }
      p.set_flag(VALIDITY_CHECKED, false);
      res.push_back(p);
    }
    return res;
  }
  std::vector<Polygon> set_hole(cpp11::integers which, const std::vector<Polygon>& new_hole) const {
    std::vector<Polygon> res;
    if (size() == 0) {
      return res;
    }
    res.reserve(size());
    for (int i = 0; i < size(); ++i) {
      if (_storage[i].is_na() || new_hole[i % new_hole.size()].is_na()) {
        res.push_back(with_NA<Polygon>::NA_value());
        continue;
      }
      Polygon p(_storage[i]);
      int j = which[i % which.size()];
      if (j < p.number_of_holes()) {
        p.holes()[j] = new_hole[i % new_hole.size()].outer_boundary();
      }
      p.set_flag(VALIDITY_CHECKED, false);
      res.push_back(p);
    }
    return res;
  }

  // Predicates
  cpp11::writable::logicals is_valid() {
    cpp11::writable::logicals res;
    res.reserve(size());
    for (size_t i = 0; i < size(); ++i) {
      if (_storage[i].is_na()) {
        res.push_back(NA_LOGICAL);
      } else {
        res.push_back((Rboolean) polygon_is_valid_impl(_storage[i]));
      }
    }
    return res;
  }
  cpp11::writable::logicals is_clockwise() const {
    cpp11::writable::logicals res;
    res.reserve(size());
    for (size_t i = 0; i < size(); ++i) {
      if (_storage[i].is_na()) {
        res.push_back(NA_LOGICAL);
      } else {
        res.push_back((Rboolean) polygon_is_clockwise_impl(_storage[i]));
      }
    }
    return res;
  }
  cpp11::writable::logicals is_counterclockwise() const {
    cpp11::writable::logicals res;
    res.reserve(size());
    for (size_t i = 0; i < size(); ++i) {
      if (_storage[i].is_na()) {
        res.push_back(NA_LOGICAL);
      } else {
        res.push_back((Rboolean) polygon_is_counterclockwise_impl(_storage[i]));
      }
    }
    return res;
  }
  cpp11::writable::logicals is_convex() const {
    cpp11::writable::logicals res;
    res.reserve(size());
    for (size_t i = 0; i < size(); ++i) {
      if (_storage[i].is_na()) {
        res.push_back(NA_LOGICAL);
      } else {
        res.push_back((Rboolean) polygon_is_convex_impl(_storage[i]));
      }
    }
    return res;
  }
  cpp11::writable::logicals is_simple() const {
    cpp11::writable::logicals res;
    res.reserve(size());
    for (size_t i = 0; i < size(); ++i) {
      if (_storage[i].is_na()) {
        res.push_back(NA_LOGICAL);
      } else {
        res.push_back((Rboolean) polygon_is_simple_impl(_storage[i]));
      }
    }
    return res;
  }
  cpp11::writable::logicals is_relatively_simple() const {
    cpp11::writable::logicals res;
    res.reserve(size());
    for (size_t i = 0; i < size(); ++i) {
      if (_storage[i].is_na()) {
        res.push_back(NA_LOGICAL);
      } else {
        res.push_back((Rboolean) polygon_is_relatively_simple_impl(_storage[i]));
      }
    }
    return res;
  }
  cpp11::writable::logicals is_collinear() const {
    cpp11::writable::logicals res;
    res.reserve(size());
    for (size_t i = 0; i < size(); ++i) {
      if (_storage[i].is_na()) {
        res.push_back(NA_LOGICAL);
      } else {
        res.push_back((Rboolean) polygon_is_collinear_impl(_storage[i]));
      }
    }
    return res;
  }

  // Modifications
  poly_vector_base_p make_valid() {
    std::vector<Polygon> res;
    res.reserve(size());

    for (size_t i = 0; i < size(); ++i) {
      if (_storage[i].is_na()) {
        res.push_back(Polygon::NA_value());
      } else {
        res.push_back(polygon_make_valid_impl(_storage[i]));
      }
    }
    return create_poly_vector(res);
  }
  poly_vector_base_p reverse_orientation() const {
    std::vector<Polygon> res;
    res.reserve(size());

    for (size_t i = 0; i < size(); ++i) {
      if (_storage[i].is_na()) {
        res.push_back(Polygon::NA_value());
      } else {
        res.push_back(polygon_reverse_orientation_impl(_storage[i]));
      }
    }
    return create_poly_vector(res);
  }
  poly_vector_base_p connect_holes() {
    std::vector<Polygon> res;
    res.reserve(size());

    for (size_t i = 0; i < size(); ++i) {
      if (_storage[i].is_na()) {
        res.push_back(Polygon::NA_value());
      } else {
        res.push_back(polygon_connect_holes_impl(_storage[i]));
      }
    }
    return create_poly_vector(res);
  }
};

typedef cpp11::external_pointer<polygon> polygon_p;
