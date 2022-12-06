#pragma once

#include <cpp11/strings.hpp>
#include "poly_vector.h"
#include "polyline_predicates.h"

class polyline : public poly_vector<Polyline> {
public:
  using poly_vector::poly_vector;
  ~polyline() = default;

  Primitive poly_type() const { return POLYLINE; }

  cpp11::writable::strings def_names() const {
    return {"x", "y"};
  }

  std::string format_elem(int i) const {
    Polyline p = _storage[i];
    size_t b_size = p.size();
    if (b_size == 0) {
      return "[empty]";
    }
    std::ostringstream f;
    f << std::setprecision(3);
    f << "[Path: " << b_size;
    if (b_size != 0) {
      Bbox_2 bb = p.bbox();
      f << ", Range: <<" << bb.xmin() << ", " << bb.ymin() << ">, <" <<bb.xmax() << ", " << bb.ymax() << ">>";
    }
    f << "]";
    return f.str();
  }

  size_t cardinality(size_t i) const {
    return _storage[i].size();
  }

  size_t n_edges(size_t i) const {
    return _storage[i].size() - 1;
  }

  Point_2 get_elem(size_t i, size_t j) const {
    return _storage[i].vertex(j);
  }

  Segment_2 get_edge(size_t i, size_t j) const {
    return _storage[i].edge(j);
  }

  // Predicates
  cpp11::writable::logicals is_x_monotone() const {
    cpp11::writable::logicals res;
    res.reserve(size());
    for (size_t i = 0; i < size(); ++i) {
      if (_storage[i].is_na()) {
        res.push_back(NA_LOGICAL);
      } else {
        res.push_back((Rboolean) polyline_is_x_monotone_impl(_storage[i]));
      }
    }
    return res;
  }
  cpp11::writable::logicals is_y_monotone() const {
    cpp11::writable::logicals res;
    res.reserve(size());
    for (size_t i = 0; i < size(); ++i) {
      if (_storage[i].is_na()) {
        res.push_back(NA_LOGICAL);
      } else {
        res.push_back((Rboolean) polyline_is_y_monotone_impl(_storage[i]));
      }
    }
    return res;
  }
  cpp11::writable::logicals is_x_weakly_monotone() const {
    cpp11::writable::logicals res;
    res.reserve(size());
    for (size_t i = 0; i < size(); ++i) {
      if (_storage[i].is_na()) {
        res.push_back(NA_LOGICAL);
      } else {
        res.push_back((Rboolean) polyline_is_x_weakly_monotone_impl(_storage[i]));
      }
    }
    return res;
  }
  cpp11::writable::logicals is_y_weakly_monotone() const {
    cpp11::writable::logicals res;
    res.reserve(size());
    for (size_t i = 0; i < size(); ++i) {
      if (_storage[i].is_na()) {
        res.push_back(NA_LOGICAL);
      } else {
        res.push_back((Rboolean) polyline_is_y_weakly_monotone_impl(_storage[i]));
      }
    }
    return res;
  }
  cpp11::writable::logicals is_selfintersecting() const {
    cpp11::writable::logicals res;
    res.reserve(size());
    for (size_t i = 0; i < size(); ++i) {
      if (_storage[i].is_na()) {
        res.push_back(NA_LOGICAL);
      } else {
        res.push_back((Rboolean) polyline_is_selfintersecting_impl(_storage[i]));
      }
    }
    return res;
  }
  Polyline glue(bool na_rm) const {
    if (!na_rm && any_na()) return Polyline::NA_value();

    std::vector<Point_2> verts;
    for (size_t i = 0; i < size(); ++i) {
      if (_storage[i].is_na() || _storage[i].is_empty()) continue;

      auto begin = _storage[i].vertices_begin();
      auto end = _storage[i].vertices_end();
      if (begin == end) continue;
      if (verts.size() != 0 && verts.back() == *begin) begin++;
      verts.insert(verts.end(), begin, end);
    }
    return {verts.begin(), verts.end()};
  }
};

typedef cpp11::external_pointer<polyline> polyline_p;
