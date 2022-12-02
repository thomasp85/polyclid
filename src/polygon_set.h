#pragma once

#include <cpp11/strings.hpp>
#include "poly_vector.h"
#include "polygon_predicates.h"
#include "polygon_modification.h"

#include <CGAL/Boolean_set_operations_2.h>

class polygon_set : public poly_vector<Polygon_set> {
public:
  using poly_vector::poly_vector;
  ~polygon_set() = default;

  Primitive poly_type() const { return POLYGON_SET; }

  cpp11::writable::strings def_names() const {
    return {"p"};
  }

  std::string format_elem(int i) const {
    Polygon_set p = _storage[i];
    if (p.is_na()) {
      return "<NA>";
    }
    size_t p_size = p.number_of_polygons_with_holes();
    if (p.is_empty()) {
      return "[empty]";
    }
    std::ostringstream f;
    f << std::setprecision(3);
    f << "[# Polygons: " << p_size;
    Bbox_2 bb = p.bbox();
    if (bb.is_na()) {
      f << ", Range: <Undefined>]";
    } else {
      f << ", Range: <<" << bb.xmin() << ", " << bb.ymin() << ">, <" <<bb.xmax() << ", " << bb.ymax() << ">>]";
    }
    return f.str();
  }

  size_t cardinality(size_t i) const {
    return _storage[i].arrangement().number_of_vertices();
  }

  size_t n_edges(size_t i) const {
    return _storage[i].arrangement().number_of_edges();
  }

  Point_2 get_elem(size_t i, size_t j) const {
    auto iter = _storage[i].arrangement().vertices_begin();
    for (size_t k = 0; k < j; ++k) {
      iter++;
    }
    return iter->point();
  }

  Segment_2 get_edge(size_t i, size_t j) const {
    auto iter = _storage[i].arrangement().edges_begin();
    for (size_t k = 0; k < j; ++k) {
      iter++;
    }
    return {iter->curve()};
  }

  cpp11::writable::integers n_polygons() const {
    cpp11::writable::integers res;
    res.reserve(size());
    for (size_t i = 0; i < size(); ++i) {
      res.push_back(_storage[i].number_of_polygons_with_holes());
    }
    return res;
  }

  poly_vector_base_p get_polygon(cpp11::integers i) const {
    std::vector<Polygon> res;
    res.reserve(size());

    for (size_t j = 0; j < size(); ++j) {
      Polygon_set p = _storage[j];
      int ii = i[j % i.size()];
      if (p.is_na()) {
        res.push_back(Polygon::NA_value());
        continue;
      }
      std::vector<Polygon> polygons;
      p.polygons_with_holes(std::back_inserter(polygons));
      if (ii >= polygons.size()) {
        res.push_back(Polygon::NA_value());
        continue;
      }
      res.push_back(polygons[ii]);
    }

    for (auto iter = res.begin(); iter != res.end(); iter++) {
      iter->set_flag(VALIDITY_CHECKED, true);
      iter->set_flag(IS_VALID, true);
    }

    return create_poly_vector(res);
  }

  poly_vector_base_p get_all_polygon() const {
    std::vector<Polygon> res;
    res.reserve(size());

    for (size_t j = 0; j < size(); ++j) {
      Polygon_set p = _storage[j];
      if (p.is_na()) {
        continue;
      }
      p.polygons_with_holes(std::back_inserter(res));
    }

    for (auto iter = res.begin(); iter != res.end(); iter++) {
      iter->set_flag(VALIDITY_CHECKED, true);
      iter->set_flag(IS_VALID, true);
    }

    return create_poly_vector(res);
  }

  poly_vector_base_p complement() const {
    std::vector<Polygon_set> res;
    res.reserve(size());

    for (size_t i = 0; i < size(); ++i) {
      Polygon_set p = _storage[i];
      if (p.is_na()) {
        res.push_back(Polygon_set::NA_value());
        continue;
      }
      p.complement();
      res.push_back(p);
    }

    return create_poly_vector(res);
  }

  cpp11::writable::logicals do_intersect(cpp11::external_pointer<polygon_set> other) const {
    size_t final_size = std::max(size(), other->size());
    cpp11::writable::logicals res;
    res.reserve(final_size);

    for (size_t i = 0; i < final_size; ++i) {
      if (_storage[i % size()].is_na() || (*other)[i % other->size()].is_na()) {
        res.push_back(NA_LOGICAL);
        continue;
      }
      Polygon_set p = _storage[i % size()];
      res.push_back((Rboolean) p.do_intersect((*other)[i % other->size()]));
    }

    return res;
  }

  poly_vector_base_p p_union(cpp11::external_pointer<polygon_set> other) const {
    size_t final_size = std::max(size(), other->size());
    std::vector<Polygon_set> res;
    res.reserve(final_size);

    for (size_t i = 0; i < final_size; ++i) {
      if (_storage[i % size()].is_na() || (*other)[i % other->size()].is_na()) {
        res.push_back(Polygon_set::NA_value());
        continue;
      }
      Polygon_set p = _storage[i % size()];
      p.join((*other)[i % other->size()]);
      res.push_back(p);
    }

    return create_poly_vector(res);
  }

  poly_vector_base_p p_intersection(cpp11::external_pointer<polygon_set> other) const {
    size_t final_size = std::max(size(), other->size());
    std::vector<Polygon_set> res;
    res.reserve(final_size);

    for (size_t i = 0; i < final_size; ++i) {
      if (_storage[i % size()].is_na() || (*other)[i % other->size()].is_na()) {
        res.push_back(Polygon_set::NA_value());
        continue;
      }
      Polygon_set p = _storage[i % size()];
      p.intersection((*other)[i % other->size()]);
      res.push_back(p);
    }

    return create_poly_vector(res);
  }

  poly_vector_base_p p_difference(cpp11::external_pointer<polygon_set> other) const {
    size_t final_size = std::max(size(), other->size());
    std::vector<Polygon_set> res;
    res.reserve(final_size);

    for (size_t i = 0; i < final_size; ++i) {
      if (_storage[i % size()].is_na() || (*other)[i % other->size()].is_na()) {
        res.push_back(Polygon_set::NA_value());
        continue;
      }
      Polygon_set p = _storage[i % size()];
      p.difference((*other)[i % other->size()]);
      res.push_back(p);
    }

    return create_poly_vector(res);
  }

  poly_vector_base_p p_symmetric_difference(cpp11::external_pointer<polygon_set> other) const {
    size_t final_size = std::max(size(), other->size());
    std::vector<Polygon_set> res;
    res.reserve(final_size);

    for (size_t i = 0; i < final_size; ++i) {
      if (_storage[i % size()].is_na() || (*other)[i % other->size()].is_na()) {
        res.push_back(Polygon_set::NA_value());
        continue;
      }
      Polygon_set p = _storage[i % size()];
      p.symmetric_difference((*other)[i % other->size()]);
      res.push_back(p);
    }

    return create_poly_vector(res);
  }

  poly_vector_base_p cum_union() const {
    std::vector<Polygon_set> res;
    if (size() != 0) {
      if (any_na()) {
        res.push_back(Polygon_set::NA_value());
      } else {
        Polygon_set p = _storage[0];
        for (size_t i = 1; i < size(); ++i) {
          p.join(_storage[i]);
        }
        res.push_back(p);
      }
    }
    return create_poly_vector(res);
  }

  poly_vector_base_p cum_intersection() const {
    std::vector<Polygon_set> res;
    if (size() != 0) {
      if (any_na()) {
        res.push_back(Polygon_set::NA_value());
      } else {
        Polygon_set p = _storage[0];
        for (size_t i = 1; i < size(); ++i) {
          p.intersection(_storage[i]);
        }
        res.push_back(p);
      }
    }
    return create_poly_vector(res);
  }

  poly_vector_base_p cum_difference() const {
    std::vector<Polygon_set> res;
    if (size() != 0) {
      if (any_na()) {
        res.push_back(Polygon_set::NA_value());
      } else {
        Polygon_set p = _storage[0];
        for (size_t i = 1; i < size(); ++i) {
          p.difference(_storage[i]);
        }
        res.push_back(p);
      }
    }
    return create_poly_vector(res);
  }

  poly_vector_base_p cum_symmetric_difference() const {
    std::vector<Polygon_set> res;
    if (size() != 0) {
      if (any_na()) {
        res.push_back(Polygon_set::NA_value());
      } else {
        Polygon_set p = _storage[0];
        for (size_t i = 1; i < size(); ++i) {
          p.symmetric_difference(_storage[i]);
        }
        res.push_back(p);
      }
    }
    return create_poly_vector(res);
  }

  poly_vector_base_p locate(SEXP points) {
    std::vector<Point_2> p_vec = euclid::get_point_2_vec(points);
    std::vector<Polygon> res;
    res.reserve(size());
    for (size_t i = 0; i < size(); ++i) {
      if (_storage[i].is_na() || p_vec[i % p_vec.size()].is_na()) {
        res.push_back(Polygon::NA_value());
      }
      Polygon located;
      if (_storage[i].locate(p_vec[i % p_vec.size()], located)) {
        res.push_back(located);
      } else {
        res.push_back(Polygon::NA_value());
      }
    }

    return create_poly_vector(res);
  }
};

typedef cpp11::external_pointer<polygon_set> polygon_set_p;
