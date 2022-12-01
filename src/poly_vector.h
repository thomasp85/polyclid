#pragma once

#include <vector>
#include <string>
#include <typeinfo>
#include <algorithm>
#include <cpp11/logicals.hpp>
#include <cpp11/doubles.hpp>
#include <cpp11/matrix.hpp>
#include <cpp11/strings.hpp>
#include <cpp11/r_string.hpp>
#include <cpp11/list.hpp>
#include <cpp11/list_of.hpp>
#include <cpp11/external_pointer.hpp>
#include <cpp11/integers.hpp>

#include <euclid.h>

#include "cgal_types.h"
#include "transform.h"
#include "poly_measures.h"
#include "set_vertex.h"
#include "set_definition.h"
#include "point_predicates.h"
#include "reverse_orientation.h"

enum Primitive {
  VIRTUAL,
  POLYGON,
  POLYLINE,
  POLYGON_SET,
  POLYLINE_SET
};

class poly_vector_base {
public:
  poly_vector_base() {}
  virtual ~poly_vector_base() = default;

  // Conversion
  virtual cpp11::writable::doubles_matrix<> as_numeric() const = 0;
  virtual cpp11::writable::strings format() const = 0;
  virtual Point_2 get_elem(size_t i, size_t j) const = 0;
  virtual Segment_2 get_edge(size_t i, size_t j) const = 0;
  virtual std::vector<double> get_row(size_t i, size_t j) const = 0;

  // Equality
  virtual cpp11::writable::logicals operator==(const poly_vector_base& other) const = 0;

  // Dimensions
  virtual size_t size() const = 0;
  virtual Primitive poly_type() const = 0;
  virtual cpp11::writable::strings def_names() const = 0;
  virtual std::vector<Exact_number> definition(int which, cpp11::integers element, bool get_all) const = 0;
  virtual cpp11::external_pointer<poly_vector_base> set_definition(int which, cpp11::integers element, bool set_all, const std::vector<Exact_number>& value) const = 0;
  virtual SEXP vertex(cpp11::integers which, bool get_all) const = 0;
  virtual cpp11::external_pointer<poly_vector_base> set_vertex(cpp11::integers which, bool set_all, const std::vector<Point_2>& value) const = 0;
  virtual SEXP edge(cpp11::integers which, bool get_all) const = 0;
  virtual Exact_number get_single_definition(size_t i, int which, int element) const = 0;
  virtual size_t cardinality(size_t i) const = 0;
  virtual size_t n_edges(size_t i) const = 0;
  virtual size_t long_length() const = 0;

  // Subsetting etc
  virtual cpp11::external_pointer<poly_vector_base> subset(cpp11::integers index) const = 0;
  virtual cpp11::external_pointer<poly_vector_base> copy() const = 0;
  virtual cpp11::external_pointer<poly_vector_base> assign(cpp11::integers index, const poly_vector_base& value) const = 0;
  virtual cpp11::external_pointer<poly_vector_base> combine(cpp11::list_of< cpp11::external_pointer<poly_vector_base> > extra) const = 0;

  // Self similarity
  virtual cpp11::external_pointer<poly_vector_base> unique() const = 0;
  virtual cpp11::writable::logicals duplicated() const = 0;
  virtual int any_duplicated() const = 0;
  //virtual cpp11::writable::integers match(const poly_vector_base& table) const = 0;

  // Missing
  virtual cpp11::writable::logicals is_na() const = 0;
  virtual bool any_na() const = 0;

  // Predicates
  virtual cpp11::writable::logicals is_degenerate() const = 0;
  virtual cpp11::writable::logicals has_inside(const SEXP points) const = 0;
  virtual cpp11::writable::logicals has_on(const SEXP points) = 0;
  virtual cpp11::writable::logicals has_outside(const SEXP points) const = 0;
  virtual cpp11::writable::logicals has_on_positive(const SEXP points) = 0;
  virtual cpp11::writable::logicals has_on_negative(const SEXP points) = 0;

  // Measures
  virtual cpp11::writable::doubles area() const = 0;
  virtual cpp11::writable::doubles length() const = 0;

  // Common
  virtual cpp11::external_pointer<poly_vector_base> transform(const std::vector<Aff_transformation_2> affine) const = 0;
  virtual SEXP bbox() const = 0;
  virtual cpp11::external_pointer<poly_vector_base> reverse_orientation() const = 0;
};
typedef cpp11::external_pointer<poly_vector_base> poly_vector_base_p;

// General constructor
template<typename T>
poly_vector_base_p create_poly_vector(std::vector<T>& input);
template<>
poly_vector_base_p create_poly_vector(std::vector<Polyline>& input);
template<>
poly_vector_base_p create_poly_vector(std::vector<Polygon>& input);
template<>
poly_vector_base_p create_poly_vector(std::vector<Polygon_set>& input);
template<>
poly_vector_base_p create_poly_vector(std::vector<Polyline_set>& input);

template<typename T>
poly_vector_base_p create_scalar_poly(const T& val) {
  std::vector<T> scalar = {val};
  return create_poly_vector(scalar);
}

// General extractors
template<typename T>
const std::vector<T> get_vector_of_poly(const poly_vector_base& geometries);
template<>
const std::vector<Polyline> get_vector_of_poly(const poly_vector_base& geometries);
template<>
const std::vector<Polygon> get_vector_of_poly(const poly_vector_base& geometries);
template<>
const std::vector<Polygon_set> get_vector_of_poly(const poly_vector_base& geometries);
template<>
const std::vector<Polyline_set> get_vector_of_poly(const poly_vector_base& geometries);

// poly_vector -------------------------------------------------------------

template <typename T>
class poly_vector : public poly_vector_base {

protected:
  std::vector<T> _storage;

public:
  poly_vector() {}
  // Construct without element copy - BEWARE!
  poly_vector(std::vector<T> content) {
    _storage.swap(content);
  }
  poly_vector(const poly_vector& copy) : _storage(copy._storage) {}
  poly_vector& operator=(const poly_vector& copy) const {
    _storage.clear();
    _storage.assign(_storage.end(), copy._storage.begin(), copy._storage.end());
    return *this;
  }
  ~poly_vector() = default;
  const std::vector<T>& get_storage() const { return _storage; }

  // Conversion
  std::vector<double> get_row(size_t i, size_t j) const {
    Point_2 p = get_elem(i, j);
    return {
      CGAL::to_double(p.x().exact()),
      CGAL::to_double(p.y().exact())
    };
  }
  Exact_number get_single_definition(size_t i, int which, int element) const {
    Point_2 p = get_elem(i, element);

    switch(which) {
    case 0: return p.x();
    case 1: return p.y();
    }
    return p.x();
  }
  cpp11::writable::doubles_matrix<> as_numeric() const {
    cpp11::writable::strings colnames = def_names();
    size_t ncols = colnames.size();
    cpp11::writable::doubles_matrix<> result(long_length(), ncols);

    size_t ii = 0;
    for (size_t i = 0; i < size(); ++i) {
      bool is_na = _storage[i].is_na();
      for (size_t j = 0; j < cardinality(i); ++j) {
        std::vector<double> row = get_row(i, j);
        for (size_t k = 0; k < ncols; ++k) {
          result(ii, k) = is_na ? R_NaReal : row[k];
        }
        ++ii;
      }
    }

    result.attr("dimnames") = cpp11::writable::list({R_NilValue, colnames});
    return result;
  }
  virtual std::string format_elem(int i) const {
    return "";
  }
  cpp11::writable::strings format() const {
    cpp11::writable::strings result(size());
    cpp11::writable::strings defnames = def_names();

    for (size_t i = 0; i < size(); ++i) {
      if (_storage[i].is_na()) {
        result[i] = "<NA>";
        continue;
      }
      result[i] = format_elem(i);
    }

    return result;
  }
  std::vector<Exact_number> definition(int which, cpp11::integers element, bool get_all) const {
    std::vector<Exact_number> result;
    result.reserve(get_all ? long_length() : size());

    for (size_t i = 0; i < size(); ++i) {
      if (get_all) {
        for (size_t j = 0; j < cardinality(i); ++j) {
          result.push_back(_storage[i] ? get_single_definition(i, which, j) : Exact_number::NA_value());
        }
      } else {
        result.push_back(_storage[i] ? get_single_definition(i, which, element[i]) : Exact_number::NA_value());
      }
    }

    return result;
  }
  poly_vector_base_p set_definition(int which, cpp11::integers element, bool set_all, const std::vector<Exact_number>& value) const {
    std::vector<T> result;
    result.reserve(size());

    int k = 0;
    for (size_t i = 0; i < size(); ++i) {
      if (_storage[i].is_na()) {
        result.push_back(T::NA_value());
        continue;
      }
      if (set_all) {
        std::vector<Exact_number> defs;
        bool any_na = false;
        for (size_t j = 0; j < cardinality(i); ++j) {
          defs.push_back(value[k % value.size()]);
          if (!defs.back()) any_na = true;
          k++;
        }
        result.push_back(any_na ? T::NA_value() : set_definition_all_impl(_storage[i], which, defs));
      } else {
        if (value[i % value.size()]) {
          result.push_back(set_definition_impl(_storage[i], which, element[i % element.size()], value[i % value.size()]));
        } else {
          result.push_back(T::NA_value());
        }
      }
    }

    return create_poly_vector(result);
  }
  SEXP vertex(cpp11::integers which, bool get_all) const {
    std::vector<Point_2> result;
    result.reserve(get_all ? long_length() : size());
    if (size() == 0) {
      return euclid::create_point_2_vec(result);
    }
    for (size_t i = 0; i < size(); ++i) {
      if (get_all) {
        for (size_t j = 0; j < cardinality(i); ++j) {
          result.push_back(_storage[i] ? get_elem(i, j) : Point_2::NA_value());
        }
      } else {
        result.push_back(_storage[i] ? get_elem(i, which[i % which.size()]) : Point_2::NA_value());
      }
    }
    return euclid::create_point_2_vec(result);
  }
  poly_vector_base_p set_vertex(cpp11::integers which, bool set_all, const std::vector<Point_2>& value) const {
    std::vector<T> result;
    if (size() == 0) {
      return create_poly_vector(result);
    }
    result.reserve(size());

    int k = 0;
    for (size_t i = 0; i < size(); ++i) {
      if (_storage[i].is_na()) {
        result.push_back(T::NA_value());
        continue;
      }
      if (set_all) {
        std::vector<Point_2> verts;
        bool any_na = false;
        for (size_t j = 0; j < cardinality(i); ++j) {
          verts.push_back(value[k % value.size()]);
          if (!verts.back()) any_na = true;
          k++;
        }
        result.push_back(any_na ? T::NA_value() : set_vertex_all_impl(_storage[i], verts));
      } else {
        if (!value[i % value.size()]) {
          result.push_back(T::NA_value());
        } else {
          result.push_back(set_vertex_impl(_storage[i], which[i % which.size()], value[i % value.size()]));
        }
      }
    }
    return create_poly_vector(result);
  }
  SEXP edge(cpp11::integers which, bool get_all) const {
    std::vector<Segment_2> segments;
    segments.reserve(size());
    for (size_t i = 0; i < size(); ++i) {
      if (get_all) {
        for (size_t j = 0; j < n_edges(i); ++j) {
          if (_storage[i].is_na()) {
            segments.push_back(Segment_2::NA_value());
          }
          segments.push_back(get_edge(i, j));
        }
      } else {
        if (_storage[i].is_na()) {
          segments.push_back(Segment_2::NA_value());
        }
        segments.push_back(get_edge(i, which[i % which.size()]));
      }
    }
    return euclid::create_segment_2_vec(segments);
  };

  // Equality
  cpp11::writable::logicals operator==(const poly_vector_base& other) const {
    if (size() == 0 || other.size() == 0) {
      return {};
    }
    size_t output_length = std::max(size(), other.size());

    cpp11::writable::logicals result(output_length);

    if (typeid(*this) != typeid(other)) {
      for (size_t i = 0; i < size(); ++i) {
        result[i] = (Rboolean) false;
      }
      return result;
    }

    auto other_vec = get_vector_of_poly<T>(other);

    for (size_t i = 0; i < output_length; ++i) {
      if (_storage[i % size()].is_na() || other_vec[i % other_vec.size()].is_na()) {
        result[i] = NA_LOGICAL;
        continue;
      }
      result[i] = (Rboolean) (_storage[i % size()] == other_vec[i % other_vec.size()]);
    }

    return result;
  }

  // Utility
  size_t size() const { return _storage.size(); }
  T operator[](size_t i) const { return _storage[i]; }
  void clear() { _storage.clear(); }
  void push_back(T element) { _storage.push_back(element); }
  size_t cardinality(size_t i) const { return 1; }
  size_t long_length() const {
    size_t len = 0;
    for (size_t i = 0; i < size(); ++i) {
      len += cardinality(i);
    }
    return len;
  }

  // Subsetting, assignment, combining etc
  poly_vector_base_p subset(cpp11::integers index) const {
    std::vector<T> new_storage;
    new_storage.reserve(index.size());
    for (R_xlen_t i = 0; i < index.size(); ++i) {
      if (index[i] == R_NaInt) {
        new_storage.push_back(T::NA_value());
      } else {
        new_storage.push_back(_storage[index[i] - 1]);
      }
    }
    return create_poly_vector(new_storage);
  }
  poly_vector_base_p copy() const {
    std::vector<T> new_storage;
    new_storage.reserve(size());
    new_storage.insert(new_storage.begin(), _storage.begin(), _storage.end());
    return create_poly_vector(new_storage);
  }
  poly_vector_base_p assign(cpp11::integers index, const poly_vector_base& value) const {
    if (index.size() != value.size()) {
      cpp11::stop("Incompatible vector sizes");
    }
    if (typeid(*this) != typeid(value)) {
      cpp11::stop("Incompatible assignment value type");
    }

    auto value_vec = get_vector_of_poly<T>(value);

    std::vector<T> new_storage(_storage);
    int max_size = *std::max_element(index.begin(), index.end());
    if (max_size > new_storage.size()) {
      new_storage.reserve(max_size);
      for (int j = new_storage.size(); j < max_size; ++j) {
        new_storage.push_back(T::NA_value());
      }
    }
    for (R_xlen_t i = 0; i < index.size(); ++i) {
      new_storage[index[i] - 1] = value_vec[i];
    }
    return create_poly_vector(new_storage);
  }
  poly_vector_base_p combine(cpp11::list_of< poly_vector_base_p > extra) const {
    std::vector<T> new_storage(_storage);

    for (R_xlen_t i = 0; i < extra.size(); ++i) {
      poly_vector_base* candidate = extra[i].get();
      if (typeid(*this) != typeid(*candidate)) {
        cpp11::stop("Incompatible vector types");
      }
      auto candidate_vec = get_vector_of_poly<T>(*candidate);
      for (size_t j = 0; j < candidate_vec.size(); ++j) {
        new_storage.push_back(candidate_vec[j]);
      }
    }

    return create_poly_vector(new_storage);
  }

  // Self-similarity
  poly_vector_base_p unique() const {
    std::vector<T> new_storage;
    bool NA_seen = false;
    for (auto iter = _storage.begin(); iter != _storage.end(); ++iter) {
      if (iter->is_na()) {
        if (!NA_seen) {
          new_storage.push_back(T::NA_value());
          NA_seen = true;
        }
        continue;
      }
      if (std::find(new_storage.begin(), new_storage.end(), *iter) == new_storage.end()) {
        new_storage.push_back(*iter);
      }
    }

    return create_poly_vector(new_storage);
  };
  cpp11::writable::logicals duplicated() const {
    std::vector<T> uniques;
    cpp11::writable::logicals dupes;
    dupes.reserve(size());
    bool NA_seen = false;
    for (auto iter = _storage.begin(); iter != _storage.end(); ++iter) {
      if (iter->is_na()) {
        if (!NA_seen) {
          dupes.push_back(TRUE);
          NA_seen = true;
        }
        continue;
      }
      if (std::find(uniques.begin(), uniques.end(), *iter) == uniques.end()) {
        uniques.push_back(*iter);
        dupes.push_back(FALSE);
      } else {
        dupes.push_back(TRUE);
      }
    }

    return dupes;
  }
  int any_duplicated() const {
    int anyone = -1;
    bool NA_seen = false;;
    int i = 0;
    for (auto iter = _storage.begin(); iter != _storage.end(); ++iter) {
      if (_storage[i].is_na()) {
        if (NA_seen) {
          anyone = i;
          break;
        }
        NA_seen = true;
      } else if (std::find(iter + 1, _storage.end(), *iter) != _storage.end()) {
        anyone = true;
        break;
      }
      ++i;
    }

    return anyone;
  }
  //cpp11::writable::integers match(const poly_vector_base& table) const {
  //  if (typeid(*this) != typeid(table)) {
  //    cpp11::writable::integers results;
  //    results.reserve(size());
  //    for (size_t i = 0; i < size(); ++i) {
  //      results.push_back(R_NaInt);
  //    }
  //    return results;
  //  }
//
  //  auto table_vec = get_vector_of_poly<T>(table);
//
  //  return match_impl(_storage, table_vec);
  //}

  // Missing values
  cpp11::writable::logicals is_na() const {
    cpp11::writable::logicals result;
    result.reserve(size());

    for (auto iter = _storage.begin(); iter != _storage.end(); ++iter) {
      result.push_back((Rboolean) !(*iter));
    }

    return result;
  }
  bool any_na() const {
    for (auto iter = _storage.begin(); iter != _storage.end(); ++iter) {
      if (!(*iter)) {
        return true;
      }
    }
    return false;
  }

  // Predicates
  cpp11::writable::logicals is_degenerate() const {
    cpp11::writable::logicals result;
    result.reserve(_storage.size());
    for (size_t i = 0; i < _storage.size(); ++i) {
      if (_storage[i].is_na()) {
        result.push_back(NA_LOGICAL);
        continue;
      }
      result.push_back((Rboolean) _storage[i].is_na());
    }
    return result;
  }
  cpp11::writable::logicals has_inside(const SEXP points) const {
    std::vector<Point_2> points_vec = euclid::get_point_2_vec(points);
    if (size() == 0 || points_vec.size() == 0) {
      return {};
    }
    size_t output_length = std::max(size(), points_vec.size());
    cpp11::writable::logicals result;
    result.reserve(output_length);
    for (size_t i = 0; i < output_length; ++i) {
      if (_storage[i % size()].is_na() || points_vec[i % points_vec.size()].is_na()) {
        result.push_back(NA_LOGICAL);
        continue;
      }
      result.push_back((Rboolean) has_inside_impl(_storage[i % size()], points_vec[i % points_vec.size()]));
    }
    return result;
  }
  cpp11::writable::logicals has_on(const SEXP points) {
    std::vector<Point_2> points_vec = euclid::get_point_2_vec(points);
    if (size() == 0 || points_vec.size() == 0) {
      return {};
    }
    size_t output_length = std::max(size(), points_vec.size());
    cpp11::writable::logicals result;
    result.reserve(output_length);
    for (size_t i = 0; i < output_length; ++i) {
      if (_storage[i % size()].is_na() || points_vec[i % points_vec.size()].is_na()) {
        result.push_back(NA_LOGICAL);
        continue;
      }
      result.push_back((Rboolean) has_on_impl(_storage[i % size()], points_vec[i % points_vec.size()]));
    }
    return result;
  }
  cpp11::writable::logicals has_outside(const SEXP points) const {
    std::vector<Point_2> points_vec = euclid::get_point_2_vec(points);
    if (size() == 0 || points_vec.size() == 0) {
      return {};
    }
    size_t output_length = std::max(size(), points_vec.size());
    cpp11::writable::logicals result;
    result.reserve(output_length);
    for (size_t i = 0; i < output_length; ++i) {
      if (_storage[i % size()].is_na() || points_vec[i % points_vec.size()].is_na()) {
        result.push_back(NA_LOGICAL);
        continue;
      }
      result.push_back((Rboolean) has_outside_impl(_storage[i % size()], points_vec[i % points_vec.size()]));
    }
    return result;
  }
  cpp11::writable::logicals has_on_positive(const SEXP points) {
    std::vector<Point_2> points_vec = euclid::get_point_2_vec(points);
    if (size() == 0 || points_vec.size() == 0) {
      return {};
    }
    size_t output_length = std::max(size(), points_vec.size());
    cpp11::writable::logicals result;
    result.reserve(output_length);
    for (size_t i = 0; i < output_length; ++i) {
      if (_storage[i % size()].is_na() || points_vec[i % points_vec.size()].is_na()) {
        result.push_back(NA_LOGICAL);
        continue;
      }
      result.push_back((Rboolean) has_on_positive_impl<T>(_storage[i % size()], points_vec[i % points_vec.size()]));
    }
    return result;
  }
  cpp11::writable::logicals has_on_negative(const SEXP points) {
    std::vector<Point_2> points_vec = euclid::get_point_2_vec(points);
    if (size() == 0 || points_vec.size() == 0) {
      return {};
    }
    size_t output_length = std::max(size(), points_vec.size());
    cpp11::writable::logicals result;
    result.reserve(output_length);
    for (size_t i = 0; i < output_length; ++i) {
      if (_storage[i % size()].is_na() || points_vec[i % points_vec.size()].is_na()) {
        result.push_back(NA_LOGICAL);
        continue;
      }
      result.push_back((Rboolean) has_on_negative_impl<T>(_storage[i % size()], points_vec[i % points_vec.size()]));
    }
    return result;
  }

  // Measures
  cpp11::writable::doubles length() const {
    cpp11::writable::doubles result;
    result.reserve(size());

    for (size_t i = 0; i < size(); ++i) {
      if (_storage[i].is_na()) {
        result.push_back(R_NaReal);
        continue;
      }
      result.push_back(length_impl(_storage[i]));
    }

    return result;
  }
  cpp11::writable::doubles area() const {
    cpp11::writable::doubles result;
    result.reserve(size());

    for (size_t i = 0; i < size(); ++i) {
      if (_storage[i].is_na()) {
        result.push_back(R_NaReal);
        continue;
      }
      result.push_back(area_impl(_storage[i]));
    }

    return result;
  }

  // Common
  poly_vector_base_p transform(const std::vector<Aff_transformation_2> affine) const {
    std::vector<T> result;

    if (size() == 0 || affine.size() == 0) {
      return create_poly_vector(result);
    }

    size_t output_length = std::max(size(), affine.size());

    result.reserve(output_length);

    for (size_t i = 0; i < output_length; ++i) {
      if (_storage[i % size()].is_na() || affine[i % affine.size()].is_na()) {
        result.push_back(T::NA_value());
        continue;
      }
      result.push_back(transform_impl(_storage[i % size()], affine[i % affine.size()]));
    }

    return create_poly_vector(result);
  }

  SEXP bbox() const {
    std::vector<Bbox_2> result;
    result.reserve(size());

    for (size_t i = 0; i < size(); ++i) {
      if (_storage[i].is_na()) {
        result.push_back(Bbox_2::NA_value());
        continue;
      }
      result.push_back(_storage[i].bbox());
    }

    return euclid::create_bbox_2_vec(result);
  }

  poly_vector_base_p reverse_orientation() const {
    std::vector<T> res;
    res.reserve(size());
    for (size_t i = 0; i < size(); ++i) {
      if (_storage[i].is_na()) {
        res.push_back(T::NA_value());
      } else {
        res.push_back(poly_reverse_orientation_impl(_storage[i]));
      }
    }
    return create_poly_vector(res);
  }
};
