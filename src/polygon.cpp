#include <cpp11/list.hpp>
#include "polygon.h"

[[cpp11::register]]
polygon_p create_polygon_empty() {
  std::vector<Polygon> vec;
  polygon *result(new polygon(vec));
  return {result};
}

[[cpp11::register]]
polygon_p create_polygon_single(SEXP p) {
  std::vector<Polygon> vec;
  auto p_vec = euclid::get_point_2_vec(p);
  Polyline boundary(p_vec.begin(), p_vec.end());
  vec.emplace_back(boundary);
  polygon *result(new polygon(vec));
  return {result};
}

[[cpp11::register]]
polygon_p create_polygon_list(cpp11::list ps) {
  std::vector<Polygon> vec;
  for (int i = 0; i < ps.size(); ++i) {
    SEXP p = ps[i];
    if (Rf_isNull(p)) {
      vec.push_back(with_NA<Polygon>::NA_value());
      continue;
    }
    auto p_vec = euclid::get_point_2_vec(p);
    Polyline boundary(p_vec.begin(), p_vec.end());
    vec.emplace_back(boundary);
  }
  polygon *result(new polygon(vec));
  return {result};
}

[[cpp11::register]]
polygon_p create_polygon_list_list(cpp11::list pss) {
  std::vector<Polygon> vec;
  for (int i = 0; i < pss.size(); ++i) {
    SEXP ps = pss[i];
    if (Rf_isNull(ps)) {
      vec.push_back(with_NA<Polygon>::NA_value());
      continue;
    }
    SEXP p = VECTOR_ELT(ps, 0);
    if (Rf_isNull(p)) {
      vec.push_back(with_NA<Polygon>::NA_value());
      continue;
    }
    auto p_vec = euclid::get_point_2_vec(p);
    Polyline boundary(p_vec.begin(), p_vec.end());
    vec.emplace_back(boundary);
    for (int j = 1; j < Rf_length(ps); j++) {
      SEXP p = VECTOR_ELT(ps, j);
      if (Rf_isNull(p)) {
        vec.pop_back();
        vec.push_back(with_NA<Polygon>::NA_value());
        continue;
      }
      auto p_vec = euclid::get_point_2_vec(p);
      Polyline hole(p_vec.begin(), p_vec.end());
      vec.back().add_hole(hole);
    }
  }
  polygon *result(new polygon(vec));
  return {result};
}

[[cpp11::register]]
polygon_p create_polygon_triangle(SEXP triangles) {
  std::vector<Polygon> vec;
  auto tri = euclid::get_triangle_2_vec(triangles);
  std::vector<Point_2> vertices;
  for (int i = 0; i < tri.size(); ++i) {
    if (!tri[i]) {
      vec.push_back(with_NA<Polygon>::NA_value());
      continue;
    }
    vertices = {tri[i][0], tri[i][1], tri[i][2]};
    Polyline boundary(vertices.begin(), vertices.end());
    vec.emplace_back(boundary);
  }
  polygon *result(new polygon(vec));
  return {result};
}

[[cpp11::register]]
cpp11::writable::integers polygon_sub_cardinality(polygon_p geometries) {
  if (geometries.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  return geometries->sub_cardinality();
}

[[cpp11::register]]
cpp11::writable::integers polygon_n_holes(polygon_p geometries) {
  if (geometries.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  return geometries->n_holes();
}

[[cpp11::register]]
cpp11::writable::logicals polygon_is_unbounded(polygon_p geometries) {
  if (geometries.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  return geometries->is_unbounded();
}

[[cpp11::register]]
polygon_p polygon_get_boundary(polygon_p geometries) {
  if (geometries.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  std::vector<Polygon> boundary = geometries->get_boundary();
  polygon *result(new polygon(boundary));
  return {result};
}

[[cpp11::register]]
polygon_p polygon_set_boundary(polygon_p geometries, polygon_p other) {
  if (geometries.get() == nullptr || other.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  std::vector<Polygon> poly = geometries->set_boundary(other->get_storage());
  polygon *result(new polygon(poly));
  return {result};
}

[[cpp11::register]]
polygon_p polygon_get_hole(polygon_p geometries, cpp11::integers which) {
  if (geometries.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  std::vector<Polygon> poly = geometries->get_hole(which);
  polygon *result(new polygon(poly));
  return {result};
}

[[cpp11::register]]
polygon_p polygon_remove_hole(polygon_p geometries, cpp11::integers which) {
  if (geometries.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  std::vector<Polygon> poly = geometries->remove_hole(which);
  polygon *result(new polygon(poly));
  return {result};
}

[[cpp11::register]]
polygon_p polygon_add_hole(polygon_p geometries, polygon_p other) {
  if (geometries.get() == nullptr || other.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  std::vector<Polygon> poly = geometries->add_hole(other->get_storage());
  polygon *result(new polygon(poly));
  return {result};
}

[[cpp11::register]]
polygon_p polygon_set_hole(polygon_p geometries, cpp11::integers which, polygon_p other) {
  if (geometries.get() == nullptr || other.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  std::vector<Polygon> poly = geometries->set_hole(which, other->get_storage());
  polygon *result(new polygon(poly));
  return {result};
}

[[cpp11::register]]
polygon_p polygon_get_rings(polygon_p geometries) {
  if (geometries.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  std::vector<Polygon> poly = geometries->get_rings();
  polygon *result(new polygon(poly));
  return {result};
}

[[cpp11::register]]
poly_vector_base_p polygon_partition_convex(polygon_p geometries, bool optimal, bool triangulate) {
  if (geometries.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  return geometries->partition_convex(optimal, triangulate);
}

[[cpp11::register]]
poly_vector_base_p polygon_partition_monotone(polygon_p geometries) {
  if (geometries.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  return geometries->partition_monotone();
}
