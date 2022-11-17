#include "poly_vector.h"

#include <cpp11/doubles.hpp>
#include <cpp11/strings.hpp>
#include <cpp11/matrix.hpp>
#include <cpp11/logicals.hpp>
#include <cpp11/integers.hpp>
#include <cpp11/list_of.hpp>
#include <cpp11/external_pointer.hpp>

[[cpp11::register]]
cpp11::writable::strings poly_primitive_type(poly_vector_base_p geometries) {
  if (geometries.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  switch (geometries->poly_type()) {
  case POLYLINE: return {"polyline"};
  case POLYGON: return {"polygon"};
  case VIRTUAL: return {"virtual"};
  }
  return {"unknown"};
}

[[cpp11::register]]
int poly_length(poly_vector_base_p geometries) {
  if (geometries.get() == nullptr) {
    return 0;
  }
  return geometries->size();
}

[[cpp11::register]]
cpp11::writable::strings poly_definition_names(poly_vector_base_p geometries) {
  if (geometries.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  return geometries->def_names();
}

[[cpp11::register]]
SEXP poly_definition(poly_vector_base_p geometries, int which, cpp11::integers element) {
  if (geometries.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  std::vector<Exact_number> res = geometries->definition(which, element, element.size() == 0);
  return euclid::create_exact_numeric_vec(res);
}

[[cpp11::register]]
poly_vector_base_p poly_set_definition(poly_vector_base_p geometries, int which, cpp11::integers element, SEXP value) {
  std::vector<Exact_number> val = euclid::get_exact_numeric_vec(value);
  if (geometries.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  return geometries->set_definition(which, element, element.size() == 0, val);
}

[[cpp11::register]]
SEXP poly_vertex(poly_vector_base_p geometries, cpp11::integers which) {
  if (geometries.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  return geometries->vertex(which, which.size() == 0);
}

[[cpp11::register]]
poly_vector_base_p poly_set_vertex(poly_vector_base_p geometries, cpp11::integers which, SEXP value) {
  std::vector<Point_2> val = euclid::get_point_2_vec(value);
  if (geometries.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  return geometries->set_vertex(which, which.size() == 0, val);
}

[[cpp11::register]]
SEXP poly_edges(poly_vector_base_p geometries, cpp11::integers which) {
  if (geometries.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  return geometries->edge(which, which.size() == 0);
}

[[cpp11::register]]
cpp11::writable::integers poly_cardinality(poly_vector_base_p geometries) {
  if (geometries.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  cpp11::writable::integers result;
  result.reserve(geometries->size());
  for (size_t i = 0; i < geometries->size(); ++i) {
    result.push_back(geometries->cardinality(i));
  }
  return result;
}

[[cpp11::register]]
cpp11::writable::integers poly_n_edges(poly_vector_base_p geometries) {
  if (geometries.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  cpp11::writable::integers result;
  result.reserve(geometries->size());
  for (size_t i = 0; i < geometries->size(); ++i) {
    result.push_back(geometries->n_edges(i));
  }
  return result;
}

[[cpp11::register]]
poly_vector_base_p poly_subset(poly_vector_base_p geometries, cpp11::integers index) {
  if (geometries.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  return geometries->subset(index);
}

[[cpp11::register]]
poly_vector_base_p poly_copy(poly_vector_base_p geometries) {
  if (geometries.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  return geometries->copy();
}

[[cpp11::register]]
poly_vector_base_p poly_assign(poly_vector_base_p geometries, cpp11::integers index, poly_vector_base_p value) {
  if (geometries.get() == nullptr || value.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  return geometries->assign(index, *value);
}

[[cpp11::register]]
poly_vector_base_p poly_combine(poly_vector_base_p geometries, cpp11::list_of< poly_vector_base_p > extra) {
  if (geometries.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  return geometries->combine(extra);
}

[[cpp11::register]]
poly_vector_base_p poly_unique(poly_vector_base_p geometries) {
  if (geometries.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  return geometries->unique();
}

[[cpp11::register]]
cpp11::writable::logicals poly_duplicated(poly_vector_base_p geometries) {
  if (geometries.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  return geometries->duplicated();
}

[[cpp11::register]]
cpp11::writable::integers poly_any_duplicated(poly_vector_base_p geometries) {
  if (geometries.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  return {geometries->any_duplicated() + 1};
}

[[cpp11::register]]
cpp11::writable::logicals poly_is_na(poly_vector_base_p geometries) {
  if (geometries.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  return geometries->is_na();
}

[[cpp11::register]]
bool poly_any_na(poly_vector_base_p geometries) {
  if (geometries.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  return geometries->any_na();
}

[[cpp11::register]]
cpp11::writable::doubles_matrix<> poly_to_matrix(poly_vector_base_p geometries) {
  if (geometries.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  return geometries->as_numeric();
}

[[cpp11::register]]
cpp11::writable::strings poly_format(poly_vector_base_p geometries) {
  if (geometries.get() == nullptr) {
    return {">>>Data structure pointer cleared from memory<<<"};
  }
  return geometries->format();
}

[[cpp11::register]]
cpp11::writable::logicals poly_is_equal(poly_vector_base_p geometries1,
                                        poly_vector_base_p geometries2) {
  if (geometries1.get() == nullptr || geometries2.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  return (*geometries1) == (*geometries2);
}

[[cpp11::register]]
poly_vector_base_p poly_transform(poly_vector_base_p geometries, SEXP affine) {
  std::vector<Aff_transformation_2> affine_vec = euclid::get_transform_2_vec(affine);
  if (geometries.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  return geometries->transform(affine_vec);
}

[[cpp11::register]]
SEXP poly_bbox(poly_vector_base_p geometries) {
  if (geometries.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  return geometries->bbox();
}
