#include <vector>
#include <cpp11/list.hpp>
#include <R_ext/Rdynload.h>

#include "cgal_types.h"
#include "poly_vector.h"

cpp11::external_pointer<poly_vector_base> get_ext_pointer(SEXP val) {
  if (TYPEOF(val) != VECSXP) {
    cpp11::stop("Provided object not an polyclid class");
  }
  SEXP pointer = VECTOR_ELT(val, 0);
  if (TYPEOF(pointer) != EXTPTRSXP) {
    cpp11::stop("Provided object not an polyclid class");
  }
  cpp11::external_pointer<poly_vector_base> ex_p(pointer);
  if (ex_p.get() == nullptr) {
    cpp11::stop("Provided object is not of the correct class or has been cleared from memory");
  }
  return ex_p;
}

const std::vector<Polygon> get_polygon_vec(SEXP val) {
  poly_vector_base_p pointer = get_ext_pointer(val);
  return get_vector_of_poly<Polygon>(*pointer);
}
const std::vector<Polygon_set> get_polygon_set_vec(SEXP val) {
  poly_vector_base_p pointer = get_ext_pointer(val);
  return get_vector_of_poly<Polygon_set>(*pointer);
}
const std::vector<Polyline> get_polyline_vec(SEXP val) {
  poly_vector_base_p pointer = get_ext_pointer(val);
  return get_vector_of_poly<Polyline>(*pointer);
}
const std::vector<Polyline_set> get_polyline_set_vec(SEXP val) {
  poly_vector_base_p pointer = get_ext_pointer(val);
  return get_vector_of_poly<Polyline_set>(*pointer);
}

SEXP create_polygon_vec(std::vector<Polygon>& val) {
  cpp11::writable::list result;
  result.push_back(create_poly_vector(val));
  result.attr("class") = {"polyclid_polygon", "polyclid_geometry"};
  return result;
}
SEXP create_polygon_set_vec(std::vector<Polygon_set>& val) {
  cpp11::writable::list result;
  result.push_back(create_poly_vector(val));
  result.attr("class") = {"polyclid_polygon_set", "polyclid_geometry"};
  return result;
}
SEXP create_polyline_vec(std::vector<Polyline>& val) {
  cpp11::writable::list result;
  result.push_back(create_poly_vector(val));
  result.attr("class") = {"polyclid_polyline", "polyclid_geometry"};
  return result;
}
SEXP create_polyline_set_vec(std::vector<Polyline_set>& val) {
  cpp11::writable::list result;
  result.push_back(create_poly_vector(val));
  result.attr("class") = {"polyclid_polyline_set", "polyclid_geometry"};
  return result;
}

Primitive get_geometry_type(SEXP val) {
  poly_vector_base_p pointer = get_ext_pointer(val);
  return pointer->poly_type();
}
size_t get_geometry_dimensions(SEXP val) {
  return 2;
}

[[cpp11::init]]
void export_euclid_api(DllInfo* dll) {
  R_RegisterCCallable("polyclid", "get_polygon_vec", (DL_FUNC)get_polygon_vec);
  R_RegisterCCallable("polyclid", "get_polygon_set_vec", (DL_FUNC)get_polygon_set_vec);
  R_RegisterCCallable("polyclid", "get_polyline_vec", (DL_FUNC)get_polyline_vec);
  R_RegisterCCallable("polyclid", "get_polyline_set_vec", (DL_FUNC)get_polyline_set_vec);

  R_RegisterCCallable("polyclid", "create_polygon_vec", (DL_FUNC)create_polygon_vec);
  R_RegisterCCallable("polyclid", "create_polygon_set_vec", (DL_FUNC)create_polygon_set_vec);
  R_RegisterCCallable("polyclid", "create_polyline_vec", (DL_FUNC)create_polyline_vec);
  R_RegisterCCallable("polyclid", "create_polyline_set_vec", (DL_FUNC)create_polyline_set_vec);

  R_RegisterCCallable("polyclid", "get_geometry_type", (DL_FUNC)get_geometry_type);
  R_RegisterCCallable("polyclid", "get_geometry_dimensions", (DL_FUNC)get_geometry_dimensions);
}
