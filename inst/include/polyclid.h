#pragma once

#include "internal/cgal_types.h"
#include <Rinternals.h>
#include <R_ext/Rdynload.h>
#include <vector>

namespace polyclid {

enum Primitive {
  VIRTUAL,
  POLYGON,
  POLYLINE,
  POLYGON_SET,
  POLYLINE_SET
};

inline std::vector<Polygon> get_polygon_vec(SEXP val) {
  typedef std::vector<Polygon> fn_ptr(SEXP);
  static fn_ptr *fn = (fn_ptr*) R_GetCCallable("polyclid", "get_polygon_vec");
  return fn(val);
}
inline std::vector<Polygon_set> get_polygon_set_vec(SEXP val) {
  typedef std::vector<Polygon_set> fn_ptr(SEXP);
  static fn_ptr *fn = (fn_ptr*) R_GetCCallable("polyclid", "get_polygon_set_vec");
  return fn(val);
}
inline std::vector<Polyline> get_polyline_vec(SEXP val) {
  typedef std::vector<Polyline> fn_ptr(SEXP);
  static fn_ptr *fn = (fn_ptr*) R_GetCCallable("polyclid", "get_polyline_vec");
  return fn(val);
}
inline std::vector<Polyline_set> get_polyline_set_vec(SEXP val) {
  typedef std::vector<Polyline_set> fn_ptr(SEXP);
  static fn_ptr *fn = (fn_ptr*) R_GetCCallable("polyclid", "get_polyline_set_vec");
  return fn(val);
}

inline SEXP create_polygon_vec(std::vector<Polygon>& val) {
  typedef SEXP fn_ptr(std::vector<Polygon>&);
  static fn_ptr *fn = (fn_ptr*) R_GetCCallable("polyclid", "create_polygon_vec");
  return fn(val);
}
inline SEXP create_polygon_set_vec(std::vector<Polygon_set>& val) {
  typedef SEXP fn_ptr(std::vector<Polygon_set>&);
  static fn_ptr *fn = (fn_ptr*) R_GetCCallable("polyclid", "create_polygon_set_vec");
  return fn(val);
}
inline SEXP create_polyline_vec(std::vector<Polyline>& val) {
  typedef SEXP fn_ptr(std::vector<Polyline>&);
  static fn_ptr *fn = (fn_ptr*) R_GetCCallable("polyclid", "create_polyline_vec");
  return fn(val);
}
inline SEXP create_polyline_set_vec(std::vector<Polyline_set>& val) {
  typedef SEXP fn_ptr(std::vector<Polyline_set>&);
  static fn_ptr *fn = (fn_ptr*) R_GetCCallable("polyclid", "create_polyline_set_vec");
  return fn(val);
}

inline Primitive get_geometry_type(SEXP val) {
  typedef Primitive fn_ptr(SEXP);
  static fn_ptr *fn = (fn_ptr*) R_GetCCallable("polyclid", "get_geometry_type");
  return fn(val);
}
inline size_t get_geometry_dimensions(SEXP val) {
  typedef size_t fn_ptr(SEXP);
  static fn_ptr *fn = (fn_ptr*) R_GetCCallable("polyclid", "get_geometry_dimensions");
  return fn(val);
}
};
