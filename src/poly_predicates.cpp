#include <cpp11/logicals.hpp>
#include "poly_vector.h"
#include "polygon.h"
#include "polyline.h"

// POINT PREDICATES

[[cpp11::register]]
cpp11::writable::logicals poly_is_degenerate(poly_vector_base_p geometries) {
  return geometries->is_degenerate();
}

[[cpp11::register]]
cpp11::writable::logicals poly_has_point_inside(poly_vector_base_p geometries, SEXP points) {
  return geometries->has_inside(points);
}

[[cpp11::register]]
cpp11::writable::logicals poly_has_point_on(poly_vector_base_p geometries, SEXP points) {
  return geometries->has_on(points);
}

[[cpp11::register]]
cpp11::writable::logicals poly_has_point_outside(poly_vector_base_p geometries, SEXP points) {
  return geometries->has_outside(points);
}

[[cpp11::register]]
cpp11::writable::logicals poly_has_point_on_positive(poly_vector_base_p geometries, SEXP points) {
  return geometries->has_on_positive(points);
}

[[cpp11::register]]
cpp11::writable::logicals poly_has_point_on_negative(poly_vector_base_p geometries, SEXP points) {
  return geometries->has_on_negative(points);
}

// POLYGONS

[[cpp11::register]]
cpp11::writable::logicals polygon_is_valid(polygon_p poly) {
  return poly->is_valid();
}

[[cpp11::register]]
cpp11::writable::logicals polygon_is_clockwise(polygon_p poly) {
  return poly->is_clockwise();
}

[[cpp11::register]]
cpp11::writable::logicals polygon_is_counterclockwise(polygon_p poly) {
  return poly->is_counterclockwise();
}

[[cpp11::register]]
cpp11::writable::logicals polygon_is_convex(polygon_p poly) {
  return poly->is_convex();
}

[[cpp11::register]]
cpp11::writable::logicals polygon_is_simple(polygon_p poly) {
  return poly->is_simple();
}

[[cpp11::register]]
cpp11::writable::logicals polygon_is_relatively_simple(polygon_p poly) {
  return poly->is_relatively_simple();
}

[[cpp11::register]]
cpp11::writable::logicals polygon_is_collinear(polygon_p poly) {
  return poly->is_collinear();
}

// POLYLINES

[[cpp11::register]]
cpp11::writable::logicals polyline_is_x_monotone(polyline_p poly) {
  return poly->is_x_monotone();
}

[[cpp11::register]]
cpp11::writable::logicals polyline_is_y_monotone(polyline_p poly) {
  return poly->is_y_monotone();
}

[[cpp11::register]]
cpp11::writable::logicals polyline_is_x_weakly_monotone(polyline_p poly) {
  return poly->is_x_weakly_monotone();
}

[[cpp11::register]]
cpp11::writable::logicals polyline_is_y_weakly_monotone(polyline_p poly) {
  return poly->is_y_weakly_monotone();
}

[[cpp11::register]]
cpp11::writable::logicals polyline_is_selfintersecting(polyline_p poly) {
  return poly->is_selfintersecting();
}
