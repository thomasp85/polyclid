#include "poly_vector.h"
#include "polygon.h"

[[cpp11::register]]
poly_vector_base_p poly_reverse_orientation(poly_vector_base_p poly) {
  if (poly.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  return poly->reverse_orientation();
}

[[cpp11::register]]
poly_vector_base_p polygon_make_valid(polygon_p poly) {
  if (poly.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  return poly->make_valid();
}

[[cpp11::register]]
poly_vector_base_p polygon_connect_holes(polygon_p poly) {
  if (poly.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  return poly->connect_holes();
}
