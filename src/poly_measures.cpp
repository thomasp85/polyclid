#include <cpp11/doubles.hpp>
#include "poly_vector.h"

[[cpp11::register]]
cpp11::writable::doubles poly_approx_length(poly_vector_base_p geometries) {
  return geometries->length();
}
[[cpp11::register]]
cpp11::writable::doubles poly_approx_area(poly_vector_base_p geometries) {
  return geometries->area();
}
