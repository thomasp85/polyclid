#include "poly_vector.h"
#include "polygon.h"
#include "polyline.h"

template<>
poly_vector_base_p create_poly_vector(std::vector<Polyline>& input) {
  polyline* vec = new polyline(input);
  return {vec};
}
template<>
poly_vector_base_p create_poly_vector(std::vector<Polygon>& input) {
  polygon* vec = new polygon(input);
  return {vec};
}

template<>
const std::vector<Polyline> get_vector_of_poly(const poly_vector_base& geometries) {
  if (geometries.poly_type() != POLYLINE) {
    cpp11::stop("Geometry must contain polylines");
  }
  auto recast = dynamic_cast< const poly_vector<Polyline>* >(&geometries);
  return recast->get_storage();
}
template<>
const std::vector<Polygon> get_vector_of_poly(const poly_vector_base& geometries) {
  if (geometries.poly_type() != POLYGON) {
    cpp11::stop("Geometry must contain polygons");
  }
  auto recast = dynamic_cast< const poly_vector<Polygon>* >(&geometries);
  return recast->get_storage();
}
