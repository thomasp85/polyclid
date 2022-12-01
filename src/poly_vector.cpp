#include "poly_vector.h"
#include "polygon.h"
#include "polyline.h"
#include "polygon_set.h"
#include "polyline_set.h"

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
poly_vector_base_p create_poly_vector(std::vector<Polygon_set>& input) {
  polygon_set* vec = new polygon_set(input);
  return {vec};
}
template<>
poly_vector_base_p create_poly_vector(std::vector<Polyline_set>& input) {
  polyline_set* vec = new polyline_set(input);
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
template<>
const std::vector<Polygon_set> get_vector_of_poly(const poly_vector_base& geometries) {
  if (geometries.poly_type() != POLYGON_SET) {
    cpp11::stop("Geometry must contain polygon sets");
  }
  auto recast = dynamic_cast< const poly_vector<Polygon_set>* >(&geometries);
  return recast->get_storage();
}
template<>
const std::vector<Polyline_set> get_vector_of_poly(const poly_vector_base& geometries) {
  if (geometries.poly_type() != POLYLINE_SET) {
    cpp11::stop("Geometry must contain polyline sets");
  }
  auto recast = dynamic_cast< const poly_vector<Polyline_set>* >(&geometries);
  return recast->get_storage();
}
