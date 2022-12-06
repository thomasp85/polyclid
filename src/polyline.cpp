#include <cpp11/list.hpp>
#include "polyline.h"

[[cpp11::register]]
polyline_p create_polyline_empty() {
  std::vector<Polyline> vec;
  polyline *result(new polyline(vec));
  return {result};
}

[[cpp11::register]]
polyline_p create_polyline_single(SEXP p) {
  std::vector<Polyline> vec;
  auto p_vec = euclid::get_point_2_vec(p);
  Polyline boundary(p_vec.begin(), p_vec.end());
  vec.emplace_back(boundary);
  polyline *result(new polyline(vec));
  return {result};
}

[[cpp11::register]]
polyline_p create_polyline_list(cpp11::list ps) {
  std::vector<Polyline> vec;
  for (int i = 0; i < ps.size(); ++i) {
    SEXP p = ps[i];
    if (Rf_isNull(p)) {
      vec.push_back(with_NA<Polyline>::NA_value());
      continue;
    }
    auto p_vec = euclid::get_point_2_vec(p);
    Polyline boundary(p_vec.begin(), p_vec.end());
    vec.emplace_back(boundary);
  }
  polyline *result(new polyline(vec));
  return {result};
}

[[cpp11::register]]
polyline_p create_polyline_segment(SEXP segments) {
  std::vector<Polyline> vec;
  auto seg = euclid::get_segment_2_vec(segments);
  std::vector<Point_2> vertices;
  for (int i = 0; i < seg.size(); ++i) {
    if (!seg[i]) {
      vec.push_back(with_NA<Polyline>::NA_value());
      continue;
    }
    vertices = {seg[i][0], seg[i][1]};
    Polyline boundary(vertices.begin(), vertices.end());
    vec.emplace_back(boundary);
  }
  polyline *result(new polyline(vec));
  return {result};
}

[[cpp11::register]]
polyline_p polyline_glue(cpp11::list_of<polyline_p> polylines, bool na_rm) {
  std::vector<Polyline> vec;
  vec.reserve(polylines.size());
  for (R_xlen_t i = 0; i < polylines.size(); ++i) {
    vec.push_back(polylines[i]->glue(na_rm));
  }
  polyline *result(new polyline(vec));
  return {result};
}
