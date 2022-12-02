#pragma once

#include <cpp11/strings.hpp>
#include "poly_vector.h"
#include "polygon_predicates.h"
#include "polygon_modification.h"

#include <CGAL/Boolean_set_operations_2.h>
#include <CGAL/Arr_overlay_2.h>
#include <CGAL/Arr_default_overlay_traits.h>

// Overlay traits for marking features resulting from overlaying features from
// different groups with 2 (input groups should be market 0 and 1)
template <typename Arrangement> struct Overlay_traits {
  typedef typename Arrangement::Vertex_const_handle     V_const_handle;
  typedef typename Arrangement::Halfedge_const_handle   H_const_handle;
  typedef typename Arrangement::Face_const_handle       F_const_handle;
  typedef typename Arrangement::Vertex_handle           V_handle;
  typedef typename Arrangement::Halfedge_handle         H_handle;
  typedef typename Arrangement::Face_handle             F_handle;

  int combine(int a, int b) const { return a == b ? a : 2; }
  int combine_face(int a, int b) const { return a == -1 ? b : (b == -1 ? a : combine(a, b)); }

  void create_face(F_const_handle f1, F_const_handle f2, F_handle f) const {
    f->set_data(combine_face(f1->data(), f2->data()));
  }
  void create_vertex(H_const_handle h1, H_const_handle h2, V_handle v) const {
    v->set_data(combine(h1->data(), h2->data()));
  }
  void create_vertex(V_const_handle v1, V_const_handle v2, V_handle v) const {
    v->set_data(combine(v1->data(), v2->data()));
  }
  void create_vertex(V_const_handle v1, H_const_handle h2, V_handle v) const {
    v->set_data(combine(v1->data(), h2->data()));
  }
  void create_vertex(H_const_handle h1, V_const_handle v2, V_handle v) const {
    v->set_data(combine(h1->data(), v2->data()));
  }
  void create_vertex(F_const_handle f1, V_const_handle v2, V_handle v) const {
    v->set_data(combine_face(f1->data(), v2->data()));
  }
  void create_vertex(V_const_handle v1, F_const_handle f2, V_handle v) const {
    v->set_data(combine_face(v1->data(), f2->data()));
  }
  void create_edge(H_const_handle h1, H_const_handle h2, H_handle h) const {
    h->set_data(combine(h1->data(), h2->data()));
    h->twin()->set_data(combine(h1->data(), h2->data()));
  }
  void create_edge(H_const_handle h1, F_const_handle f2, H_handle h) const {
    h->set_data(h1->data());
    h->twin()->set_data(h1->data());
  }
  void create_edge(F_const_handle f1, H_const_handle h2, H_handle h) const {
    h->set_data(h2->data());
    h->twin()->set_data(h2->data());
  }
};


class polyline_set : public poly_vector<Polyline_set> {
public:
  using poly_vector::poly_vector;
  ~polyline_set() = default;

  Primitive poly_type() const { return POLYLINE_SET; }

  cpp11::writable::strings def_names() const {
    return {"p"};
  }

  std::string format_elem(int i) const {
    Polyline_set p = _storage[i];
    if (p.is_na()) {
      return "<NA>";
    }
    size_t p_size = p.number_of_edges();
    if (p.is_empty()) {
      return "[empty]";
    }
    std::ostringstream f;
    f << std::setprecision(3);
    f << "[# Polylines: " << p_size;
    Bbox_2 bb = p.bbox();
    if (bb.is_na()) {
      f << ", Range: <Undefined>]";
    } else {
      f << ", Range: <<" << bb.xmin() << ", " << bb.ymin() << ">, <" <<bb.xmax() << ", " << bb.ymax() << ">>, ";
    }
    f << "Isolated vertices: " << p.number_of_isolated_vertices() << "]";
    return f.str();
  }

  size_t cardinality(size_t i) const {
    return _storage[i].number_of_vertices();
  }

  size_t n_edges(size_t i) const {
    size_t n = 0;
    for (auto iter = _storage[i].edges_begin(); iter != _storage[i].edges_end(); iter++) {
      n += iter->curve().number_of_subcurves();
    }
    return n;
  }

  Point_2 get_elem(size_t i, size_t j) const {
    auto iter = _storage[i].vertices_begin();
    for (size_t k = 0; k < j; ++k) {
      iter++;
    }
    return iter->point();
  }

  Segment_2 get_edge(size_t i, size_t j) const {
    auto iter = _storage[i].edges_begin();
    size_t k = 0;
    while (k + iter->curve().number_of_subcurves() < j) {
      k += iter->curve().number_of_subcurves();
      iter++;
    }
    return {iter->curve()[j - k]};
  }

  cpp11::writable::integers n_polylines() const {
    cpp11::writable::integers res;
    res.reserve(size());
    for (size_t i = 0; i < size(); ++i) {
      if (_storage[i].is_na()) {
        res.push_back(0);
      } else {
        res.push_back(_storage[i].number_of_edges());
      }
    }
    return res;
  }

  poly_vector_base_p get_polyline(cpp11::integers i) const {
    std::vector<Polyline> res;
    res.reserve(size());

    for (size_t j = 0; j < size(); ++j) {
      Polyline_set p = _storage[j];
      if (p.is_na() || i[j % i.size()] >= p.number_of_edges()) {
        res.push_back(Polyline::NA_value());
        continue;
      }
      auto edge = p.edges_begin();
      for (int k = 0; k < i[j % i.size()]; ++k) {
        edge++;
      }
      res.emplace_back(edge->curve().points_begin(), edge->curve().points_end());
    }

    return create_poly_vector(res);
  }

  poly_vector_base_p get_all_polylines() const {
    std::vector<Polyline> res;
    res.reserve(size());

    for (size_t j = 0; j < size(); ++j) {
      Polyline_set p = _storage[j];
      if (p.is_na()) {
        continue;
      }
      for (auto iter = p.edges_begin(); iter != p.edges_end(); iter++) {
        res.emplace_back(iter->curve().points_begin(), iter->curve().points_end());
      }
    }

    return create_poly_vector(res);
  }

  cpp11::writable::integers n_polylines_simplified() const {
    cpp11::writable::integers res;
    res.reserve(size());
    for (size_t i = 0; i < size(); ++i) {
      Polyline_set p = _storage[i];
      if (p.is_na()) {
        res.push_back(0);
        continue;
      }
      for (auto iter = p.edges_begin(); iter != p.edges_end(); iter++) {
        iter->set_data(0);
      }
      int count = 0;
      for (auto iter = p.edges_begin(); iter != p.edges_end(); iter++) {
        if (iter->data() != 0) continue;
        auto pl = iter->twin()->twin();
        pl->set_data(1);
        while (pl->source()->degree() == 2) {
          if (pl->data() == 1) break;
          pl = pl->prev();
        }
        pl->set_data(2);
        while (true) {
          pl->set_data(1);
          if (pl->target()->degree() != 2) break;
          pl = pl->next();
          if (pl->data() == 2) break;
        }
        count++;
      }
      res.push_back(count);
    }
    return res;
  }

  poly_vector_base_p get_polylines_simplified(cpp11::integers i) {
    std::vector<Polyline> res;
    res.reserve(size());

    for (size_t j = 0; j < size(); ++j) {
      Polyline_set p = _storage[j];
      if (p.is_na()) {
        res.push_back(Polyline::NA_value());
        continue;
      }
      int index = i[j % i.size()];
      Polyline polyline = Polyline::NA_value();
      for (auto iter = p.edges_begin(); iter != p.edges_end(); iter++) {
        if (iter->data() != 0) continue;
        if (index != 0) {
          index--;
          continue;
        }
        std::vector<Point_2> points;
        auto pl = iter->twin()->twin();
        pl->set_data(1);
        while (pl->source()->degree() == 2) {
          if (pl->data() == 1) break;
          pl = pl->prev();
        }
        pl->set_data(2);
        while (true) {
          pl->set_data(1);
          points.insert(points.end(), pl->curve().points_begin(), pl->curve().points_end());
          if (pl->target()->degree() != 2) break;
          pl = pl->next();
          if (pl->data() == 2) break;
        }
        polyline = Polyline(points.begin(), points.end());
      }
      res.push_back(polyline);
    }

    return create_poly_vector(res);
  }

  poly_vector_base_p get_all_polylines_simplified() {
    std::vector<Polyline> res;
    res.reserve(size());

    for (size_t j = 0; j < size(); ++j) {
      Polyline_set p = _storage[j];
      if (p.is_na()) {
        continue;
      }
      for (auto iter = p.edges_begin(); iter != p.edges_end(); iter++) {
        iter->set_data(0);
      }
      for (auto iter = p.edges_begin(); iter != p.edges_end(); iter++) {
        if (iter->data() != 0) continue;
        std::vector<Point_2> points;
        auto pl = iter->twin()->twin();
        pl->set_data(1);
        while (pl->source()->degree() == 2) {
          if (pl->data() == 1) break;
          pl = pl->prev();
        }
        pl->set_data(2);
        while (true) {
          pl->set_data(1);
          points.insert(points.end(), pl->curve().points_begin(), pl->curve().points_end());
          if (pl->target()->degree() != 2) break;
          pl = pl->next();
          if (pl->data() == 2) break;
        }
        res.emplace_back(points.begin(), points.end());
      }
    }

    return create_poly_vector(res);
  }

  cpp11::writable::integers vert_degree() const {
    cpp11::writable::integers res;
    res.reserve(size());
    for (size_t i = 0; i < size(); ++i) {
      for (auto iter = _storage[i].vertices_begin(); iter != _storage[i].vertices_end(); iter++) {
        res.push_back(iter->degree());
      }
    }
    return res;
  }

  cpp11::writable::list vert_neighbors() {
    cpp11::writable::list res;
    res.reserve(size());
    for (size_t i = 0; i < size(); ++i) {
      unsigned int j = 1;
      for (auto iter = _storage[i].vertices_begin(); iter != _storage[i].vertices_end(); iter++) {
        iter->set_data(j++);
      }
      for (auto iter = _storage[i].vertices_begin(); iter != _storage[i].vertices_end(); iter++) {
        cpp11::writable::integers neighbors;
        auto incoming = iter->incident_halfedges();
        if (incoming != nullptr) {
          auto i_iter = incoming;
          do {
            neighbors.push_back(i_iter->source().current_iterator()->data());
          } while (++i_iter != incoming);
        }
        res.push_back(neighbors);
      }
    }
    return res;
  }

  cpp11::writable::list polyline_verts() {
    cpp11::writable::list res;
    res.reserve(size());
    for (size_t i = 0; i < size(); ++i) {
      unsigned int j = 1;
      for (auto iter = _storage[i].vertices_begin(); iter != _storage[i].vertices_end(); iter++) {
        iter->set_data(j++);
      }
      for (auto iter = _storage[i].edges_begin(); iter != _storage[i].edges_end(); iter++) {
        cpp11::writable::integers verts;
        verts.push_back(iter->source()->data());
        verts.push_back(iter->target()->data());
        res.push_back(verts);
      }
    }
    return res;
  }

  cpp11::writable::integers n_faces() const {
    cpp11::writable::integers res;
    res.reserve(size());
    for (size_t i = 0; i < size(); ++i) {
      int n = _storage[i].number_of_faces() - 1;
      res.push_back(n);
    }
    return res;
  }

  poly_vector_base_p get_faces() const {
    std::vector<Polygon> res;
    res.reserve(size());
    for (size_t i = 0; i < size(); ++i) {
      for (auto iter = _storage[i].faces_begin(); iter != _storage[i].faces_end(); iter++) {
        if (iter->is_unbounded()) continue;
        std::vector<Point_2> points;
        auto outer = iter->outer_ccb();
        if (outer != nullptr) {
          auto c_iter = outer;
          do {
            auto curve = c_iter->curve();
            auto end = curve.points_end();
            end--;
            for (auto p_iter = curve.points_begin(); p_iter != end; p_iter++) {
              points.push_back(*p_iter);
            }
          } while (++c_iter != outer);
        }
        Polyline boundary(points.begin(), points.end());
        res.emplace_back(boundary);
        if (iter->number_of_holes() != 0) {
          for (auto h_iter = iter->holes_begin(); h_iter != iter->holes_end(); h_iter++) {
            auto inner = *h_iter;
            if (inner != nullptr) {
              points.clear();
              auto i_iter = inner;
              do {
                auto curve = i_iter->curve();
                auto end = curve.points_end();
                end--;
                for (auto p_iter = curve.points_begin(); p_iter != end; p_iter++) {
                  points.push_back(*p_iter);
                }
              } while (++i_iter != inner);
              Polyline hole(points.begin(), points.end());
              res.back().add_hole(hole);
            }
          }
        }
      }
    }

    for (auto iter = res.begin(); iter != res.end(); iter++) {
      iter->set_flag(VALIDITY_CHECKED, true);
      iter->set_flag(IS_VALID, true);
    }

    return create_poly_vector(res);
  }

  cpp11::writable::logicals do_intersect(cpp11::external_pointer<polyline_set> other) const {
    size_t final_size = std::max(size(), other->size());
    cpp11::writable::logicals res;
    res.reserve(final_size);

    for (size_t i = 0; i < final_size; ++i) {
      if (_storage[i % size()].is_na() || (*other)[i % other->size()].is_na()) {
        res.push_back(NA_LOGICAL);
        continue;
      }
      Polyline_set p = _storage[i % size()];
      Polyline_set p_other = (*other)[i % other->size()];
      bool is_intersecting = false;
      for (auto iter = p_other.edges_begin(); iter != p_other.edges_end(); iter++) {
        if (CGAL::do_intersect(p, iter->curve())) {
          is_intersecting = true;
          break;
        }
      }
      res.push_back((Rboolean) is_intersecting);
    }

    return res;
  }

  poly_vector_base_p p_union(cpp11::external_pointer<polyline_set> other) const {
    size_t final_size = std::max(size(), other->size());
    std::vector<Polyline_set> res;
    res.reserve(final_size);

    for (size_t i = 0; i < final_size; ++i) {
      if (_storage[i % size()].is_na() || (*other)[i % other->size()].is_na()) {
        res.push_back(Polyline_set::NA_value());
        continue;
      }
      Polyline_set p;
      CGAL::overlay(_storage[i % size()], (*other)[i % other->size()], p);
      res.push_back(p);
    }

    return create_poly_vector(res);
  }

  poly_vector_base_p p_intersection(cpp11::external_pointer<polyline_set> other) const {
    Overlay_traits<Polyline_set> traits;
    size_t final_size = std::max(size(), other->size());
    std::vector<Polyline_set> res;
    res.reserve(final_size);

    for (size_t i = 0; i < final_size; ++i) {
      if (_storage[i % size()].is_na() || (*other)[i % other->size()].is_na()) {
        res.push_back(Polyline_set::NA_value());
        continue;
      }
      Polyline_set p = _storage[i % size()];
      p.set_flag(0);
      Polyline_set p_other = (*other)[i % other->size()];
      p_other.set_flag(1);
      Polyline_set ol;
      CGAL::overlay(p, p_other, ol, traits);

      ol.remove_without_flag(2);

      res.push_back(ol);
    }

    return create_poly_vector(res);
  }

  poly_vector_base_p p_difference(cpp11::external_pointer<polyline_set> other) const {
    Overlay_traits<Polyline_set> traits;
    size_t final_size = std::max(size(), other->size());
    std::vector<Polyline_set> res;
    res.reserve(final_size);

    for (size_t i = 0; i < final_size; ++i) {
      if (_storage[i % size()].is_na() || (*other)[i % other->size()].is_na()) {
        res.push_back(Polyline_set::NA_value());
        continue;
      }
      Polyline_set p = _storage[i % size()];
      p.set_flag(0);
      Polyline_set p_other = (*other)[i % other->size()];
      p_other.set_flag(1);
      Polyline_set ol;
      CGAL::overlay(p, p_other, ol, traits);

      ol.remove_without_flag(0);

      res.push_back(ol);
    }

    return create_poly_vector(res);
  }

  poly_vector_base_p p_symmetric_difference(cpp11::external_pointer<polyline_set> other) const {
    Overlay_traits<Polyline_set> traits;
    size_t final_size = std::max(size(), other->size());
    std::vector<Polyline_set> res;
    res.reserve(final_size);

    for (size_t i = 0; i < final_size; ++i) {
      if (_storage[i % size()].is_na() || (*other)[i % other->size()].is_na()) {
        res.push_back(Polyline_set::NA_value());
        continue;
      }
      Polyline_set p = _storage[i % size()];
      p.set_flag(0);
      Polyline_set p_other = (*other)[i % other->size()];
      p_other.set_flag(1);
      Polyline_set ol;
      CGAL::overlay(p, p_other, ol, traits);

      ol.remove_with_flag(2);

      res.push_back(ol);
    }

    return create_poly_vector(res);
  }

  poly_vector_base_p cum_union() const {
    std::vector<Polyline_set> res;
    if (size() == 1) {
      res.push_back(_storage[0]);
    } else if (size() != 0) {
      if (any_na()) {
        res.push_back(Polyline_set::NA_value());
      } else {
        std::vector<Polyline_set> p(2);
        for (size_t i = 0; i < size(); ++i) {
          CGAL::overlay(p[0], _storage[i], p[1]);
          std::reverse(p.begin(), p.end());
          p[1].clear();
        }
        res.push_back(p[0]);
      }
    }
    return create_poly_vector(res);
  }

  poly_vector_base_p cum_intersection() const {
    Overlay_traits<Polyline_set> traits;
    std::vector<Polyline_set> res;
    if (size() == 1) {
      res.push_back(_storage[0]);
    } else if (size() != 0) {
      if (any_na()) {
        res.push_back(Polyline_set::NA_value());
      } else {
        std::vector<Polyline_set> p(3);
        p[2] = _storage[0];
        for (size_t i = 1; i < size(); ++i) {
          p[0] = p[2];
          p[0].set_flag(0);
          p[1] = _storage[i];
          p[1].set_flag(1);
          p[2].clear();
          CGAL::overlay(p[0], p[1], p[2], traits);
          p[2].remove_without_flag(2);
        }
        res.push_back(p[2]);
      }
    }
    return create_poly_vector(res);
  }

  poly_vector_base_p cum_difference() const {
    Overlay_traits<Polyline_set> traits;
    std::vector<Polyline_set> res;
    if (size() == 1) {
      res.push_back(_storage[0]);
    } else if (size() != 0) {
      if (any_na()) {
        res.push_back(Polyline_set::NA_value());
      } else {
        std::vector<Polyline_set> p(3);
        p[2] = _storage[0];
        for (size_t i = 1; i < size(); ++i) {
          p[0] = p[2];
          p[0].set_flag(0);
          p[1] = _storage[i];
          p[1].set_flag(1);
          p[2].clear();
          CGAL::overlay(p[0], p[1], p[2], traits);
          p[2].remove_without_flag(0);
        }
        res.push_back(p[2]);
      }
    }
    return create_poly_vector(res);
  }

  poly_vector_base_p cum_symmetric_difference() const {
    Overlay_traits<Polyline_set> traits;
    std::vector<Polyline_set> res;
    if (size() == 1) {
      res.push_back(_storage[0]);
    } else if (size() != 0) {
      if (any_na()) {
        res.push_back(Polyline_set::NA_value());
      } else {
        std::vector<Polyline_set> p(3);
        p[2] = _storage[0];
        for (size_t i = 1; i < size(); ++i) {
          p[0] = p[2];
          p[0].set_flag(0);
          p[1] = _storage[i];
          p[1].set_flag(1);
          p[2].clear();
          CGAL::overlay(p[0], p[1], p[2], traits);
          p[2].remove_with_flag(2);
        }
        res.push_back(p[2]);
      }
    }
    return create_poly_vector(res);
  }

  poly_vector_base_p union_polygon_set(const std::vector<Polygon_set> other) const {
    Overlay_traits<Polyline_set> traits;
    size_t final_size = std::max(size(), other.size());
    std::vector<Polyline_set> res;
    res.reserve(final_size);

    for (size_t i = 0; i < final_size; ++i) {
      if (_storage[i % size()].is_na() || other[i % other.size()].is_na()) {
        res.push_back(Polyline_set::NA_value());
        continue;
      }
      Polyline_set ps = _storage[i % size()];
      Polyline_set ps2;
      std::vector<Segment_2> segments;
      for (auto iter = other[i % other.size()].arrangement().faces_begin(); iter != other[i % other.size()].arrangement().faces_end(); iter++) {
        if (iter->is_unbounded()) continue;
        auto outer = iter->outer_ccb();
        if (outer != nullptr) {
          segments.clear();
          auto c_iter = outer;
          do {
            segments.emplace_back(c_iter->curve());
          } while (++c_iter != outer);
          ps2.insert(segments.begin(), segments.end());
        }
      }
      ps.set_flag(0);
      ps2.set_flag(1);
      Polyline_set ol;
      CGAL::overlay(ps, ps2, ol, traits);
      ps.clear();
      for (auto iter = ol.edges_begin(); iter != ol.edges_end(); iter++) {
        if ((iter->data() == 1) ||
            (iter->face()->data() <= 0 && iter->twin()->face()->data() <= 0)) {
          CGAL::insert_non_intersecting_curve(ps, iter->curve());
        }
      }

      res.push_back(ps);
    }
    return create_poly_vector(res);
  }

  poly_vector_base_p intersection_polygon_set(const std::vector<Polygon_set> other) const {
    Overlay_traits<Polyline_set> traits;
    size_t final_size = std::max(size(), other.size());
    std::vector<Polyline_set> res;
    res.reserve(final_size);

    for (size_t i = 0; i < final_size; ++i) {
      if (_storage[i % size()].is_na() || other[i % other.size()].is_na()) {
        res.push_back(Polyline_set::NA_value());
        continue;
      }
      Polyline_set ps = _storage[i % size()];
      Polyline_set ps2;
      std::vector<Segment_2> segments;
      for (auto iter = other[i % other.size()].arrangement().faces_begin(); iter != other[i % other.size()].arrangement().faces_end(); iter++) {
        if (iter->is_unbounded()) continue;
        auto outer = iter->outer_ccb();
        if (outer != nullptr) {
          segments.clear();
          auto c_iter = outer;
          do {
            segments.emplace_back(c_iter->curve());
          } while (++c_iter != outer);
          ps2.insert(segments.begin(), segments.end());
        }
      }
      ps.set_flag(0);
      ps2.set_flag(1);
      Polyline_set ol;
      CGAL::overlay(ps, ps2, ol, traits);
      ps.clear();
      for (auto iter = ol.edges_begin(); iter != ol.edges_end(); iter++) {
        if ((iter->data() == 1) ||
            (iter->face()->data() <= 0 && iter->twin()->face()->data() <= 0)) {
          if (iter->source()->data() == 2) CGAL::insert_point(ps, iter->source()->point());
          if (iter->target()->data() == 2) CGAL::insert_point(ps, iter->target()->point());
          continue;
        }
        CGAL::insert_non_intersecting_curve(ps, iter->curve());
      }

      res.push_back(ps);
    }
    return create_poly_vector(res);
  }

  poly_vector_base_p difference_polyline_polygon_set(const std::vector<Polygon_set> other) const {
    Overlay_traits<Polyline_set> traits;
    size_t final_size = std::max(size(), other.size());
    std::vector<Polyline_set> res;
    res.reserve(final_size);

    for (size_t i = 0; i < final_size; ++i) {
      if (_storage[i % size()].is_na() || other[i % other.size()].is_na()) {
        res.push_back(Polyline_set::NA_value());
        continue;
      }
      Polyline_set ps = _storage[i % size()];
      Polyline_set ps2;
      std::vector<Segment_2> segments;
      for (auto iter = other[i % other.size()].arrangement().faces_begin(); iter != other[i % other.size()].arrangement().faces_end(); iter++) {
        if (iter->is_unbounded()) continue;
        auto outer = iter->outer_ccb();
        if (outer != nullptr) {
          segments.clear();
          auto c_iter = outer;
          do {
            segments.emplace_back(c_iter->curve());
          } while (++c_iter != outer);
          ps2.insert(segments.begin(), segments.end());
        }
      }
      ps.set_flag(0);
      ps2.set_flag(1);
      Polyline_set ol;
      CGAL::overlay(ps, ps2, ol, traits);
      ps.clear();
      for (auto iter = ol.edges_begin(); iter != ol.edges_end(); iter++) {
        if (iter->data() == 1 || iter->face()->data() > 0 || iter->twin()->face()->data() > 0) {
          continue;
        }
        CGAL::insert_non_intersecting_curve(ps, iter->curve());
      }
      res.push_back(ps);
    }
    return create_poly_vector(res);
  }

  poly_vector_base_p difference_polygon_polyline_set(const std::vector<Polygon_set> other) const {
    Overlay_traits<Polyline_set> traits;
    size_t final_size = std::max(size(), other.size());
    std::vector<Polyline_set> res;
    res.reserve(final_size);

    for (size_t i = 0; i < final_size; ++i) {
      if (_storage[i % size()].is_na() || other[i % other.size()].is_na()) {
        res.push_back(Polyline_set::NA_value());
        continue;
      }
      Polyline_set ps = _storage[i % size()];
      Polyline_set ps2;
      std::vector<Segment_2> segments;
      for (auto iter = other[i % other.size()].arrangement().faces_begin(); iter != other[i % other.size()].arrangement().faces_end(); iter++) {
        if (iter->is_unbounded()) continue;
        auto outer = iter->outer_ccb();
        if (outer != nullptr) {
          segments.clear();
          auto c_iter = outer;
          do {
            segments.emplace_back(c_iter->curve());
          } while (++c_iter != outer);
          ps2.insert(segments.begin(), segments.end());
        }
      }
      ps.set_flag(0);
      ps2.set_flag(1);
      Polyline_set ol;
      CGAL::overlay(ps, ps2, ol, traits);
      ps.clear();
      for (auto iter = ol.faces_begin(); iter != ol.faces_end(); iter++) {
        if (iter->is_unbounded()) continue;
        if (iter->data() <= 0) continue;
        auto outer = iter->outer_ccb();
        if (outer != nullptr) {
          auto c_iter = outer;
          do {
            CGAL::insert(ps, c_iter->curve());
          } while (++c_iter != outer);
        }
        if (iter->number_of_holes() != 0) {
          for (auto h_iter = iter->holes_begin(); h_iter != iter->holes_end(); h_iter++) {
            auto inner = *h_iter;
            if (inner != nullptr) {
              auto i_iter = inner;
              do {
                CGAL::insert(ps, i_iter->curve());
              } while (++i_iter != inner);
            }
          }
        }
      }
      res.push_back(ps);
    }
    return create_poly_vector(res);
  }

  poly_vector_base_p symmetric_difference_polygon_set(const std::vector<Polygon_set> other) const {
    size_t final_size = std::max(size(), other.size());
    std::vector<Polyline_set> res;
    res.reserve(final_size);

    for (size_t i = 0; i < final_size; ++i) {
      if (_storage[i % size()].is_na() || other[i % other.size()].is_na()) {
        res.push_back(Polyline_set::NA_value());
        continue;
      }
      Polyline_set ps = _storage[i % size()];
      std::vector<Segment_2> segments;
      for (auto iter = other[i % other.size()].arrangement().faces_begin(); iter != other[i % other.size()].arrangement().faces_end(); iter++) {
        if (iter->is_unbounded()) continue;
        auto outer = iter->outer_ccb();
        if (outer != nullptr) {
          segments.clear();
          auto c_iter = outer;
          do {
            segments.emplace_back(c_iter->curve());
          } while (++c_iter != outer);
          ps.insert(segments.begin(), segments.end());
        }
      }
      res.push_back(ps);
    }
    return create_poly_vector(res);
  }
};

typedef cpp11::external_pointer<polyline_set> polyline_set_p;
