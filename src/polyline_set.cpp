#include <cpp11/list.hpp>
#include "polyline_set.h"
#include "polygon_set.h"
#include "polyline.h"

[[cpp11::register]]
polyline_set_p create_polylineset_empty() {
  std::vector<Polyline_set> vec;
  polyline_set *result(new polyline_set(vec));
  return {result};
}

[[cpp11::register]]
polyline_set_p create_polylineset_polyline(polyline_p p) {
  std::vector<Polyline_set> vec;
  for (size_t i = 0; i < p->size(); ++i) {
    Polyline pl = p->get_storage()[i];
    if (pl.is_na()) {
      vec.push_back(Polyline_set::NA_value());
    } else {
      Polyline_set ps(pl.edges_begin(), pl.edges_end());
      vec.push_back(ps);
    }
  }
  polyline_set *result(new polyline_set(vec));
  return {result};
}

[[cpp11::register]]
polyline_set_p create_polylineset_list(cpp11::list_of<polyline_p> p) {
  std::vector<Polyline_set> vec;
  for (R_xlen_t i = 0; i < p.size(); ++i) {
    polyline_set_p pv = create_polylineset_polyline(p[i]);
    std::vector<Polyline_set> p(2);
    if (pv->size() > 0) {
      for (size_t j = 0; j < pv->size(); ++j) {
        if ((*pv)[j].is_na()) {
          p[0] = Polyline_set::NA_value();
          break;
        }
        CGAL::overlay(p[0], (*pv)[j], p[1]);
        std::reverse(p.begin(), p.end());
        p[1].clear();
      }
    }
    vec.push_back(p[0]);
  }
  polyline_set *result(new polyline_set(vec));
  return {result};
}

[[cpp11::register]]
cpp11::writable::integers polylineset_n_polylines(polyline_set_p geometries) {
  if (geometries.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  return geometries->n_polylines();
}

[[cpp11::register]]
cpp11::writable::integers polylineset_n_polylines_simplified(polyline_set_p geometries) {
  if (geometries.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  return geometries->n_polylines_simplified();
}

[[cpp11::register]]
poly_vector_base_p polylineset_get_polylines(polyline_set_p geometries, cpp11::integers which) {
  if (geometries.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  return geometries->get_polyline(which);
}

[[cpp11::register]]
poly_vector_base_p polylineset_get_all_polylines(polyline_set_p geometries) {
  if (geometries.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  return geometries->get_all_polylines();
}

[[cpp11::register]]
poly_vector_base_p polylineset_get_polylines_simplified(polyline_set_p geometries, cpp11::integers which) {
  if (geometries.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  return geometries->get_polylines_simplified(which);
}

[[cpp11::register]]
poly_vector_base_p polylineset_get_all_polylines_simplified(polyline_set_p geometries) {
  if (geometries.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  return geometries->get_all_polylines_simplified();
}

[[cpp11::register]]
cpp11::writable::integers polylineset_vert_degree(polyline_set_p geometries) {
  if (geometries.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  return geometries->vert_degree();
}

[[cpp11::register]]
cpp11::writable::list polylineset_vert_neighbors(polyline_set_p geometries) {
  if (geometries.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  return geometries->vert_neighbors();
}

[[cpp11::register]]
cpp11::writable::list polylineset_polyline_verts(polyline_set_p geometries) {
  if (geometries.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  return geometries->polyline_verts();
}

[[cpp11::register]]
cpp11::writable::integers polylineset_n_faces(polyline_set_p geometries) {
  if (geometries.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  return geometries->n_faces();
}

[[cpp11::register]]
poly_vector_base_p polylineset_get_faces(polyline_set_p geometries) {
  if (geometries.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  return geometries->get_faces();
}

[[cpp11::register]]
poly_vector_base_p polylineset_union(polyline_set_p geometries, polyline_set_p other) {
  if (geometries.get() == nullptr || other.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  return geometries->p_union(other);
}

[[cpp11::register]]
poly_vector_base_p polylineset_intersection(polyline_set_p geometries, polyline_set_p other) {
  if (geometries.get() == nullptr || other.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  return geometries->p_intersection(other);
}

[[cpp11::register]]
poly_vector_base_p polylineset_difference(polyline_set_p geometries, polyline_set_p other) {
  if (geometries.get() == nullptr || other.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  return geometries->p_difference(other);
}

[[cpp11::register]]
poly_vector_base_p polylineset_symmetric_difference(polyline_set_p geometries, polyline_set_p other) {
  if (geometries.get() == nullptr || other.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  return geometries->p_symmetric_difference(other);
}

[[cpp11::register]]
poly_vector_base_p polylineset_cum_union(polyline_set_p geometries) {
  if (geometries.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  return geometries->cum_union();
}

[[cpp11::register]]
poly_vector_base_p polylineset_cum_intersection(polyline_set_p geometries) {
  if (geometries.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  return geometries->cum_intersection();
}

[[cpp11::register]]
poly_vector_base_p polylineset_cum_difference(polyline_set_p geometries) {
  if (geometries.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  return geometries->cum_difference();
}

[[cpp11::register]]
poly_vector_base_p polylineset_cum_symmetric_difference(polyline_set_p geometries) {
  if (geometries.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  return geometries->cum_symmetric_difference();
}

[[cpp11::register]]
cpp11::writable::logicals polylineset_do_intersect(polyline_set_p geometries, polyline_set_p other) {
  if (geometries.get() == nullptr || other.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  return geometries->do_intersect(other);
}

[[cpp11::register]]
poly_vector_base_p polylineset_polygonset_union(polyline_set_p geometries, polygon_set_p other) {
  if (geometries.get() == nullptr || other.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  return geometries->union_polygon_set(other->get_storage());
}

[[cpp11::register]]
poly_vector_base_p polylineset_polygonset_intersection(polyline_set_p geometries, polygon_set_p other) {
  if (geometries.get() == nullptr || other.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  return geometries->intersection_polygon_set(other->get_storage());
}

[[cpp11::register]]
poly_vector_base_p polylineset_polygonset_difference(polyline_set_p geometries, polygon_set_p other) {
  if (geometries.get() == nullptr || other.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  return geometries->difference_polyline_polygon_set(other->get_storage());
}

[[cpp11::register]]
poly_vector_base_p polygonset_polylineset_difference(polyline_set_p geometries, polygon_set_p other) {
  if (geometries.get() == nullptr || other.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  return geometries->difference_polygon_polyline_set(other->get_storage());
}

[[cpp11::register]]
poly_vector_base_p polylineset_polygonset_symmetric_difference(polyline_set_p geometries, polygon_set_p other) {
  if (geometries.get() == nullptr || other.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  return geometries->symmetric_difference_polygon_set(other->get_storage());
}
