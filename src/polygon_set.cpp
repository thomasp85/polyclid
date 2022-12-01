#include <cpp11/list.hpp>
#include "polygon_set.h"
#include "polygon.h"
#include "polygon_modification.h"

[[cpp11::register]]
polygon_set_p create_polygonset_empty() {
  std::vector<Polygon_set> vec;
  polygon_set *result(new polygon_set(vec));
  return {result};
}

[[cpp11::register]]
polygon_set_p create_polygonset_polygon(polygon_p p) {
  std::vector<Polygon_set> vec;
  for (size_t i = 0; i < p->size(); ++i) {
    Polygon pl = p->get_storage()[i];
    if (pl.is_na()) {
      vec.push_back(Polygon_set::NA_value());
    } else {
      Polygon_set ps;
      ps.insert(polygon_make_valid_impl(pl));
      vec.push_back(ps);
    }
  }
  polygon_set *result(new polygon_set(vec));
  return {result};
}

[[cpp11::register]]
polygon_set_p create_polygonset_list(cpp11::list_of<polygon_p> p) {
  std::vector<Polygon_set> vec;
  for (R_xlen_t i = 0; i < p.size(); ++i) {
    polygon_set_p pv = create_polygonset_polygon(p[i]);
    Polygon_set ps;
    if (pv->size() > 0) {
      ps = (*pv)[0];
      if (!ps.is_na()) {
        for (size_t j = 1; j < pv->size(); ++j) {
          if ((*pv)[j].is_na()) {
            ps = Polygon_set::NA_value();
            break;
          }
          ps.join((*pv)[j]);
        }
      }
    }
    vec.push_back(ps);
  }
  polygon_set *result(new polygon_set(vec));
  return {result};
}

[[cpp11::register]]
cpp11::writable::integers polygonset_n_polygons(polygon_set_p geometries) {
  if (geometries.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  return geometries->n_polygons();
}

[[cpp11::register]]
poly_vector_base_p polygonset_get_polygon(polygon_set_p geometries, cpp11::integers which) {
  if (geometries.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  return geometries->get_polygon(which);
}

[[cpp11::register]]
poly_vector_base_p polygonset_get_all_polygon(polygon_set_p geometries) {
  if (geometries.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  return geometries->get_all_polygon();
}

[[cpp11::register]]
poly_vector_base_p polygonset_complement(polygon_set_p geometries) {
  if (geometries.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  return geometries->complement();
}

[[cpp11::register]]
poly_vector_base_p polygonset_union(polygon_set_p geometries, polygon_set_p other) {
  if (geometries.get() == nullptr || other.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  return geometries->p_union(other);
}

[[cpp11::register]]
poly_vector_base_p polygonset_intersection(polygon_set_p geometries, polygon_set_p other) {
  if (geometries.get() == nullptr || other.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  return geometries->p_intersection(other);
}

[[cpp11::register]]
poly_vector_base_p polygonset_difference(polygon_set_p geometries, polygon_set_p other) {
  if (geometries.get() == nullptr || other.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  return geometries->p_difference(other);
}

[[cpp11::register]]
poly_vector_base_p polygonset_symmetric_difference(polygon_set_p geometries, polygon_set_p other) {
  if (geometries.get() == nullptr || other.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  return geometries->p_symmetric_difference(other);
}

[[cpp11::register]]
poly_vector_base_p polygonset_cum_union(polygon_set_p geometries) {
  if (geometries.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  return geometries->cum_union();
}

[[cpp11::register]]
poly_vector_base_p polygonset_cum_intersection(polygon_set_p geometries) {
  if (geometries.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  return geometries->cum_intersection();
}

[[cpp11::register]]
poly_vector_base_p polygonset_cum_difference(polygon_set_p geometries) {
  if (geometries.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  return geometries->cum_difference();
}

[[cpp11::register]]
poly_vector_base_p polygonset_cum_symmetric_difference(polygon_set_p geometries) {
  if (geometries.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  return geometries->cum_symmetric_difference();
}

[[cpp11::register]]
cpp11::writable::logicals polygonset_do_intersect(polygon_set_p geometries, polygon_set_p other) {
  if (geometries.get() == nullptr || other.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  return geometries->do_intersect(other);
}

[[cpp11::register]]
poly_vector_base_p polygonset_locate(polygon_set_p geometries, SEXP points) {
  if (geometries.get() == nullptr) {
    cpp11::stop("Data structure pointer cleared from memory");
  }
  return geometries->locate(points);
}
