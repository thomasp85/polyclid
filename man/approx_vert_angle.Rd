% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/vert_stat.R
\name{approx_vert_angle}
\alias{approx_vert_angle}
\title{Calculate angle at vertices of polylines or polygons}
\usage{
approx_vert_angle(x, ...)
}
\arguments{
\item{x}{A \code{polyclid_polyline} or \code{polyclid_polygon} vector}

\item{...}{Arguments passed on to methods}
}
\value{
A numeric vector of matchng the vertices returned by \code{\link[euclid:subgeometries]{euclid::vert()}}
}
\description{
This function will calculate the angle at each vertex of a polygon or
polyline. The measured angle is that on the left side of the line, ie if it
turns left the angle will be between 0 and pi, and if it turns right it will
be between pi and 2pi.
}
\examples{
# Polylines
sine <- polyline(
  seq(0, 2*pi, length.out = 20),
  sin(seq(0, 2*pi, length.out = 20))
)
approx_vert_angle(sine)

# Polygons
poly <- polygon(
  c(0, 10, 10, 0, 1, 5, 1),
  c(0, 0, 10, 10, 1, 1, 5),
  hole_id = c(1, 1, 1, 1, 2, 2, 2)
)
approx_vert_angle(poly)

}
