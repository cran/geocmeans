% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{adj_spconsist_arr_window_globstd}
\alias{adj_spconsist_arr_window_globstd}
\title{Adjusted spatial inconsistency index for rasters}
\usage{
adj_spconsist_arr_window_globstd(data, memberships, window, mindist = 1e-11)
}
\arguments{
\item{data}{an arma cube of dimension nr,nc,ns}

\item{memberships}{an arma cube of dimension nr, nc, ks}

\item{window}{a matrix representing the neighbouring of each pixel}

\item{mindist}{A minimum value for distance between two observations. If two
neighbours have exactly the same values, then the euclidean distance
between them is 0, leading to an infinite spatial weight. In that case,
the minimum distance is used instead of 0.}
}
\value{
a double, the adjusted spatial inconsitency index
}
\description{
Adjusted spatial inconsistency index for rasters
}
\keyword{internal}
