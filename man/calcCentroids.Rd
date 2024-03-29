% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{calcCentroids}
\alias{calcCentroids}
\title{Calculate the centroids}
\arguments{
\item{data}{A Numeric matrix representing the observed data with n rows
and p columns}

\item{belongmatrix}{A n X k matrix giving for each observation n, its
probability to belong to the cluster k}

\item{m}{A float representing the fuzziness degree}
}
\value{
A a matrix with the centers calculated for each cluster
}
\description{
Calculate the new centroids of the clusters based on the membership matrix
for a classical FCM.
}
\keyword{internal}
