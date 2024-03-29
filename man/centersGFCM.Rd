% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/assembling.R
\name{centersGFCM}
\alias{centersGFCM}
\title{center matrix calculator for GFCM algorithm}
\usage{
centersGFCM(data, centers, belongmatrix, dots)
}
\arguments{
\item{data}{a matrix (the dataset used for clustering)}

\item{centers}{a matrix (the centers of the clusters)}

\item{belongmatrix}{a matrix with the membership values}

\item{dots}{a list of other arguments specific to FCM}
}
\value{
a matrix with the new centers
}
\description{
center matrix calculator for GFCM algorithm
}
\keyword{internal}
