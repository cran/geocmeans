% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/shinyapp.R
\name{sp_clust_explorer}
\alias{sp_clust_explorer}
\title{Classification result explorer}
\usage{
sp_clust_explorer(
  object = NULL,
  spatial = NULL,
  membership = NULL,
  dataset = NULL,
  port = 8100,
  ...
)
}
\arguments{
\item{object}{A FCMres object, typically obtained from functions CMeans,
GCMeans, SFCMeans, SGFCMeans}

\item{spatial}{A feature collection (sf) used to map the observations. Only needed if object was not created
from rasters.}

\item{membership}{A matrix or a dataframe representing the membership values
obtained for each observation. If NULL, then the matrix is extracted from
object.}

\item{dataset}{A dataframe or matrix representing the data used for the
classification. If NULL, then the matrix is extracted from object.}

\item{port}{An integer of length 4 indicating the port on which to start the
Shiny app. Default is 8100}

\item{...}{Other parameters passed to the function runApp}
}
\description{
Start a local Shiny App to explore the results of a classification
}
\examples{
\dontrun{
data(LyonIris)

#selecting the columns for the analysis
AnalysisFields <-c("Lden","NO2","PM25","VegHautPrt","Pct0_14",
                   "Pct_65","Pct_Img","TxChom1564","Pct_brevet","NivVieMed")

#rescaling the columns
Data <- sf::st_drop_geometry(LyonIris[AnalysisFields])
for (Col in names(Data)){
  Data[[Col]] <- as.numeric(scale(Data[[Col]]))
}

Cmean <- CMeans(Data,4,1.5,500,standardize = FALSE, seed = 456, tol = 0.00001, verbose = FALSE)

sp_clust_explorer(Cmean, LyonIris)
}
}
