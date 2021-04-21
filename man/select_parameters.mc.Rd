% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/side_functions.R
\name{select_parameters.mc}
\alias{select_parameters.mc}
\alias{selectParameters.mc}
\title{Select parameters for clustering algorithm (multicore)}
\usage{
select_parameters.mc(
  algo,
  data,
  k,
  m,
  alpha = NA,
  beta = NA,
  nblistw = NULL,
  lag_method = "mean",
  spconsist = TRUE,
  classidx = TRUE,
  standardize = TRUE,
  maxiter = 500,
  tol = 0.01,
  seed = NULL,
  chunk_size = 100,
  verbose = FALSE
)

selectParameters.mc(
  algo,
  data,
  k,
  m,
  alpha = NA,
  beta = NA,
  nblistw = NULL,
  lag_method = "mean",
  spconsist = TRUE,
  classidx = TRUE,
  standardize = TRUE,
  maxiter = 500,
  tol = 0.01,
  seed = NULL,
  chunk_size = 100,
  verbose = FALSE
)
}
\arguments{
\item{algo}{A string indicating which method to use (FCM, GFCM, SFCM, SGFCM)}

\item{data}{A dataframe with numeric columns}

\item{k}{A sequence of values for k to test (>=2)}

\item{m}{A sequence of values for m to test}

\item{alpha}{A sequence of values for alpha to test (NULL if not required)}

\item{beta}{A sequence of values for beta to test (NULL if not required)}

\item{nblistw}{A list of list.w objects describing the neighbours typically
produced by the spdep package (NULL if not required)}

\item{lag_method}{A string indicating if a classical lag must be used
("mean") or if a weighted median must be used ("median"). Both can be
tested by specifying a vector : c("mean","median")}

\item{spconsist}{A boolean indicating if the spatial consistency must be
calculated}

\item{classidx}{A boolean indicating if the quality of classification
indices must be calculated}

\item{standardize}{A boolean to specify if the variable must be centered and
reduce (default = True)}

\item{maxiter}{An integer for the maximum number of iteration}

\item{tol}{The tolerance criterion used in the evaluateMatrices function for
convergence assessment}

\item{seed}{An integer used for random number generation. It ensures that the
start centers will be the same if the same integer is selected.}

\item{chunk_size}{The size of a chunk used for multiprocessing. Default is 100.}

\item{verbose}{A boolean indicating if a progressbar should be displayed}
}
\value{
A dataframe with indicators assessing the quality of classifications
}
\description{
Function to select the parameters for a clustering algorithm.
This version of the function allows to use a plan defined with the package
future to reduce calculation time.
}
\examples{
\donttest{
data(LyonIris)
AnalysisFields <-c("Lden","NO2","PM25","VegHautPrt","Pct0_14","Pct_65","Pct_Img",
"TxChom1564","Pct_brevet","NivVieMed")
dataset <- LyonIris@data[AnalysisFields]
queen <- spdep::poly2nb(LyonIris,queen=TRUE)
Wqueen <- spdep::nb2listw(queen,style="W")
future::plan(future::multiprocess(workers=2))
#set spconsist to TRUE to calculate the spatial consistency indicator
#FALSE here to reduce the time during package check
values <- select_parameters.mc("SFCM", dataset, k = 5, m = seq(1,2.5,0.1),
    alpha = seq(0,2,0.1), nblistw = Wqueen, spconsist=FALSE)
\dontshow{
   ## R CMD check: make sure any open connections are closed afterward
   if (!inherits(future::plan(), "sequential")) future::plan(future::sequential)
}
}
\donttest{
data(LyonIris)
AnalysisFields <-c("Lden","NO2","PM25","VegHautPrt","Pct0_14","Pct_65","Pct_Img",
"TxChom1564","Pct_brevet","NivVieMed")
dataset <- LyonIris@data[AnalysisFields]
queen <- spdep::poly2nb(LyonIris,queen=TRUE)
Wqueen <- spdep::nb2listw(queen,style="W")
future::plan(future::multiprocess(workers=2))
#set spconsist to TRUE to calculate the spatial consistency indicator
#FALSE here to reduce the time during package check
values <- select_parameters.mc("SFCM", dataset, k = 5, m = seq(1,2.5,0.1),
    alpha = seq(0,2,0.1), nblistw = Wqueen, spconsist=FALSE)
\dontshow{
   ## R CMD check: make sure any open connections are closed afterward
   if (!inherits(future::plan(), "sequential")) future::plan(future::sequential)
}
}
}