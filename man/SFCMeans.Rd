% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/FCM.R
\name{SFCMeans}
\alias{SFCMeans}
\title{SFCMeans}
\usage{
SFCMeans(
  data,
  nblistw = NULL,
  k,
  m,
  alpha,
  lag_method = "mean",
  window = NULL,
  noise_cluster = FALSE,
  delta = NULL,
  maxiter = 500,
  tol = 0.01,
  standardize = TRUE,
  robust = FALSE,
  verbose = TRUE,
  init = "random",
  seed = NULL
)
}
\arguments{
\item{data}{A dataframe with only numerical variables. Can also be a list of
rasters (produced by the package raster). In that case, each raster is
considered as a variable and each pixel is an observation. Pixels with NA
values are not used during the classification.}

\item{nblistw}{A list.w object describing the neighbours typically produced
by the spdep package. Required if data is a dataframe, see the parameter window
if you use a list of rasters as input.}

\item{k}{An integer describing the number of cluster to find}

\item{m}{A float for the fuzziness degree}

\item{alpha}{A float representing the weight of the space in the analysis (0
is a typical fuzzy-c-mean algorithm, 1 is balanced between the two
dimensions, 2 is twice the weight for space)}

\item{lag_method}{A string indicating if a classical lag must be used
("mean") or if a weighted median must be used ("median"). When working with rasters, a function
can be given (or a string which will be parsed). It will be applied to all the pixels values
in the matrix designated by the parameter window and weighted according to the values of this matrix.
Typically, to obtain an average of the pixels in a 3x3 matrix one could use the function sum (or "sum")
and set the window as: window <- matrix(1/9,nrow = 3, ncol = 3). There is one special case when working 
with rasters: one can specify "nl" (standing for non-local) which calculated a lagged version of the 
input rasters, using the inverse of the euclidean distance as spatial weights (see the section Advanced 
examples in the vignette introduction for more details).}

\item{window}{If data is a list of rasters, then a window must be specified instead of
a list.w object. It will be used to calculate a focal function on each raster. The
window must be a square numeric matrix with odd dimensions (such 3x3). The values in
the matrix indicate the weight to give to each pixel and the centre of the matrix is
the centre of the focal function.}

\item{noise_cluster}{A boolean indicatong if a noise cluster must be added to the solution (see details)}

\item{delta}{A float giving the distance of the noise cluster to each observation}

\item{maxiter}{An integer for the maximum number of iterations}

\item{tol}{The tolerance criterion used in the evaluateMatrices function for
convergence assessment}

\item{standardize}{A boolean to specify if the variables must be centred and
reduced (default = True)}

\item{robust}{A boolean indicating if the "robust" version of the algorithm must be used (see details)}

\item{verbose}{A boolean to specify if the progress should be printed}

\item{init}{A string indicating how the initial centres must be selected. "random"
indicates that random observations are used as centres. "kpp" use a distance-based method
resulting in more dispersed centres at the beginning. Both of them are heuristic.}

\item{seed}{An integer used for random number generation. It ensures that the
starting centres will be the same if the same value is selected.}
}
\value{
An S3 object of class FCMres with the following slots
\itemize{
        \item Centers: a dataframe describing the final centers of the groups
        \item Belongings: the final membership matrix
        \item Groups: a vector with the names of the most likely group for each observation
        \item Data: the dataset used to perform the clustering (might be standardized)
        \item isRaster: TRUE if rasters were used as input data, FALSE otherwise
        \item k: the number of groups
        \item m: the fuzyness degree
        \item alpha: the spatial weighting parameter (if SFCM or SGFCM)
        \item beta: beta parameter for generalized version of FCM (GFCM or SGFCM)
        \item algo: the name of the algorithm used
        \item rasters: a list of rasters with membership values and the most likely group (if rasters were used)
        \item missing: a boolean vector indicating raster cell with data (TRUE) and with NA (FALSE) (if rasters were used)
        \item maxiter: the maximum number of iterations used
        \item tol: the convergence criterio
        \item lag_method: the lag function used (if SFCM or SGFCM)
        \item nblistw: the neighbours list used (if vector data were used for SFCM or SGFCM)
        \item window: the window used (if raster data were used for SFCM or SGFCM)
}
}
\description{
spatial version of the c-mean algorithm (SFCMeans, FCM_S1)
}
\details{
The implementation is based on the following article : \doi{10.1016/j.patcog.2006.07.011}.\cr

the membership matrix (u) is calculated as follow \cr
\deqn{u_{ik} = \frac{(||x_{k} - v{_i}||^2 + \alpha||\bar{x_{k}} - v{_i}||^2)^{(-1/(m-1))}}{\sum_{j=1}^c(||x_{k} - v{_j}||^2 + \alpha||\bar{x_{k}} - v{_j}||^2)^{(-1/(m-1))}}}

the centers of the groups are updated with the following formula
\deqn{v_{i} = \frac{\sum_{k=1}^N u_{ik}^m(x_{k} + \alpha\bar{x_{k}})}{(1 + \alpha)\sum_{k=1}^N u_{ik}^m}}

with
\itemize{
\item vi the center of the group vi
\item xk the data point k
\item xk_bar the spatially lagged data point k
}
}
\examples{
data(LyonIris)
AnalysisFields <-c("Lden","NO2","PM25","VegHautPrt","Pct0_14","Pct_65","Pct_Img",
"TxChom1564","Pct_brevet","NivVieMed")
dataset <- sf::st_drop_geometry(LyonIris[AnalysisFields])
queen <- spdep::poly2nb(LyonIris,queen=TRUE)
Wqueen <- spdep::nb2listw(queen,style="W")
result <- SFCMeans(dataset, Wqueen,k = 5, m = 1.5, alpha = 1.5, standardize = TRUE)
}
