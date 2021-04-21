#' geocmeans: A package implementing methods for spatially constrained c-ceans
#' algorithm
#'
#' The geocmeans package implements a modified c-means algorithm more suited to
#' work with spatial data (characterized by spatial autocorrelation). The
#' spatial information is introduced with a spatial weight matrix W (n * n)
#' where wij indicate the strength of the spatial relationship between the
#' observations i and j. It is recommended to use a matrix standardized by row
#' (so that the sum of each row is 1). More specifically, the spatial c-means
#' combine the euclidean distance of each observation in the data matrix X to
#' each center with the euclidean distance of the lagged version of X by W (WX).
#' A parameter alpha controls for the weight of the lagged matrix. If
#' alpha = 0, then the spatial c-means is equal to a classical c-means. If alpha
#' = 1, then the weights given to  X and WX are equals. If alpha = 2, then the
#' weight of WX is twice the one of X and so on.
#' An index to measure the spatial consistency of a classification is proposed in
#' this package.
#'
#'
#' @docType package
#' @name geocmeans
NULL