## ----setup, include=FALSE-----------------------------------------------------
knitr::opts_chunk$set(echo = TRUE)

## ----include=FALSE------------------------------------------------------------
#loading the pre-calculated results
load(system.file("extdata", "results_vignette_raster.rda",
                           package = "geocmeans", mustWork = TRUE))

## ----message=FALSE, warning=FALSE, fig.width= 4-------------------------------
library(geocmeans)
library(ggpubr)
library(future)
library(tmap)
library(viridis)
library(RColorBrewer)
library(terra)

Arcachon <- terra::rast(system.file("extdata/Littoral4_2154.tif", package = "geocmeans"))
names(Arcachon) <- c("blue", "green", "red", "infrared", "SWIR1", "SWIR2")

# show the pseudo-color image
terra::plotRGB(Arcachon, r = 3, g = 2, b = 1, stretch = "hist")

## ----message=FALSE, warning=FALSE---------------------------------------------
# converting the RasterBrick to a simple list of SpatRaster
dataset <- lapply(names(Arcachon), function(n){
  aband <- Arcachon[[n]]
  return(aband)
})

# giving a name to each band
names(dataset) <- names(Arcachon)

## ----message=FALSE, warning=FALSE, eval=FALSE---------------------------------
#  # finding an appropriate k and m values (using a multicore plan)
#  future::plan(future::multisession(workers = 6))
#  FCMvalues <- select_parameters.mc(algo = "FCM", data = dataset,
#                                 k = 5:10, m = seq(1.1,2,0.1), spconsist = FALSE,
#                                 indices = c("XieBeni.index", "Explained.inertia",
#                                             "Negentropy.index", "Silhouette.index"),
#                                 verbose = TRUE)

## ----message=FALSE, warning=FALSE, fig.width = 5, fig.align='center'----------
# plotting the silhouette index values
ggplot(FCMvalues) + 
  geom_raster(aes(x = m, y = k, fill = Silhouette.index)) + 
  geom_text(aes(x = m, y = k, label = round(Silhouette.index,2)), size = 2)+
  scale_fill_viridis() +
  coord_fixed(ratio=0.125) 

# plotting the explained inertia
ggplot(FCMvalues) + 
  geom_raster(aes(x = m, y = k, fill = Explained.inertia)) + 
  geom_text(aes(x = m, y = k, label = round(Explained.inertia,2)), size = 2)+
  scale_fill_viridis() +
  coord_fixed(ratio=0.125)


## ----message=FALSE, warning=FALSE, fig.width = 4, fig.align = "center"--------
FCM_result <- CMeans(dataset, k = 7, m = 1.5, standardize = TRUE,
                     verbose = FALSE, seed = 789, tol = 0.001, init = "kpp")

maps1 <- mapClusters(object = FCM_result, undecided = 0.45)

# plotting membership values for group 2
maps1$ProbaMaps[[2]] + theme(legend.position = "bottom")

# plotting membership values for group 5
maps1$ProbaMaps[[5]] + theme(legend.position = "bottom")

# plotting the most likely categories
maps1$ClusterPlot + theme(legend.position = "bottom") + scale_fill_brewer(palette = "Set2")

## ----message=FALSE, warning=FALSE, eval = FALSE-------------------------------
#  GFCMvalues <- select_parameters.mc(algo = "GFCM", data = dataset,
#                               k = 7, m = seq(1.1,2,0.1), beta = seq(0.1,0.9,0.1),
#                               spconsist = FALSE, verbose = TRUE, init = "kpp",
#                               indices = c("XieBeni.index", "Explained.inertia",
#                                           "Negentropy.index", "Silhouette.index"))

## ----message=FALSE, warning=FALSE, fig.width = 5, fig.align='center'----------

# plotting the explained inertia
ggplot(GFCMvalues) + 
  geom_raster(aes(x = m, y = beta, fill = Explained.inertia)) + 
  geom_text(aes(x = m, y = beta, label = round(Explained.inertia,2)), size = 2)+
  scale_fill_viridis() +
  coord_fixed(ratio=1)

# plotting the silhouette index
ggplot(GFCMvalues) + 
  geom_raster(aes(x = m, y = beta, fill = Silhouette.index)) + 
  geom_text(aes(x = m, y = beta, label = round(Silhouette.index,2)), size = 2)+
  scale_fill_viridis() +
  coord_fixed(ratio=1)


## ----message=FALSE, warning=FALSE, fig.width = 4, fig.align = "center"--------
GFCM_result <- GCMeans(dataset, k = 7, m = 1.5, beta = 0.5, standardize = TRUE,
                       verbose = FALSE, seed = 789, tol = 0.001)

# reorganizing the groups for an easier comparison
GFCM_result <- groups_matching(FCM_result, GFCM_result)

maps2 <- mapClusters(object = GFCM_result, undecided = 0.45)

# plotting membership values for group 2
maps2$ProbaMaps[[2]] + theme(legend.position = "bottom")

# plotting membership values for group 5
maps2$ProbaMaps[[5]] + theme(legend.position = "bottom")

# plotting the most likely categories
maps2$ClusterPlot + theme(legend.position = "bottom") +
  scale_fill_brewer(palette = "Set2")


## ----message=FALSE, warning=FALSE---------------------------------------------
w1 <- matrix(1, nrow = 3, ncol = 3)
w2 <- matrix(1, nrow = 5, ncol = 5)
w3 <- matrix(1, nrow = 7, ncol = 7)

## ----message=FALSE, warning=FALSE, eval = FALSE-------------------------------
#  future::plan(future::multisession(workers = 6))
#  SGFCMvalues <- select_parameters.mc(algo = "SGFCM", data = dataset, k = 7, m = 1.5,
#                               beta = 0.5, alpha = seq(0.5,2,0.1),
#                               window = list(w1,w2,w3),
#                               spconsist = TRUE, nrep = 5,
#                               verbose = TRUE, chunk_size = 4,
#                               seed = 456, init = "kpp",
#                               indices = c("XieBeni.index", "Explained.inertia",
#                                           "Negentropy.index", "Silhouette.index"))

## ----message=FALSE, warning=FALSE, fig.width = 5, fig.align='center'----------
dict <- data.frame(
  w = c(1,2,3),
  window = c("3x3","5x5","7x7")
)

SGFCMvalues$window <- dict$window[match(SGFCMvalues$window,dict$w)]

# showing the silhouette index
ggplot(SGFCMvalues) + 
  geom_raster(aes(x = alpha, y = window, fill = Silhouette.index)) + 
  geom_text(aes(x = alpha, y = window, label = round(Silhouette.index,2)), size = 1.5)+
  scale_fill_viridis() +
  coord_fixed(ratio=0.125)

# showing the explained inertia
ggplot(SGFCMvalues) + 
  geom_raster(aes(x = alpha, y = window, fill = Explained.inertia)) + 
  geom_text(aes(x = alpha, y = window, label = round(Explained.inertia,2)), size = 1.5)+
  scale_fill_viridis() +
  coord_fixed(ratio=0.125)

# showing the spatial inconsistency
ggplot(SGFCMvalues) + 
  geom_raster(aes(x = alpha, y = window, fill = spConsistency)) + 
  geom_text(aes(x = alpha, y = window, label = round(spConsistency,2)), size = 1.5)+
  scale_fill_viridis() +
  coord_fixed(ratio=0.125)


## ----message=FALSE, warning=FALSE, fig.width = 4, fig.align = "center"--------
SGFCM_result <- SGFCMeans(dataset, k = 7, m = 1.5, standardize = TRUE,
                       lag_method = "mean",
                       window = w1, alpha = 0.9, beta = 0.5,
                       seed = 789, tol = 0.001, verbose = FALSE, init = "kpp")

# reorganizing the groups for an easier comparison
SGFCM_result <- groups_matching(FCM_result, SGFCM_result)

maps3 <- mapClusters(object = SGFCM_result, undecided = 0.2)

# plotting membership values for group 2
maps3$ProbaMaps[[2]] + theme(legend.position = "bottom")

# plotting membership values for group 5
maps3$ProbaMaps[[5]] + theme(legend.position = "bottom")


# plotting the most likely categories
maps3$ClusterPlot + theme(legend.position = "bottom") + 
  scale_fill_brewer(palette = "Set2")


## ----message=FALSE, warning=FALSE---------------------------------------------
cluster_results <- list(FCM_result, GFCM_result, SGFCM_result)

indices <- sapply(cluster_results, function(clust){
  c(calcexplainedInertia(clust$Data, clust$Belongings),
    calcSilhouetteIdx(clust$Data, clust$Belongings),
    spConsistency(clust, window = w1, nrep = 5)$Mean)
})

colnames(indices) <- c("FCM", "GFCM", "SGFCM")
rownames(indices) <- c("explained inertia", "silhouette index", "spatial inconsistency")

knitr::kable(indices, digits = 3)


## ----message=FALSE, warning=FALSE---------------------------------------------
diagGFCM <- spatialDiag(GFCM_result, window = matrix(1, nrow = 3, ncol = 3), nrep = 5)

## ----message=FALSE, warning=FALSE---------------------------------------------
knitr::kable(diagGFCM$MoranValues, digits = 3, row.names = FALSE)

## ----message=FALSE, warning=FALSE, fig.width = 6, fig.align = "center"--------
library(classInt)

# calculating the local Moran I values
loc_moran4 <- calc_local_moran_raster(GFCM_result$rasters$group4,w1)
loc_moran5 <- calc_local_moran_raster(GFCM_result$rasters$group5,w1)

# mapping the values
cols <- rev(RColorBrewer::brewer.pal(n = 8, "Spectral"))
vals <- terra::values(loc_moran4, mat = FALSE)
limits <- classIntervals(vals,  n = 8, style = "kmeans") 
plot(loc_moran4, col = cols, breaks = limits$brks)

vals <- terra::values(loc_moran5, mat = FALSE)
limits <- classIntervals(vals,  n = 8, style = "kmeans") 
plot(loc_moran5, col = cols, breaks = limits$brks)


## ----message=FALSE, warning=FALSE, fig.width = 6, fig.align = "center"--------

cols <- RColorBrewer::brewer.pal(n = 7, "Greys")
vals <- terra::values(diagGFCM$Elsa, mat = FALSE)
limits <- classIntervals(vals[!is.na(vals)],  n = 7, style = "kmeans") 
plot(diagGFCM$Elsa, col = cols, breaks = limits$brks)


## ----message=FALSE, warning=FALSE, fig.width = 6, fig.align = "center"--------
fuzzy_elsa_rast <- calcFuzzyELSA(GFCM_result,window = matrix(1,nrow = 3, ncol = 3))


cols <- RColorBrewer::brewer.pal(n = 7, "Greys")
vals <- terra::values(fuzzy_elsa_rast, mat = FALSE)
limits <- classIntervals(vals[!is.na(vals)],  n = 7, style = "kmeans") 
plot(fuzzy_elsa_rast, col = cols, breaks = limits$brks)


