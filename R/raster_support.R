#' @title Check the shape of a window
#'
#' @description Check is a window is squarred and have odd dimensions
#'
#' @param w A matrix
#' @keywords internal
#' @examples
#' # this is an internal function, no example provided
check_window <- function(w){
  dims <- dim(w)
  if(dims[[1]] != dims[[2]]){
    stop("The window provided must be a square matrix")
  }
  if(dims[[1]] %%2 == 0){
    stop("The width and height of the window must be an odd number")
  }
}


#' @title Calculate lagged values for a raster dataset
#'
#' @description Calculate lagged values for a raster dataset given a window
#' and an agregation function
#'
#' @param w A matrix
#' @param dataset A list of rasters
#' @param fun A string giving the name of a function or a function or "nl"
#' for non-local method
#' @param missing_pxl A boolean vector of missing (FALSE) pixels
#' @keywords internal
#' @examples
#' # this is an internal function, no example provided
calcWdataRaster <- function(w, dataset, fun, missing_pxl){

  if(class(fun) != "function"){
    fun <- as.character(fun)
    if(fun != "nl"){
      useNL <- FALSE
      tryCatch(fun <- eval(parse(text=fun)),
               error = function(e)
                 print("When using rasters, the parameter lag_method must be a function or a string
                 that can be parsed to a function like sum, mean, min, max ...,
                 Note that this function is applied to the pixels values multiplied by the weights in the window.
                 There is one exception : 'nl_mean', see help(Cmeans).")
      )
    }else{
      useNL <- TRUE
    }
  }
  if(useNL == FALSE){
    Wdatamatrix <- do.call(cbind,lapply(dataset, function(band){
      wraster <- focal(band, w, fun, na.rm = TRUE, pad = TRUE)
      return(raster::values(wraster))
    }))
  }else{
    # step1: creating an array with all the matrices
    mats <- lapply(dataset, raster::as.matrix)
    arr <- array(do.call(c,mats),c(nrow(mats[[1]]), ncol(mats[[1]]), length(mats)))
    # step2 : getting the lagged version of the array
    arr_lag <- focal_adj_mean_arr_window(arr, w)
    # step3 : creating the Wdatamatrix
    nsl <- dim(arr_lag)[[3]]
    Wdatamatrix <- do.call(cbind, lapply(1:nsl, function(i){
      vec <- arr_lag[,,i]
      dim(vec) <- NULL
      return(vec)
    })
    )
  }
  Wdata_class <- Wdatamatrix[missing_pxl,]
  colnames(Wdata_class) <- names(dataset)
  return(Wdata_class)
}


#' @title Check dimensions of a list of rasters
#'
#' @description Check if all the rasters in a list have the same dimensions
#'
#' @param rasters A list of rasters
#' @keywords internal
#' @examples
#' # this is an internal function, no example provided
check_raters_dims <- function(rasters){
  dims <- lapply(rasters, function(i){
    return(c(raster::nrow(i), raster::ncol(i)))
  })
  dims <- do.call(rbind, dims)
  ref <- dims[1,]
  test1 <- !dims[,1] == ref[[1]]
  test2 <- !dims[,2] == ref[[2]]
  if(any(test1) | any(test2)){
    stop("The rasters provided do not have the same dimensions")
  }
}

#' @title Raster data preparation
#'
#' @description Prepare a raster dataset
#'
#' @param dataset A list of rasters
#' @param w The window to use in the focal function
#' @param fun the function to use as the focal function
#' @param standardize A boolean to specify if the variable must be centered and
#'   reduced (default = True)
#' @return A list with the required elements to perform clustering
#' @importFrom raster focal ncell
#' @keywords internal
#' @examples
#' # this is an internal function, no example provided
input_raster_data <- function(dataset, w = NULL, fun = sum, standardize = TRUE){

  if(is.null(w) == FALSE){
    check_raters_dims(dataset)
    check_window(w)
  }

  #   if(class(fun) != "function"){
  #     if(fun != "nl"){
  #       useNL <- FALSE
  #       tryCatch(fun <- eval(parse(text=fun)),
  #                error = function(e)
  #                  print("When using rasters, the parameter lag_method must be a function or a string
  #                that can be parsed to a function like sum, mean, min, max ...,
  #                Note that this function is applied to the pixels values multiplied by the weights in the window.
  #                There is one exception : 'nl_mean', see help(Cmeans).")
  #       )
  #     }else{
  #       useNL <- TRUE
  #     }
  #   }
  # }

  refdim <- dim(dataset[[1]])
  for(band in dataset){
    dim2 <- dim(band)
    if(sum(abs(dim2 - refdim)) != 0){
      stop("The rasters provided must have EXACTLY the same dimensions.")
    }
  }

  if(is.null(names(dataset))){
    okname <- paste("band",1:length(dataset), sep="")
  }else{
    okname <- names(dataset)
  }

  if(standardize){
    for(i in 1:length(dataset)){
      dataset[[i]] <- raster::scale(dataset[[i]])
    }
  }

  #step1 : prepare the regular dataset
  datamatrix <- do.call(cbind,lapply(dataset, function(band){
    raster::values(band)
  }))

  #step2 : only keep the non-missing valuez
  missing_pxl <- complete.cases(datamatrix)
  data_class <- datamatrix[missing_pxl,]
  colnames(data_class) <- okname

  #step3 : calculating Wdata if necessary
  if(is.null(w) == FALSE){
    Wdata_class <- calcWdataRaster(w, dataset, fun, missing_pxl)
    # if(useNL == FALSE){
    #   Wdatamatrix <- do.call(cbind,lapply(dataset, function(band){
    #     wraster <- focal(band, w, fun, na.rm = TRUE, pad = TRUE)
    #     return(raster::values(wraster))
    #   }))
    # }else{
    #   # step1: creating an array with all the matrices
    #   mats <- lapply(dataset, raster::as.matrix)
    #   arr <- array(do.call(c,mats),c(nrow(mats[[1]]), ncol(mats[[1]]), length(mats)))
    #   # step2 : getting the lagged version of the array
    #   arr_lag <- focal_adj_mean_arr_window(arr, w)
    #   # step3 : creating the Wdatamatrix
    #   nsl <- dim(arr_lag)[[3]]
    #   Wdatamatrix <- do.call(cbind, lapply(1:nsl, function(i){
    #       vec <- arr_lag[,,i]
    #       dim(vec) <- NULL
    #       return(vec)
    #     })
    #   )
    # }
    # Wdata_class <- Wdatamatrix[missing_pxl,]
    colnames(Wdata_class) <- okname

  }else{
    Wdata_class <- NULL
  }

  return(list("data" = data_class,
              "wdata" = Wdata_class,
              "missing" = missing_pxl,
              "rst" = dataset[[1]],
              "window" = w
              ))
}



#' @title Raster result transformation
#'
#' @description Adapt the results if a raster is used
#'
#' @param object A FCMres object
#' @param missing A boolean indicating which pixels have no missing values
#' @param rst A raster object used as template to structure the results
#' @return A FCMres object with isRaster = TRUE
#' @keywords internal
#' @examples
#' # this is an internal function, no example provided
output_raster_data <- function(object, missing, rst){
  # object is created by a FCM like function
  # missing is indicating which pixels are complete
  # rst is a basic raster to duplicate it

  #step1 : creating a raster for each cluster
  rasters <- lapply(1:ncol(object$Belongings), function(i){
    rst2 <- rst
    vec <- rep(NA,times = ncell(rst))
    vec[missing] <- object$Belongings[,i]
    raster::values(rst2) <- vec
    return(rst2)
  })

  names(rasters) <- paste("group", 1:ncol(object$Belongings), sep = "")

  #step2 : adding the most likely group
  rst2 <- rst
  vec <- rep(NA,times = ncell(rst))
  DF <- as.data.frame(object$Belongings)
  vec[missing] <- max.col(DF, ties.method = "first")
  raster::values(rst2) <- vec
  rasters[["Groups"]] <- rst2

  object$isRaster <- TRUE
  object$rasters <- rasters
  object$missing <- missing
  return(object)
}
