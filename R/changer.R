#' Identify change points in spatiotemporal cubes
#'
#' @param x A spatiotemporal cube with a class of `stars` (and in the future, `raster`). Assumes dimensions are (x,y,time)
#' @param ... arguments tbd.
#'
#' @export
changer <- function(x, ...){
  UseMethod('changer', x)
}

#' Changepoint detection for stars objects
#' @param x stars. Spatiotemporal cube.
#' @param method character. One of prophet, ...
#' @param FUTURE logical. Controls whether the calculation should be done in parallel via the future package.
#' @param ... arguments passed to the underlying method.
changer.stars <- function(x, method = 'prophet', FUTURE = FALSE, ...){

  #confirm dimensions
  method <- match.arg(method, c('prophet'))

  if(method == 'prophet'){

    stars::st_apply(x, 1:2, pixel_prophet, ..., FUTURE = FUTURE)

  }

}

#' Changepoint detection for stars objects
#' @param x raster. Spatiotemporal cube in the form of a raster stack or raster brick.
#' @param dates date (or similar). Vector of dates, 1 per layer in x.
#' @param method character. One of prophet, ...
#' @param FUTURE logical. Controls whether the calculation should be done in parallel via the future package.
#' @param ... arguments passed to the underlying method.
#'
#' @details Warning. raster converts to an array internally and therefore involves a copy.
#'
#' @export
changer.RasterBrick <- function(x, dates, method = 'prophet', FUTURE = FALSE, ...){

  #confirm dimensions
  method <- match.arg(method, c('prophet'))

  stopifnot(inherits(dates, 'Date'))

  x <- array(x, dim = dim(x))

  if(method == 'prophet'){
    ret <- future.apply::future_apply(x, 1:2, pixel_prophet, dates = dates, ...)
  }

}
