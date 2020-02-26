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

  #x_arr <- array(x, dim = dim(x))

  if(method == 'prophet'){

    dots = list(...)

    #Prophet specifies 25 potential changepoints which are uniformly placed in the first 80% of the time series.
    #https://facebook.github.io/prophet/docs/trend_changepoints.html
    #https://github.com/facebook/prophet/blob/master/R/R/prophet.R#L478
    #catch and replace arguments in dots to fix change points
    if(any(names(dots) %in% 'changepoints')){
      chgpts = dots$changepoints
    }else{
      n.changepoints = ifelse(is.null(dots$n.changepoints), 25, dots$n.changepoints)
      changepoint.range = ifelse(is.null(dots$changepoint.range), .8, dots$changepoint.range)

      hist.size <- floor(dim(x)[3] * changepoint.range)
      if (n.changepoints + 1 > hist.size) {
        n.changepoints <- hist.size - 1
        message('n.changepoints greater than number of observations. Using ',
                n.changepoints)
      }
      if (n.changepoints > 0) {
        cp.indexes <- round(seq.int(1, hist.size,
                                    length.out = (n.changepoints + 1))[-1])
        changepoints <- dates[cp.indexes]
      } else {
        changepoints <- c()
      }
      chgpts = changepoints
    }

    dots$changepoints = chgpts

    anom = function(v){
      pixel_prophet(v, dates, dots)
    }

    ret <- calc(x, fun = anom)

    #x_arr <- future.apply::future_apply(x_arr, 1:2, pixel_prophet, dates = dates, dots = dots)
    #x_arr <- aperm(x_arr, c(2,3,1))
    #x[] <- x_arr[]
    names(ret) <- chgpts
  }

  return(ret)

}
