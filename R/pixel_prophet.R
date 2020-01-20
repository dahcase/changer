#' A function to run the prophet algorithm by pixel
#' @param x numeric vector
#' @param dates vector of dates, with equal length to x
#' @param ... arguments passed to `prophet`
#'
#' @export
#'
pixel_prophet <- function(x, dates, ...){

  df = data.frame(ds = dates, y = x)

  mod = prophet(df, ...)

  browser()
}
