pkg.env <- new.env()
pkg.env$apiKey = NULL

#' api
#' This function sets the api key
#'
#' @param key api key to set
#'
#' @return
#' @export
api <- function(key) {
  pkg.env$apiKey = key
}
