#' Detect number of cores
#'
#' @name detectCores
#' @rdname detectCores
#' @keywords internal
#' @export
#' @importFrom parallel detectCores
#' @usage detectCores(all.tests = FALSE, logical = TRUE)
NULL

validate.parallelCores <-  function(parallelCores){
  nCore <- detectCores()
  if (nCore < parallelCores) {
    message(paste0('[!] There not more than ',  nCore, 'availabe, for safe analysis, we stablish ', nCore - 1, 'to do the analysis'))
    return(nCore - 1)
  } else {
    return(parallelCores)
  }
}