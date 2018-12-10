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
  if (nCore < parallelCores) {message(paste0('[!] For safe analysis, we stablish only ', floor(nCore * .5), ' core(s) to do the analysis.'))}
  ifelse(nCore < parallelCores, return(floor(nCore * .5)), return(parallelCores))
}