#' @title Function to return the coefficients
#'
#' @param obj 
#'
#' @return Returns a vector containing the calculated coefficient values
#' @export
#'
#' @examples
#' coef.lr(mod_object)
coef.lr <- function(obj){
  return(as.vector(obj$coeff))
}