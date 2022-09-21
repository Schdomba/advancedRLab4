#' Function to Return Predicted Values
#'
#' @param obj 
#'
#' @return Function returns a numeric vector of all the predicted values
#' @export
#'
#' @examples
#' pred.lr(mod_object)
pred.lr <- function(obj)
{
  return(obj$y_fitted)
}