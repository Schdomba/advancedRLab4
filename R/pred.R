#' Function to Return Predicted Values
#'
#' @param obj An object of class linreg
#'
#' @return Function returns a numeric vector of all the predicted values
#' @export
#'
#' @examples
#' data(iris)
#' mod_object <- linreg(formula = Petal.Length ~ Species, data = iris)
#' pred(mod_object)


pred.linreg<- function(obj)
{

  return(obj$y_fitted)
}
