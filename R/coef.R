#' @title Function to return the coefficients
#'
#' @param object An object of class linreg
#' @param ... Other arguments
#'
#' @return Returns a vector containing the calculated coefficient values
#' @export
#'
#' @examples
#' data(iris)
#' mod_object <- linreg(formula = Petal.Length ~ Species, data = iris)
#' coef(mod_object)


coef.linreg <- function(object,...){
  x <- as.vector(object$Beta)
  names(x)<- names(object$Beta)
  return(x)
}
