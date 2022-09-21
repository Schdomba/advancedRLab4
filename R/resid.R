#' Function to return residual values
#'
#' @param object An object of class linreg
#' @param ... other arguments
#'
#' @return Returns residual values
#' @export
#'
#' @examples
#' data(iris)
#' mod_object <- linreg(formula = Petal.Length ~ Species, data = iris)
#' resid(mod_object)

residuals.linreg <- function(object,...)
{
  return(object$ehat)
}
