#' @title Custom Print Function
#' @description This is a custom print function that prints the coefficients
#' @param x An list object containing all the relevant information calculated by the function linreg or lr
#' @param ... further arguments passed to or from other methods.
#' @return Returns coefficients
#' @export
#'
#' @examples
#' data(iris)
#' mod_object <- linreg(formula = Petal.Length ~ Species, data = iris)
#' print(mod_object)

print.linreg <- function(x,...){

  cat(paste("linreg(formula = ", format(x$formula), ", data = ", x$dfname , ")\n\n ", sep = ""))

  print(x$Beta)

}
