#' Function that creates an S3 Class of class lm1
#'
#' @param formula passes the dependent variable and independant variable
#' @param data A data frame 
#'
#' @return A class of type lm1 with specific attributes and functions(?)
#' @export
#'
#' @examples
#' lm(formula = Petal.Length ~ Species, data = iris)

lm <- function(formula,data)
{
  lmlist <- linreg(formula, data)
 
  class(lmlist) <- "lm1"
  return(lmlist)
}


