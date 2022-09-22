#' Function to print Summary of Results
#'
#' @param object A object of class linreg
#' @param ... further arguments passed to or from other methods.
#'
#' @return Returns a data frame consisting of results.
#'
#' @export
#'
#' @examples
#' data(iris)
#' mod_object <- linreg(formula = Petal.Length ~ Species, data = iris)
#' summary(mod_object)

summary.linreg <- function(object,...)
{
  df1 <- data.frame(Coefficients = object$Beta,"Standard Error" = object$stde,
                    "t-value" = object$tval,"p-val" = object$pval,"sigma" = object$sigma1,"DoF" = object$dof)
  stars <- vector(length = length(object$pval))
  for (i in 1:length(object$pval)) {
    if(object$pval[i] > 0.1)
    {
      stars[i] <- c(" ")
    }
    else if(object$pval[i] > 0.05){
      stars[i] <- c(".")
    }
    else if(object$pval[i] > 0.01){
      stars[i] <- c("*")
    }
    else if(object$pval[i] > 0.001){
      stars[i] <- c("**")
      }
    else{
      stars[i] <- c("***")
    }

  }

  df1[' '] <- stars
  print(df1)
  cat("Residual standard error:", object$sigma, "on" , object$dof, "degrees of freedom")
}
