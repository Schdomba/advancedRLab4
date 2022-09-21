#' Function to Plot Results
#'
#' @param x An object of class linreg
#' @param ... further arguments passed to or from other methods
#'
#' @return A list contains ggplots.
#' @export
#' @import ggplot2
#' @import stats
#' @examples
#' data(iris)
#' mod_object <- linreg(formula = Petal.Length ~ Species, data = iris)
#' plot(mod_object)


plot.linreg <- function(x,...){
values <- data.frame("Residuals" = x$ehat,"Fitted_Values" = round(x$y_fitted,5))

  colnames(values) <- c("Residuals","Fitted_Values")
  k<- unique(values$Fitted_Values)
  # return(k)
 medianvec <- c()
 for(i in k){
   medianvec <-  append(medianvec,median(as.vector(values$Residuals[which(
     values$Fitted_Values == i)])))
 }

 linedf <- data.frame(k,medianvec)

 plotlist <- list()
 plot1 <- (ggplot2::ggplot() +
 ggplot2::geom_point(data = values, ggplot2::aes(y = values$Residuals,x = values$Fitted_Values),shape = 1) +
 ggplot2::geom_line(data = linedf, ggplot2::aes(x = k,y = medianvec),color = "red") +
   ggplot2::xlab("Fitted Values") + ggplot2::ylab("Residuals") + ggplot2::ggtitle("Residuals vs Fitted") +
   ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) )


 # qwerty <- sqrt(abs(mod_obj$varB))
 values$Residuals <- sqrt(abs(values$Residuals/sd(values$Residuals)))

  medianvec1 <- c()
  for(i in k){
    medianvec1 <-  append(medianvec1,median(as.vector(values$Residuals[which(
      values$Fitted_Values == i)])))
  }

  linedf2 <- data.frame(k,medianvec1)

   plot2 <- ggplot2::ggplot() +
   ggplot2::geom_point(data = values, ggplot2::aes(x = values$Fitted_Values, y = values$Residuals),shape = 1) +
   ggplot2::geom_line(data = linedf2, ggplot2::aes(x = k,y = medianvec1),color = "red") +
     ggplot2::xlab("Fitted Values") +
     ggplot2::ylab(expression(sqrt(abs("Standardized Residuals")))) +
     ggplot2::ggtitle("Scale-Location") +
     ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5))

   plotlist <- list(plot1,plot2)
  return(plotlist)
}
