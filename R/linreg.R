#' Function that creates an S3 Class of class linreg
#'
#' @param formula a formula object
#' @param data A data frame
#'
#' @return A class of type linreg with specific attributes and functions
#' @export
#'
#' @examples
#' data(iris)
#' linreg(formula = Petal.Length ~ Species, data = iris)
#'
#'

linreg <- function(formula, data)
{

  A <- model.matrix(formula,data)
  dependent <- data[all.vars(formula)[1]]


  QR <- qr(A)

  Q <- qr.Q(QR)
  R <- qr.R(QR)

  y <- dependent
  b <- qr.coef(QR,y)
  y1 <- qr.fitted(QR,as.matrix(y), k = QR$rank)
  e <- qr.resid(QR,as.matrix(y))



  # sigma2 means sigma^2
  sigma2 <- sum(e^2)/ (nrow(Q) - ncol(Q))

  sigma1 <- sqrt(sigma2)
  varB <- diag(chol2inv(R) * sigma2)
  tval <- b/ sqrt(varB)
  dof <- (nrow(Q) - ncol(Q))
  pval <- 2*pt(abs(tval), dof, lower.tail = FALSE)

  stde <- sqrt(varB)
  stored_calc <- list(QR = QR,Q = Q,R = R,varB = varB,ehat = e,y_fitted = y1,
                      Beta = b,dof = dof,tval = tval,pval = pval,
                      sigma1 = sigma1,stde = stde,formula = formula,
                      dfname = deparse(substitute(data)))
  class(stored_calc) <- "linreg"

  return(stored_calc)
}



