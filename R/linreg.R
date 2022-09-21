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
  # data is a data frame that will be returned by model.frame which is
  #implicitly called when the data is another sort of object
  # maybe look at implementing that explicitly?
  A <- model.matrix(formula,data)
  dependent <- data[all.vars(formula)[1]]
  #print(dependent)

  QR <- qr(A)
  #print(QR)
  Q <- qr.Q(QR)
  #typeof(Q)
  #print(Q)
  R <- qr.R(QR)
  #print(R)
  y <- dependent
  # str(y) #print(y)
  #print(as.matrix(y))
  # b <- backsolve(R, crossprod(Q,y))
  b <- solve.qr(QR,y)
  #print(b)
  # VB <-var(b) doesn't work, generates a 0x0 matrix
  # yfitted <- tcrossprod(Q)%*% (as.matrix(y)) works but just using inbuilt qr
  # function as 1st prefference, see : line 26
  y1 <- qr.fitted(QR,as.matrix(y), k = QR$rank)
  #print(y1==yfitted) elementwise comparision, all FALSE (could be becuase of
  # approx diff in decimal values?)
  # print(yfitted)
  e <- qr.resid(QR,as.matrix(y))
  #print(e)

  coeff <- qr.coef(QR,y)

  # sigma2 means sigma^2
  sigma2 <- sum(e^2)/ (nrow(Q) - ncol(Q))

  sigma1 <- sqrt(sigma2)
  varB <- diag(chol2inv(R) * sigma2)
  tval <- b/ sqrt(varB)
  dof <- (nrow(Q) - ncol(Q))
  pval <- 2*pt(abs(tval), dof, lower.tail = FALSE)

  stde <- sqrt(varB)
  stored_calc <- list(QR = QR,Q = Q,R = R,varB = varB,ehat = e,y_fitted = y1,
                      Beta = b,coeff = coeff,dof = dof,tval = tval,pval = pval,
                      sigma1 = sigma1,stde = stde,formula = formula,
                      dfname = deparse(substitute(data)))
  class(stored_calc) <- "linreg"

  return(stored_calc)
}



