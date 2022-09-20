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
  
  # for sigma^2 , from stackoverflow 
  sigma2 <- sum(e^2)/ (nrow(Q) - ncol(Q))
  #print(sigma2)
  
  varB <- chol2inv(R) * sigma2
  #print(varB)
  
  stored_calc <- list(QR = QR,Q = Q,R = R,varB = varB,ehat = e,y_fitted = y1, Beta = b,coeff = coeff)
  
  
  return(stored_calc)
}


#setClass("linreg", slots = formula,data)
class(linreg) <- "linear_model"

linear_model.print <- function(linreg){
  cat("Coefficients", linreg.linear_model$R)
}


