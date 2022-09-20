lm <- function(formula,data)
{
  lmlist <- linreg(formula, data)
  # lmlist <- list(
  # ehat = templist$residuale,
  # Beta = templist$betahat,
  # Q = templist$Q,
  # R = templist$R,
  # QR = templist$QR,
  # y_fitted = templist$fitted_Y,
  # varB = templist$variance_Beta,
  # coeff = templist$coeff
  # )
  # lmlist <- list(residual = ehat,Beta,Q,R,QR,y_fitted,varB)
  
  
  # print <- function(obj){
  #   UseMethod("print")
  # }
  
  
  #lmlist <- append(lmlist,print.lm1)
  class(lmlist) <- "lm1"
  return(lmlist)
}


