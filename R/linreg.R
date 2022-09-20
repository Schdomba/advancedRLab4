linreg <- function(formula, data){
  A <- model.matrix(formula,data)
  y <- data[all.vars(formula)[1]]
  
  QR <- qr(A)
  Q <- qr.Q(QR)
  R <- qr.R(QR)
  
  Q %*% A[y]
  
  beta <- solve(R) %*% qr.qty(QR,as.matrix(y))
  
}