calc_QR <- function(A){
  # save row number to variable
  rows <- nrow(A)
  
  # create an empty list for Q_i's
  Q_list <- list()
  # save the starting matrix
  A_start <- A
  
  # initialize counter variable
  i = 0
  # loop terminates when a 1x1 matrix is left
  while(nrow(A) > 1 && ncol(A) > 1){
    # calculate alpha
    alpha <- sign(A[1,1]) * sqrt(sum(A[,1]^2))
    # calculate v_i
    v <- as.matrix(A[,1]+alpha*c(1,rep(0,rows-1-i)))
    # temporary interim result
    temp <- (2*v%*%t(v))/as.vector(t(v)%*%v)
    # use temporary result to calculate Q_i
    Q <- diag(rows-i) - temp
    # pad Q_i, if Q_i's dimensions are smaller than original matrix
    if(i > 0){
      padded_Q <- diag(rows)
      padded_Q[-i,-i] <- Q
      Q <- padded_Q
    }
    # add Q_i to the list
    Q_list[[i+1]] <- Q
    # set temporary variable to Q
    temp <- Q
    # use temporary variable and reversed list of Q_i's to multiply all Q_i's
    for(elem in rev(Q_list)[-1]){
      temp <- temp %*% elem
    }
    # calculate R with the help of temp and A_start. Round to 5 digits,
    # because otherwise 0 won't be exactly 0
    R <- round(temp %*% A_start, digits = 10)
    # Assign a subset of R to A
    A <- as.matrix(R[-1:-(i+1), -1:-(i+1)])
    # increment counter
    i <- i + 1
  }
  
  # calculate Q by multiplying all Q_i's
  Q <- t(Q_list[[1]])
  for(elem in Q_list[-1]){
    Q <- Q%*%t(elem)
  }
  # return a list with Q and R
  return_list <- list(Q,R)
  return(return_list)
}
