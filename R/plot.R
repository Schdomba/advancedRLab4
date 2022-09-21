plot.lr <- function(obj){
  values <- data.frame(Residuals = obj$ehat,Fitted_Values = round(obj$y_fitted,5))
  
  colnames(values) <- c("Residuals","Fitted_Values")
  k<- unique(values$Fitted_Values)
  # return(k)
 medianvec <- c()
 for(i in k){
   medianvec <-  append(medianvec,median(as.vector(values$Residuals[which(values$Fitted_Values == i)])))
 }
 
 linedf <- data.frame(k,medianvec)
 #print(linedf)
 #print(medianvec)
 plotlist <- list()
 # (ggplot() + 
 # geom_point(data = values, aes(y = Residuals,x = Fitted_Values),shape = 1) +
 # geom_line(data = linedf, aes(x = k,y = medianvec),color = "red") +
 # xlab("Fitted Values") + ylab("Residuals"))
 
 
 qwerty <- sqrt(abs(diag(mod_object$varB)))
 linedf2 <- data.frame(k,qwerty)
 # print(linedf2)
 colnames(linedf2) <- c("Residuals","Fitted_Values")
 # change aes values from median vec to qwerty df, also name the columns of qwerty
 
 (ggplot() + 
   geom_point(data = values, aes(x = Fitted_Values, y = linedf2$k),shape = 1) +
   geom_line(data = linedf2, aes(x = k,y = qwerty),color = "red") +
   xlab("Fitted Values") +
   ylab(expression(sqrt(abs("Standardized Residuals")))) + ylim(0,2))
 
 #return(plotlist)
}
