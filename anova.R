anova.mars = function(obj){
  X = obj$B
  y = obj$y

  for (i in 1:ncol(obj$B)){
    if(is.na(colnames(obj$x)[obj$splits[[i]][2][2,1]])){
      cat("intercept \n")
    }
    else {
      cat(paste0(colnames(obj$x)[obj$splits[[i]][2][2,1]],"(",colnames(obj$B)[i],")", ":", "\n"))
    }
    cat(paste0("* Variance: ",var((X[,i]*obj$coefficients[i]))))
    cat(paste0("\n* residual: ", var((X[,i]* obj$residuals)), "\n"))
    cat(paste0("* fitted value: ", var((X[,i]*obj$fitted.values)), "\n"))
    cat("\n")
  }
}