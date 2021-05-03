summary.mars = function(obj) {
  cat("FUNCTION CALL: \n")
  cat("* ")
  print(obj$call)
  cat("\n-------------------------------------------------------------------------\n\n")
  
  cat("RESIDUALS: \n",summary(summary.lm(obj)$residuals))
  cat("\n\n-------------------------------------------------------------------------\n")
  
  cat("\nBASIS FUNCTION: \n")
  for (i in 1:ncol(obj$B)){
    if(is.na(colnames(obj$x)[obj$splits[[i]][2][2,1]])){
      cat("intercept \n")
    }
    else {
      cat(paste0(colnames(obj$x)[obj$splits[[i]][2][2,1]],"(",colnames(obj$B)[i],")", ":", "\n"))
      cat(paste0("* Sign: ", -1*obj$splits[[i]][3][2,1]))
      cat(paste0("\n* Split at: ", obj$splits[[i]][4][2,1]), "\n")
    }
    cat(paste0("* Estimate coefficients: ", coefficients(obj)[i]), "\n\n")
  }
  cat("-------------------------------------------------------------------------\n\n")
  
  cat("Multiple R-squared:",summary.lm(obj)$r.squared,
      "\t Adjusted R-squared:",summary.lm(obj)$adj.r.squared,"\n")
}