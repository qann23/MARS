print.mars = function(obj) {
  
  cat("FUNCTION CALL: \n")
  cat("\t")
  print(obj$call)
  
  cat("RESULT: \n")
  cat("\t")
  cat("Coefficients:", "\n")
  for (i in 1:ncol(obj$B)){
    cat("\t")
    cat(paste0(coefficients(obj)[i], "\t"))
    cat(colnames(obj$B)[i],"\n")
  }
}
