predict.mars = function(object, newdata){
  if (missing(newdata) || is.null(newdata)) {
    X = object$B
  }
  else {
    term = terms(object$formula)
    term = delete.response(term)
    modelf = model.frame(term, newdata)
    modelt = attr(modelf, "terms")
    X = model.matrix(modelt, modelf)
    X = splitX(X, object$splits)
  }
  beta = object$coefficients
  drop(X %*% beta)
}

splitX = function(X, splits) {
  Xout = matrix(0, nrow = nrow(X), ncol = length(splits))
  for (i in 1:length(splits)) {
    Xout[,i] = splX(X, splits[[i]])
  }
  Xout
}

splX = function(X, s) {
  Xout = rep(1, nrow(X))
  if (nrow(s) > 1) {
    for (i in 2 : nrow(s)){
      Xout = Xout * H(X[,s[i,"v"]])
    }
  }
  return(Xout)
}