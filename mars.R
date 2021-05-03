library(rpart)
library(earth)

mars = function(formula ,data, control = NULL, ...) {
  if(is.null(control)) {
    control = mars.control()
  }
  cc = match.call()
  mf = model.frame(formula, data)
  y = model.response(mf)
  mt = attr(mf, "terms")
  x = model.matrix(mt, mf)
  fwd_out = fwd_stepwise(y, x, control)
  bwd_out = bwd_stepwise(fwd_out, control)
  fit = lm(y~.-1,data = data.frame(y = y,bwd_out$B))
  out = c(list(call = cc, formula = formula, y = y, B = bwd_out$B, splits = bwd_out$splits, x = x, control = control),fit)
  class(out) = c("mars", class(fit))
  return(out)
}

mars.control = function(Mmax = 2, d=3,trace=FALSE) {
  control = new_mars.control(Mmax, d, trace)
  validate_mars.control(control)
  return (control)
}

new_mars.control = function(Mmax, d, trace) {
  structure(list(Mmax = Mmax, d = d, trace = trace), class = 'mars.control')
}

validate_mars.control = function(control) {
  if(control$Mmax < 2) {
    warning("Input Mmax must be >= 2; setting to 2")
    Mmax = 2
  }
}

fwd_stepwise = function(y, x, control) {
  # Initialize:
  N = length(y) # Sample size
  n = ncol(x) # Number of predictors
  B = init_B(N)
  splits = list(data.frame(
    m = 0,
    v = 0,
    s = NA,
    t = NA
  ))
  # Forward selection:
  M = 1
  while (M <= control$Mmax) {
    lof_best = Inf
    for (m in 1:M) {
      # Choose basis function to split
      # Filter out X[,v] already in Bm.
      remove = splits[[m]]$v
      Xv = c(1:n)
      Xv = Xv[!Xv %in% remove]
      for (v in Xv) {
        # Select variable to split on
        tt = split_points(x[, v], B[, m])
        for (t in tt) {
          Bnew =
            data.frame(B[, (1:M)[-m]], Btem1 =
                         B[, m] * h(+1, x[, v], t), Btem2 = B[, m] * h(-1, x[, v], t))
          gdat = data.frame(y = y, Bnew)
          lof = LOF(y ~ ., gdat, control)
          if (lof < lof_best) {
            lof_best = lof
            split_best = c(m = m,
                           v = v,
                           t = t)
          } # end if
        } # end loop over splits
      } # end loop over variables
    } # end loop over basis functions to split
    m = split_best[['m']]
    v = split_best[['v']]
    t = split_best[['t']]
    B = cbind(B, B[, m] * h(+1, x[, v], t))
    B = cbind(B, B[, m] * h(-1, x[, v], t))
    
    right = rbind(splits[[m]], c(m, v, 1, t))
    left = rbind(splits[[m]], c(m, v, -1, t))
    splits = c(splits, list(left), list(right))
    
    M = M + 2
  } # end loop over M
  colnames(B) = paste0('B', seq(0, control$Mmax))
  return(list(y = y, B = B, splits = splits))
}

bwd_stepwise = function(fwd_out, control) {
  y = fwd_out$y
  Jstar = fwd_out$B
  splits = fwd_out$splits
  
  Ksplits = splits
  Kstar = Jstar
  gdat = data.frame(y = y, Jstar)
  
  lof_best = LOF(y ~ ., gdat, control)
  Mmax = ncol(Jstar)
  
  for (M in Mmax:2) {
    L = Kstar
    S = Ksplits
    b = Inf
    for (m in 2:M) {
      K = L[, -m]
      gdat_2 = data.frame(y = y, K)
      lof = LOF(y ~ ., gdat_2, control)
      if (lof < b) {
        b = lof
        Kstar = K
        Ksplits = S[-m]
      }
      if (lof < lof_best) {
        lof_best = lof
        Jstar = K
        splits = S[-m]
      }
    }
  }
  return(list(y = y, B = Jstar, splits = splits))
}

# Supporting functions:

h = function(s, x, t) {
  return(pmax(0, s * (x - t)))
}

init_B = function(N) {
  B = matrix(1, nrow = N, ncol = 1)
  return(B)
}

split_points = function(Xv, Bm) {
  x = Xv[Bm > 0]
  out = sort(unique(x))
  return(out[-length(out)])
}

split_points = function(xv,Bm) {
  out = sort(unique(xv[Bm>0]))
  return(out[-length(out)])
}

LOF = function(form, data, control) {
  ff = lm(form, data)
  RSS = sum(residuals(ff) ^ 2)
  M = length(ff$coefficients) - 1
  N = nrow(data)
  C_M = sum(hatvalues(ff))
  return (RSS * N / (N - (C_M + control$d * M)) ^ 2)
}