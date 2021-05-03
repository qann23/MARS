plot.mars = function(obj) {
  # Fitted vs Residuals
  residual = obj$residuals
  fit = obj$fitted.values
  test = lowess(residual ~ fit)
  plot(fit, residual, main = "Residuals vs Fitted", xlab = "Fitted Values", ylab = "Residuals")
  abline(h = 0, lty = 2, col = "green")
  lines(test, col = "red")
  # Residual QQ plot
  qqnorm(obj$residuals, pch = 16, main = "Residual QQ",
         xlab = "Normal Quantiles", 
         ylab = "Residual Quantiles")
  qqline(obj$residuals, col = "red")
}