irfvar <- function(A, SIGMA, p, h) {
  J <- cbind(diag(3), matrix(0, nrow = 3, ncol = 3 * (p - 1)))
  IRF <- matrix(NA, nrow = 3^2, ncol = h + 1)
  
  IRF[, 1] <- as.vector(t(J %*% A^0 %*% t(J) %*% chol(SIGMA)))
  
  for (i in 1:h) {
    IRF[, i + 1] <- as.vector(t(J %*% A^i %*% t(J) %*% chol(SIGMA)))
  }
  
  return(IRF)
}
