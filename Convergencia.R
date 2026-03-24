library(betareg)
set.seed(2026)
R <- 100
n_vals <- c(10)
beta0 <- 0.8
beta1 <- -0.03
phi <- 50
Resultados_beta0 <- list()
Resultados_beta1 <- list()
Resultados_phi <- list()
for (n in n_vals) {
  Est_beta0 <- matrix(NA, nrow = n-2, ncol = R) 
  Est_beta1 <- matrix(NA, nrow = n-2, ncol = R)  
  Est_phi <- matrix(NA, nrow = n-2, ncol = R)  
  for (i in 1:R) {
    x <- runif(n, 4, 250)
    eta <- beta0 + beta1 * x
    mu  <- exp(eta)/(1+exp(eta))
    y <- rbeta(n, mu*phi, (1-mu)*phi)
    for (j in 3:n) {
      y_sub <- y[1:j]
      x_sub <- x[1:j]
      n_sub <- length(y_sub)
      y_sub <- (y_sub*(n_sub-1)+0.5)/n_sub
      modelo <- betareg(y_sub ~ x_sub)
      Est_beta0[j-2, i] <- coef(modelo)[1]
      Est_beta1[j-2, i] <- coef(modelo)[2]
      Est_phi[j-2, i] <- coef(modelo)[3]
    }
  }
  Resultados_beta0[[as.character(n)]] <- rowMeans(Est_beta0)
  Resultados_beta1[[as.character(n)]] <- rowMeans(Est_beta1)
  Resultados_phi[[as.character(n)]] <- rowMeans(Est_phi)
}
max_n <- max(n_vals)
colores <- c("black", "blue", "red")
plot(3:max_n, rep(NA, max_n-2), type="n",
     ylim = range(unlist(Resultados_beta0), beta0),
     main="Convergencia de beta0")
for (k in 1:length(n_vals)) {
  lines(3:n_vals[k], Resultados_beta0[[k]], col=colores[k], type="b")
}
abline(h = beta0, col="red", lwd=2)
plot(3:max_n, rep(NA, max_n-2), type="n",
     ylim = range(unlist(Resultados_beta1), beta1),
     main="Convergencia de beta1")
for (k in 1:length(n_vals)) {
  lines(3:n_vals[k], Resultados_beta1[[k]], col=colores[k], type="b")
}
abline(h = beta1, col="red", lwd=2)
plot(3:max_n, rep(NA, max_n-2), type="n",
     ylim = range(unlist(Resultados_phi), phi),
     main="Convergencia de phi")
for (k in 1:length(n_vals)) {
  lines(3:n_vals[k], Resultados_phi[[k]], col=colores[k], type="b")
}
abline(h = phi, col="red", lwd=2)


