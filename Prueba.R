set.seed(123)
# Datos base
n <- 100
x <- seq(-6, 6, length.out = n)
p <- 1 / (1 + exp(-1 * (x - 0)))
Y <- rbinom(n, size = 1, prob = p)

# Agregar outliers
x_out <- 7; Y_out <- 0
x_out1 <- 10; Y_out1 <- 0

x2 <- c(x, x_out, x_out1)
Y2 <- c(Y, Y_out, Y_out1)
# Ajustar modelo logístico
modelo_out <- glm(Y ~ x, family = binomial(link = "logit"))
# Ajustar modelo logístico con outliers
modelo_out1 <- glm(Y2 ~ x2, family = binomial(link = "logit"))

# Predicciones
pred_out <- predict(modelo_out, newdata = data.frame(x = sort(x)), type = "response")
pred_out1 <- predict(modelo_out1, newdata = data.frame(x2 = sort(x2)), type = "response")

# Graficar
plot(x2, Y2, pch = 19, col = rgb(0, 0, 1, 0.5),
     xlab = "x", ylab = "Probabilidad / Datos",
     main = "Curva logística afectada por un outlier",
     ylim = c(-0.1, 1.1))

# Curva verdadera (verde)
lines(x, p, col = "green", lwd = 2, lty = 2)
# Curva ajustada  (orange)
lines(sort(x), pred_out, col = "orange", lwd = 2)
# Curva ajustada con el outlier (roja)
lines(sort(x2), pred_out1, col = "red", lwd = 2)

# Señalar los outliers
points(x_out, Y_out, pch = 19, col = "red", cex = 2)
points(x_out1, Y_out1, pch = 19, col = "red", cex = 2)

# Leyenda fuera del gráfico

xrange <- range(x2)
yrange <- range(Y)

legend(x = xrange[2] - diff(xrange)*0.4,   # 40% antes del borde derecho
       y = yrange[2] - diff(yrange)*0.1,   # 10% bajo el borde superior
       legend = c("Datos normales", "Curva verdadera", "Ajuste No afectado","Ajuste afectado", "Outlier"),
       col = c(rgb(0, 0, 1, 0.5), "green", "orange","red", "red"),
       pch = c(19, NA, NA, NA, 19),
       lty = c(NA, 1, 1, 1, NA),
       lwd = c(NA, 1, 1, 1, NA),
       cex = 0.8,
       bty = "n")



