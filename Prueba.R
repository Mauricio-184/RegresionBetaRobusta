install.packages("statmod")
install.packages("numDeriv")
library(numDeriv)
library(statmod)
library(betareg)
library(ggplot2)
set.seed(2026)
n <- 20
epsilon <- n*0.10
x <- runif(n, 4, 250)
beta0 <- 0.8
beta1 <- -0.03
eta <- beta0 + beta1 * x
mu  <- exp(eta) / (1 + exp(eta))   
phi <- 50                         
y <- rbeta(length(x), mu * phi, (1 - mu) * phi)
outliers_idx <- sample(1:n, epsilon)
y_outliers <- y
#y_outliers[outliers_idx[1:5]] <- 0.0001   
y_outliers[outliers_idx[1:epsilon]] <- 0.99  
x_outliers <- outliers_idx[1:epsilon]
y_out <- y_outliers[outliers_idx[1:epsilon]]
outliers <- data.frame(x_outliers,y_out)
data <- data.frame(x = x, y = y)
Modelo <- betareg(y ~ x, data = data, link = "logit")
#outliers <- data.frame(
#  x = c(45, 50),
# y = c(0.85, 0.95)
#)

data_Completa <- data.frame(x,y = y_outliers)
Modelo_1 <- betareg(y ~ x, data = data_Completa, link = "logit")
x_grid <- seq(min(data$x), max(data$x), length.out = n)
data_Nueva <- data.frame(x = x_grid)
prediciones_Modelo <- predict(Modelo,newdata =data_Nueva ,type = "response")
prediciones_Modelo_1 <- predict(Modelo_1,newdata =data_Nueva,type = "response")
pred_limpio_df <- data.frame(
  x = x_grid,
  y = prediciones_Modelo
)
Y_P <- pred_limpio_df$y
pred_Completo_df <- data.frame(
  x = x_grid,
  y = prediciones_Modelo_1
)
Y_P_1 <- pred_Completo_df$y
ggplot(data_Completa, aes(x = x, y = y)) +
  geom_point(aes(color = "Datos limpios"))+
  geom_line(data = pred_limpio_df,
            aes(x = x, y = Y_P, color = "Ajuste limpio"),
            linewidth = 1) +
  geom_line(data =pred_Completo_df,
            aes(x = x, y = Y_P_1, color = "Ajuste Completo"),
            linewidth = 1) +
  geom_point(data = outliers,
             aes(x = x[x_outliers] , y =y_out , color = "Outliers"))+
  scale_color_manual(
    values = c(
      "Datos limpios"  = "black","Ajuste limpio" = "blue",
      "Ajuste Completo"="orange","Outliers"    = "red") 
  )+
  labs(
    title = "Datos contaminados con valores atípicos",
    y = "Proporciones",
    color = "Leyenda"
  )+
  theme(plot.title = element_text(hjust = 0.5))
coef(Modelo) 
coef(Modelo_1)
