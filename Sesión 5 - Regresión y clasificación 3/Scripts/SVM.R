# Sesión 5: Clasificación
# Support Vector Machines

# Cargar la librería
library(e1071)

# Generemos datos simulados
set.seed(1011)
x <- matrix(rnorm(20*2), ncol = 2)
y <- rep(c(-1,1), each = 10)

x[y == 1,] <- x[y == 1,] + 1


# Grafiquemos
plot(x, col = factor(y), pch = 19)

# Ajustemos el SVC
data <- data.frame(x,y = as.factor(y))

svm.fit <- svm(y ~ ., data, kernel = 'linear', cost = 10, scale = FALSE) 
# El C del que hablamos lo fijamos en 10. Además no estamos estandarizando variables


plot(svm.fit,data)

# El límite de decisión es un zigzag 
# Recordamos que los vectores de soporte son los puntos cercanos al límite o en el lado erróneo del límite
# Los vectores de soporte son graficados como cruces
# Las demás observaciones son círculos

summary(svm.fit)


# Ahora mejoremos la visualización

make.grid <- function(x, n = 100) {
  grange = apply(x, 2, range)
  x1 <- seq(from = grange[1,1], to = grange[2,1], length = n)
  x2 <- seq(from = grange[1,2], to = grange[2,2], length = n)
  expand.grid(X1 = x1, X2 = x2)
}

xgrid <- make.grid(x)
xgrid[1:10,]

y.grid <- predict(svm.fit, xgrid)

plot(xgrid, col = c('orange','green')[as.numeric(y.grid)], pch = 20, cex = .2)



plot(xgrid, col = c('orange','green')[as.numeric(y.grid)], pch = 20, cex = .2)
points(x, col = factor(y), pch = 19)
points(x[svm.fit$index,], pch = 5, cex = 2)



# Ahora visualicemos los márgenes

beta <- drop(t(svm.fit$coefs) %*% x[svm.fit$index,])
beta0 <- svm.fit$rho

plot(xgrid, col = c('orange','green')[as.numeric(y.grid)], pch = 20, cex = .2)
points(x, col = factor(y), pch = 19)
points(x[svm.fit$index,], pch = 5, cex = 2)
abline((beta0 - 1)/beta[2], -beta[1]/beta[2], lty = 2, col = 'steelblue', lwd = 2)
abline((beta0 + 1)/beta[2], -beta[1]/beta[2], lty = 2, col = 'steelblue', lwd = 2)



# Validación cruzada

# El paquete e1071 tiene la función tune() para realizar la validación crosstune()
# Pasamos por varios valores del parámetro de costo

set.seed(1012)
tunes <- tune(svm, y ~ ., data = data, kernel = 'linear', ranges = list(cost = c(.001, .01, 1, 5, 10, 100)))

summary(tunes)

best.model <-  tunes$best.model
summary(best.model)



# Desempeño del model

set.seed(123)
x <- matrix(rnorm(20*2), ncol = 2)
y <- rep(c(-1,1), each = 10)

x[y == 1,] <- x[y == 1,] + 1


test.data <- data.frame(x, y = as.factor(y))

y.predict <- predict(best.model, test.data)

# Casos de error de clasificación
table(predichos = y.predict, verdaderos = test.data$y)

