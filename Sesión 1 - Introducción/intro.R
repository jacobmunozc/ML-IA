# Sesión 1: Conceptos introductorios a Machine Learning
# Curso ML-IA
# Prof. Jacob Muñoz Castro
# Universidad de los Andes


# RStudio ---------------------------------------------------------------------------------------------------------

# Esta es una linea de código
print(2)

# Esta es otra línea de código
2 + 2

# Se oprime Cmd + Enter (Mac) o Ctrl + Enter (Win) para ejecutar una línea!

# Los paquetes están en la pestaña 'Packages' en el panel de la derecha

# Para cargarlos, se usar la función 'library' o 'require'
library(data.table)
require(readxl)

# Podemos asignar valores a objetos, que se muestran en el panel 'Environment'
x <-  runif(n = 1000,min = 0,max = 3)
y <- x**2 + rnorm(n = 1000,mean = 0,sd = 1)

# Como nuestros modelos no son determinísticos, es recomendable usar semillas 
set.seed(8080)
x <-  runif(n = 1000,min = 0,max = 3)
y <- x**2 + rnorm(n = 1000,mean = 0,sd = 1)
  

# Medidas de dispersión y tendencia central -----------------------------------------------------------------------

mean(x)
sd(x)
median(x)

# ¿Qué pasa si no hay una entrada (missing)?
x.na <- x
x.na[10] <- NA
mean(x.na)
mean(x.na,na.rm = TRUE)

# Guardamos las medias de ambas variables
x.mean <- mean(x)
y.mean <- mean(y)

# Distancia euclidiana y similitud coseno -------------------------------------------------

# La distancia euclidiana de ambos vectores
sqrt(sum(y**2))
sqrt(sum(x**2))

# La versión centrada
y.norm <- sqrt(sum((y - y.mean)**2))
x.norm <- sqrt(sum((x - x.mean)**2))

# La distancia del punto en la entrada 100 al punto de la entrada 22
sqrt((x[100] - x[20])**2 + (y[100] - y[20])**2)


# Correlación -----------------------------------------------------------------------------------------------------

# Un gráfico de dispersión a la mano (luego veremos mejores prácticas de visualización)
plot(x,y) 

# Aplicando la formulación trigonométrica de antes
((x - x.mean) %*% (y - y.mean))/(x.norm*y.norm) 

# Usando iota nos ahorra el calculo del vector de medias
X <- cbind(x,y)  #matrix(c(x,y),nrow = 1000,ncol = 2)
iota <- rep(1,1000)
X.c <- (diag(1000) - iota %*% solve(t(iota) %*% iota) %*% t(iota)) %*% X

(X.c[,1] %*% X.c[,2])/(sqrt(sum(X.c[,1]**2)) * sqrt(sum(X.c[,2]**2)))

# Con la función de correlación
cor(x,y)

# Pruebas de hipótesis --------------------------------------------------------------------------------------------

t.test(x)
t.test(y)
var.test(x,y)

# Estadísticas descriptiva ----------------------------------------------------------------------------------------

summary(X)
summary(data.frame(X))
summary(data.frame(V1 = x, V2 = y))

df <- data.frame(x = x, y = y)

hist(df$x,main = 'Histograma x', xlab = 'x')
hist(df$y,main = 'Histograma y', xlab = 'y')

install.packages('stargazer')


stargazer::stargazer(df)
stargazer::stargazer(df,type = 'html')
stargazer::stargazer(df,type = 'text',median = TRUE)

# Laboratorio: busqueda -------------------------------------------------------------------------------------------

# Creemos un algoritmo que nos ayude a buscar los puntos más lejanos de la muestra.



