
require(class) # para kNN
require(MASS) # Para los datos

data(fgl)

str(fgl)

# Ejercicio 1: graficar distribuciones de cada tipo para cada variable 



# PSA: distancias...matriz de datos...las unidades importan

# La columna 10 es la que tiene nuestra variable de clase, entonces:
x <- scale(fgl[,1:9]) 

apply(x, 2, sd) # aplicación de la familia apply

set.seed(111000)

test <- sample(1:214,10)
N1 <- knn(train = x[-test,], test = x[test,], cl = fgl$type[-test], k = 1)
N5 <- knn(train = x[-test,], test = x[test,], cl = fgl$type[-test], k = 5)



