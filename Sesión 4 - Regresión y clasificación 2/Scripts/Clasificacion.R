

# kNN -------------------------------------------------------------------------------------------------------------

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

data.frame(fgl$type[test],N1,N5)





# Logit -----------------------------------------------------------------------------------------------------------

require(gamlr)  # ML
 
# Datos
credito <- readRDS('/Users/jacobmunozc/Dropbox/Curso Machine Learninng SFC/Consolidado/ML-IA/Sesión 4 - Regresión y clasificación 2/Data/credit_class.rds')

dim(credito)

head(credito)

logit1 <- glm(Default ~ duration + amount + installment + age + 
                factor(history) + factor(purpose) + factor(foreign) + 
                factor(rent), 
              data = credito, family = 'binomial')

summary(logit1)

str(credito$foreign)

require(Matrix)
cred.matrix <- sparse.model.matrix(Default ~ .**2, data = credito)[,-1]

default <- credito$Default
cred.score <- cv.gamlr(cred.matrix, default, family = 'binomial')

plot(cred.score)

predicted <- predict(cred.score$gamlr,cred.matrix,type = 'response')
predicted <- drop(predicted) # quitarle el formato de sparse matrix

boxplot(predicted ~ default, xlab = 'default', ylab = 'Prob. default')



# LDA -------------------------------------------------------------------------------------------------------------

set.seed(111000)
muestra <- sample(1:nrow(credito),nrow(credito)*.7)
train <- credito[muestra,]
test <- credito[-muestra,]

require(MASS) # LDA ML
LDA <- lda(Default ~ duration, data = train)
lda.pred <- predict(LDA, test)

