# Sesión 5: Clasificación

# Árboles de decisión ---------------------------------------------------------------------------------------------

# Iris data

## load the library
library(rpart)

set.seed(123)
data(iris)

train.id <- sample(1:nrow(iris), 0.7*nrow(iris))
iris.train <- iris[train.id,]
dim(iris.train)

iris.test <- iris[-train.id,]
dim(iris.test)


iris.tree <- rpart(Species ~ ., iris.train)

# Grafiquemos
plot(iris.tree, margin = c(.25))
title(main = "Decision Tree Model of Iris Data")
text(iris.tree, use.n = TRUE)


rpart.plot::rpart.plot(iris.tree)
summary(iris.tree)

## Evaluación
iris.pred <- predict(iris.tree, iris.test, type = "class")

## Comparison table
iris.comp <- iris.test
iris.comp$Predicciones <- iris.pred
iris.comp[ , c("Species", "Predictions")]

## En cuáles nos equivocamos?
equiv.id <- iris.comp$Species != iris.comp$Predicciones
iris.comparison[equiv.id,]


### Ajuste de Parámetros ##

## Parámetros de rpart
tree.param <- rpart.control(minsplit = 20, minbucket = 7, maxdepth = 30, cp = 0.01)

## Ajustemos el modelo al conjunto de entrenamiento
## Usemos parameteros de arriba y Gini index para las divisiones
iris.tree <- rpart(Species ~ ., data = iris.train, 
                   control = tree.param, parms = list(split = "gini"))



#  Kyphosis data

## Exploración
data(kyphosis)
str(kyphosis)
dim(kyphosis)
summary(kyphosis)

set.seed(102)

train.id <- sample(1:nrow(kyphosis), 0.6*nrow(kyphosis))
kyphosis.train <- kyphosis[train.id,]
dim(kyphosis.train)

kyphosis.test <- kyphosis[-train.id,]
dim(kyphosis.test)

table(kyphosis.train$Kyphosis)
table(kyphosis.test$Kyphosis)

## MODELO 1
## Dividamos con Gini index, minsplit = 20, cp = 0.01

model.1 <- rpart(Kyphosis ~ ., data = kyphosis.train)

## MODELO 2
## Dividamos con  information gain, minsplit = 20, cp = 0.01
model.2 <- rpart(Kyphosis ~ ., data = kyphosis.train, parms = list(split = "information"))

## BUILD MODEL 3
## Splitting on gini index; cp = 0.1, minsplit = 10
model.3 <- rpart(Kyphosis ~ ., data = kyphosis.train, control = rpart.control(cp = 0.1, minsplit = 10))

## VISUALIZACION
par(mfrow = c(1,3), xpd = TRUE)
plot(model.1)
title(main = "Model 1")
text(model.1, use.n = TRUE)
plot(model.2)
title(main = "Model 2")
text(model.2, use.n = TRUE)
plot(model.3)
title(main = "Model 3")
text(model.3, use.n = TRUE)


# Random Forest -------------------------------------------------------------------------------------------


## load the library
library(randomForest)

## DATA EXPLORATION AND CLEANING
## load the Titanic data in R
titanic <- read.csv('https://raw.githubusercontent.com/jacobmunozc/ML-IA/main/Sesi%C3%B3n%205%20-%20Regresi%C3%B3n%20y%20clasificaci%C3%B3n%203/Data/titanic.csv')
## explore the data set
dim(titanic)
str(titanic)
summary(titanic)

## remove PassengerID, Name, Ticket, and Cabin attributes
titanic <- titanic[, -c(1, 4, 9, 11)]

## Cast target attribute to factor
titanic$Survived <- as.factor(titanic$Survived)

## there are some NAs in Age, fill them with the median value
titanic$Age[is.na(titanic$Age)] <- median(titanic$Age, na.rm=TRUE)

## BUILD MODEL
## randomly choose 70% of the data set as training data
set.seed(27)
train.index <- sample(1:nrow(titanic), 0.7*nrow(titanic))
titanic.train <- titanic[train.index,]
dim(titanic.train)
summary(titanic.train$Survived)
## select the other 30% as the testing data
titanic.test <- titanic[-train.index,]
dim(titanic.test)
summary(titanic.test$Survived)

## You could also do this
#random.rows.test <- setdiff(1:nrow(titanic),random.rows.train)
#titanic.test <- titanic[random.rows.test,]

## Fit decision model to training set
RF.model <- randomForest(Survived ~ ., data=titanic.train, importance = TRUE, ntree = 500)
print(RF.model)

## show variable importance
importance(RF.model)
varImpPlot(RF.model)

# Boosting --------------------------------------------------------------------------------------------------------


## load the library
library(bst)

## DATA EXPLORATION AND CLEANING
titanic <- read.csv('https://raw.githubusercontent.com/jacobmunozc/ML-IA/main/Sesi%C3%B3n%205%20-%20Regresi%C3%B3n%20y%20clasificaci%C3%B3n%203/Data/titanic.csv')
## ignore the PassengerID, Name, Ticket, and Cabin
titanic <- titanic[, -c(1, 4, 9, 11)]
## the bst default settings require that binary target classes have the values {-1, 1}
## map 0 -> -1 in Survived column; so 'Dead' is now coded as -1 rather than 0
titanic[titanic$Survived == 0, "Survived"] <- -1
## there are some NAs in Age, fill with the median value
titanic$Age[is.na(titanic$Age)] = median(titanic$Age, na.rm = TRUE)

## BUILD MODEL
## randomly choose 70% of the data set as training data
set.seed(27)
titanic.train.indices <- sample(1:nrow(titanic), 0.7*nrow(titanic), replace = FALSE)
titanic.train <- titanic[titanic.train.indices,]
dim(titanic.train)
summary(titanic.train$Survived)
## select the other 30% as the testing data
titanic.test <- titanic[-titanic.train.indices,]
dim(titanic.test)
summary(titanic.test$Survived)

## fitting decision model on training set
## note that bst doesn't take formula variables like previous models
## different models often have different requirements. Look at package manuals,
## or use ? to figure out what a given model requires
titanic.bst.model <- bst(titanic.train[,2:8], titanic.train$Survived, family = "hinge", learner = "tree")
#titanic.bst.model <- bst(titanic.train[,2:8], titanic.train$Survived, family = "hinge", learner = "tree", control.tree=list(maxdepth=2))
print(titanic.bst.model)


