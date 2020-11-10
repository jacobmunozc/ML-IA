# Sesión 3
# No distribuir ni copiar sin autorización

require(McSpatial)
data("matchdata")

?matchdata

set.seed(1010100)

require(dplyr)

matchdata <- matchdata %>% 
  mutate(
    price = exp(lnprice),
    holdout = as.logical(
      1:nrow(matchdata) %in% 
      sample(nrow(matchdata),nrow(matchdata)*.7)
      )
    )

test <- matchdata[matchdata$holdout == TRUE,]
train <- matchdata[matchdata$holdout == FALSE,]

model1 <- lm(price ~ 1, data = train)
summary(model1)

coef(model1)
mean(train$price)

test$model1 <- predict(model1, newdata = test)
with(test,mean((price - model1)**2))


model2 <- lm(price ~ bedrooms, data = train)
test$model2 <- predict(model2, newdata = test)
with(test,mean((price - model2)**2))

model3 <- lm(price ~ bedrooms + bathrooms + centair + fireplace + brick, data = train)
test$model3 <- predict(model3, newdata = test)
with(test,mean((price - model3)**2))


model4 <- lm(price ~ bedrooms + bathrooms + centair + fireplace + brick +
               lnland + lnbldg + rooms + garage1 + garage2 + dcbd + rr +
               yrbuilt + factor(carea) + latitude + longitude, data = train)
test$model4 <- predict(model4, newdata = test)
with(test,mean((price - model4)**2))


# Volvamoslo una regresión polinomial

model5 <- lm(price ~ poly(bedrooms,2) + poly(bathrooms,2) + centair + fireplace + brick +
               lnland + lnbldg + rooms + garage1 + garage2 + dcbd + rr +
               yrbuilt + factor(carea) + poly(latitude,2) + poly(longitude,2), data = train)
test$model5 <- predict(model5, newdata = test)
with(test,mean((price - model5)**2))


model6 <- lm(price ~ poly(bedrooms,2) + poly(bathrooms,2) + centair + fireplace + brick +
               lnland + lnbldg + garage1 + garage2 + rr +
               yrbuilt + factor(carea) + poly(latitude,2) + poly(longitude,2), data = train)
test$model6 <- predict(model6, newdata = test)
with(test,mean((price - model6)**2))


model7 <- lm(price ~ poly(bedrooms,2) + poly(bathrooms,2) + centair + fireplace + brick +
               lnland + lnbldg + rooms + garage1 + garage2 + dcbd + rr +
               yrbuilt + factor(carea) + poly(latitude,3) + poly(longitude,3), data = train)
test$model5 <- predict(model5, newdata = test)
with(test,mean((price - model5)**2))




# ¿Cómo encontrar el nivel óptimo de complejidad? CV

require(caret)

# Definir control de entrenamiento
train.control <- trainControl(method = "cv", number = 10)
# train.control <- trainControl(method = "LOOCV")
# Entrenar el modelo
model <- train(price ~., data = matchdata, method = "lm",
               trControl = train.control)

print(model)


# Looping en R

aux <- c()
for (x in 1:3) {
  aux[x] <- x
}

aux <- c()
lapply(1:3,function(x) aux[x] <- x)


###### Encontrar el mejor modelo 

# 1. Encontrar el CV de cada modelo

vars <- list(
  model1 = c('bedrooms','bathrooms','centair','fireplace','brick'),
  model2 = c('bedrooms','bathrooms','centair','fireplace','brick',
               'lnland','lnbldg','rooms','garage1','garage2','dcbd','rr',
               'yrbuilt','latitude','longitude'),
  model3 = c('bedrooms')
) 

model <- c()
for (x in 1:3) {
  model[x] <- train(x = matchdata %>% select(all_of(vars[[x]])),
                 y = matchdata$price,
                 # data = matchdata,
                 method = "lm", metric = "RMSE",
                 trControl = trainControl(method = "cv", number = 10))$results$RMSE
}

model

ggplot(NULL,aes(x = 1:3, y = model)) + geom_bar(stat = 'identity') + 
  labs(x = 'modelo', y = 'RMSE', title = 'Validación cruzada de modelos') + 
  theme_bw()

# 2. Elegir el modelo con menor AveragedMSE (CV)
# Modelo 2 tiene el mejor desempeño de los 3 modelos ajustados.


