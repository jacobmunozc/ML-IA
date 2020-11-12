
require(McSpatial)

data("matchdata")

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

require(caret)

lambda <- 10**seq(-2,3, length = 100)

# Lasso
lasso <- train(
  price ~ ., 
  data = matchdata %>% dplyr::select(-holdout), 
  method = 'glmnet',
  trControl = trainControl('cv', number = 10),
  tuneGrid = expand.grid(alpha = 1, lambda = lambda),
  preProcess = c('center','scale')
)

lasso





# Ridge

ridge <- train(
  price ~ ., 
  data = matchdata %>% dplyr::select(-holdout), method = 'glmnet',
  trControl = trainControl('cv', number = 10),
  tuneGrid = expand.grid(alpha = 0, lambda = lambda),
  preProcess = c('center','scale')
)

ridge




# Elastic Net

EN <- train(
  price ~ ., 
  data = matchdata %>% dplyr::select(-holdout), method = 'glmnet',
  trControl = trainControl('cv', number = 10),
  preProcess = c('center','scale')
)

EN


models <- list(ridge = ridge, lasso = lasso, EN = EN)
resamples(models) %>% summary(metric = 'RMSE')


# Ejercicio 1: grafique los coeficientes de cada atributo en un scatter plot
# Pista: coeficiente (y), variable (x), modelo (color)



# Ejercicio 2: Aplique lo que acabamos de aprender con Swiss data
# Separar dato en entrenamiento y prueba
# 1. Determine RMSE de OLS
# 2. Determine RMSE de Lasso
# 3. Determine RMSE de Ridge
# 3. Determine RMSE de EN
# 4. Compare 

data("swiss")

?swiss
