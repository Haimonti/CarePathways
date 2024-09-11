# Loading packages

library(smotefamily)
library(dplyr)
library(mltools)
library(readxl)
library(glmnet)
library(purrr)
library(UBL)
library(keras3)
library(ggplot2)
library(tensorflow)

use_python("C:\\Users\\adric\\AppData\\Local\\Programs\\Python\\Python312\\python.exe", 
           required = TRUE)

# Reading in data

hospital <- read.csv("new.csv")

# Creating parameter grid

param_grid <- expand.grid(
  dist = c("Manhattan", "Euclidean", "Chebyshev", "p-norm"),
  thr.rel = c(0.4, 0.5, 0.6),
  C.perc = c("balance", "extreme"),
  p = c(1, 2),
  k = c(2, 5, 10, 15, 20),
  repl = c(TRUE, FALSE),
  pert = c(0.01, 0.1, 0.5)
)

# Running SMOGNRegress

evaluate_model <- function(params, data, seed = 123) {
  newdata <- SMOGNRegress(
    form = as.formula("hospital_length_of_stay ~ ."),
    dat = data,  
    dist = as.character(params$dist),
    thr.rel = params$thr.rel,
    C.perc = params$C.perc,
    p = params$p,
    k = params$k,
    repl = params$repl,
    pert = params$pert
  )
  

  x <- newdata %>% 
    select(-hospital_length_of_stay)
  y <- newdata %>% 
    select(hospital_length_of_stay)
  
  set.seed(seed)  
  
  train_indices <- sample(seq_len(nrow(newdata)), size = 0.75 * nrow(newdata))
  xtrain <- as.matrix(x[train_indices, ])
  ytrain <- as.matrix(y[train_indices, , drop = FALSE]) 
  xtest <- as.matrix(x[-train_indices, ])
  ytest <- as.matrix(y[-train_indices, , drop = FALSE] )
  
  cv_model <- cv.glmnet(xtrain, ytrain, alpha = 1)
  best_lambda <- cv_model$lambda.min
  lasso_model <- glmnet(xtrain, ytrain, alpha = 1, lambda = best_lambda)
  
  predictions <- predict(lasso_model, newx = xtest, s = best_lambda)
  
  predictions <- as.vector(predictions)
  
  mse <- mean((ytest - predictions)^2, na.rm = TRUE)
  residuals <- ytest - predictions
  
  original = ytest

  return(data.frame(seed = seed, original = original, predicted = predictions,
                    residuals = residuals, MSE = mse))
}

# Placing results into data frame

results <- data.frame()
for (i in 1:nrow(param_grid)) {
  params <- param_grid[i, ]
  eval <- evaluate_model(params, hospital)
  
  eval <- eval %>% 
    mutate(grid_row = i) %>% 
    rename('original' = 'hospital_length_of_stay',
           'residual' = 'hospital_length_of_stay.1') %>% 
    select(grid_row, everything())
    
  results <- rbind(results, eval)
}

# Identifying best parameters 

best_param_id <- which.min(results$MSE)
best_param_id <- results[best_param_id, "grid_row"]


best_params <- param_grid[best_param_id, ]

# Extracting best data

newdata <- SMOGNRegress(
  form = as.formula("hospital_length_of_stay ~ ."),
  dat = hospital,  
  dist = as.character(best_params[['dist']]),
  thr.rel = best_params[['thr.rel']],
  C.perc = best_params[['C.perc']],
  p = best_params[['p']],
  k = best_params[['k']],
  repl = best_params[['repl']],
  pert = best_params[['pert']]
)

# Splitting best data

x <- newdata %>% 
  select(-hospital_length_of_stay)
y <- newdata %>% 
  select(hospital_length_of_stay)

seed = 123

set.seed(seed)  

train_indices <- sample(seq_len(nrow(newdata)), size = 0.75 * nrow(newdata))
xtrain <- as.matrix(x[train_indices, ])
ytrain <- as.matrix(y[train_indices, , drop = FALSE]) 
xtest <- as.matrix(x[-train_indices, ])
ytest <- as.matrix(y[-train_indices, , drop = FALSE] )

# Extracting best model

cv_model <- cv.glmnet(xtrain, ytrain, alpha = 1)
best_lambda <- cv_model$lambda.min
lasso_model <- glmnet(xtrain, ytrain, alpha = 1, lambda = best_lambda)

# Graphing results

best_param_results <- results %>% 
  filter(grid_row == best_param_id) %>% 
  mutate(id = row_number())

ggplot(data = best_param_results, mapping = aes(x = original, y = predicted)) +
geom_point() + labs(title = "Predicted Values vs Original Values", 
                    x = "Original", y = "Predicted") + theme_minimal()

ggplot(data = best_param_results, mapping = aes(x = predicted, y = residual)) +
geom_point() + labs(title = "Fitted Values vs Residuals", x = "Fitted", 
                    y = 'Residual') + theme_minimal()

ggplot(data = best_param_results, mapping = aes(x = id)) +
  geom_point(aes(y = original, color = 'Original')) +
  geom_point(aes(y = predicted, color = 'Predicted')) +
  geom_segment(aes(xend = id, yend = predicted, y = original), color = 'azure4', 
               linetype = 'dashed') +
  scale_color_manual(name = "Legend", values = c('Original' = 'cornsilk3',
                                              'Predicted' = 'darkslateblue')) +
  labs(title = 'Residual Index Plot', x = 'Index', y = 'Values') +
  theme_minimal() 

hist(hospital$hospital_length_of_stay)

hist(best_param_results$original)



# Establishing a neural network

leng <- ncol(xtrain) + ncol(xtest)

modnn <- keras_model_sequential() %>%
  layer_dense(units = 50, activation = "relu", input_shape = leng) %>%
  layer_dropout(rate = 0.4) %>%
  layer_dense(units = 1)

modnn %>% compile(loss = "mse",
                  optimizer = optimizer_rmsprop(),
                  metrics = list("mean_absolute_error")
)

# Fitting neural network


history <- modnn %>% fit(
  xtrain, ytrain, epochs = 15, batch_size = 32,
  validation_data = list(xtest, ytest)
)

class(history)










################

# # Attempting Auto Encoder
# # Was told to hold off on this.
# 
# 
# # Partitioning data
# 
# x <- hospital %>% 
#   select(-hospital_length_of_stay)
# y <- hospital %>% 
#   select(hospital_length_of_stay)
# 
# set.seed(123)  
# 
# train_indices <- sample(seq_len(nrow(hospital)), size = 0.75 * nrow(hospital))
# xtrain <- as.matrix(x[train_indices, ])
# ytrain <- as.matrix(y[train_indices, , drop = FALSE]) 
# xtest <- as.matrix(x[-train_indices, ])
# ytest <- as.matrix(y[-train_indices, , drop = FALSE] )
# 
# encoder1 <- keras_model_sequential() %>% 
#   layer_dense(units = 54, activation = "relu", input_shape = c(908)) %>% 
#   layer_dense(units = 2, activation = "relu")
# 
# decoder1 <- keras_model_sequential() %>%
#   layer_dense(units = 64, activation = 'relu', input_shape = c(2)) %>%
#   layer_dense(units = 784, activation = 'sigmoid')
# 
# autoencoder1 <- keras_model(inputs = encoder1$input, outputs = decoder(encoder1$output))
# 
# autoencoder1 %>% compile(optimizer = 'adam', loss = 'binary_crossentropy')
# 
# autoencoder1 %>% fit(x_train, x_train, epochs = 50, batch_size = 256, validation_data = list(x_test, x_test))
# 
# summary(autoencoder1)


