# Turn this code into a function which takes the seed and generates data

# Kullback-Leibler divergence test of the distance measure of different seeds 
# > normalize_rows <- function(mat) {
#   +     # Apply normalization to each row
#     +     apply(mat, 1, function(row) row / sum(row))
#   + }
# > normalized_matrix <- t(normalize_rows(t(temp)))
# > 
#   > KL(normalized_matrix)


# Loading packages

library(dplyr)
library(mltools)
library(readxl)
library(glmnet)
library(purrr)
library(UBL)
library(keras3)
library(ggplot2)
library(tensorflow)
library(factoextra)
library(philentropy)
 
seed = 123

set.seed(seed)  

# use_python("C:\\Users\\adric\\AppData\\Local\\Programs\\Python\\Python312\\python.exe", 
#            required = TRUE)

# Reading in data

hospital <- read.csv("new.csv")

rownames(hospital) <- 1:nrow(hospital)

# In order to find optimal number for k, as it is just KNN. Graph supports our 
# grid search value of k = 2

fviz_nbclust(hospital, FUNcluster = kmeans, 
             diss = dist(hospital, method = "manhattan"))
k <- 2

# Can get rid of p as it is only used if p-norm is the chosen distance method

# dist and repl will be found using the parameter grid as they have a 
# small number of options. I kept them as options in the gird for now if we 
# want to check these values versus the other optimal values.

# Need to determine a method to choose thr.rel, C.perc, and pert. 

# For thr.rel, it seems the best method is to create a grid with strictly a 
# sequence for thr.rel every 0.02 from 0.2 to 0.8, and graph versus MSE output

# C.perc source code usage is below. It rates hospital length of stay values of 
# 20 and above and rescales them. If we increase C.perc[[1]], then there will be 
# more observations of the smaller class. Cannot find the source code for the 
# oversampling function (SMOGNRegress.exe), so not sure what the effect of 
# C.perc[[2]] directly is, but can assume larger it is the larger the higher 
# class is

# pert is the radius of the Gaussian noise used. Not sure how high of standard 
# deviation we want, but it does not seem to change the MSE at all. 

# Creating parameter grid

param_grid <- expand.grid(
  dist = c("Manhattan", "Euclidean", "Chebyshev"),
  thr.rel = seq(0.2, 0.8, by = 0.01),
  C.perc = c("balance", "extreme"),
  p = 1,
  k = k,
  repl = c(TRUE, FALSE),
  pert = 0.01
)

# Running SMOGNRegress

####################
# Values such as C.perc, repl, and pert are all a part of gaussian noise 
# as seen in function GaussNoiseClassif

# rel is the relevance function used to determine the phi control used as input 
# into phi, the relevance function. It can be a manually entered matrix 
# containing control.pts for phi.control, or "auto" in which it runs 
# phi.control(y, method = "extremes")

# thr.rel is then used as the threshold of when to make observations bumps. We 
# might want to increase the threshold as currently anything over 20 is 
# oversampled

# If C.perc is manually given, it must be a list the length of the bumps (2)
# C.perc = balanced uses the initial bumps
# C.perc = extreme uses a rescale which is made by: 
# rescale <- nbump * B/sum(B^2/sapply(obs.ind, length))
# obj <- round((B^2/sapply(obs.ind, length)) * rescale, 2)

# C.perc set to extreme returns values of C.perc[1] = 0.2 and C.perc[2] = 4.6

# C.perc[1] is the percentage of the data to be used as synthetic examples in 
# the minority class or the regions of the feature space where the target 
# variable is underrepresented.
# C.perc[2]: The percentage of the data to be used as synthetic examples in the 
# majority class or the regions where the target variable is more represented.

# k, dist, and pert are used to create new samples in the SMOGNRegress.exe 
# function if the C.perc for an observation is > 1 (minority class), 
# implemented using:
# rbind(newdata, newExs, dat[names(obs.ind[[i]])
# Use getAnywhere(SMOGNRegress.exs) to see source code

# If the C.perc value for an observation is < 1 (majority class), then 
# dat[names(obs.ind[[i]][sel.maj] is added as an observation made by:
# sel.maj <- sample(1:length(obs.ind[[i]]), as.integer(C.perc[[i]] * 
# length(obs.ind[[i]])), replace = repl)

# Here is how to pull up source code:

browser(SMOGNRegress(
  form = as.formula("hospital_length_of_stay ~ ."),
  dat = hospital,
  dist = as.character(best_params[['dist']]),
  thr.rel = best_params[['thr.rel']],
  C.perc = best_params[['C.perc']],
  k = best_params[['k']],
  repl = best_params[['repl']],
  pert = best_params[['pert']]
))

thr.rel <- 0.77
dat <- hospital
target_position <- 3
C.perc <- "extreme"
dist <- "Manhattan"
repl <- FALSE
pert <- 0.01
rel = "auto"
p <- 1

y <- dat[, target_position]
pc <- phi.control(y, method = "extremes")

##### rownames(dat) is null, should we assign rownames 1:nrow(dat)?

attr(y, "names") <- rownames(dat)
s.y <- sort(y)
temp <- y.relev <- phi(s.y, pc)


# Examining relevance function

n <- length(y)
Charmeth <- pc[[1]]
meth <- ifelse(Charmeth == "extremes", 0, 1)
npts <- pc[[2]]
lparms <- length(pc[[3]])
phiParms <- pc[[3]]
yPhi <- rep(0, times = n)
ydPhi <- rep(0, times = n)
yddPhi <- rep(0, times = n)
storage.mode(n) <- "integer"
storage.mode(y) <- "double"
storage.mode(meth) <- "integer"
storage.mode(npts) <- "integer"
storage.mode(lparms) <- "integer"
storage.mode(phiParms) <- "double"
storage.mode(yPhi) <- "double"
storage.mode(ydPhi) <- "double"
storage.mode(yddPhi) <- "double"
res <- .Fortran("rtophi", n = n, y = y, method = meth, npts = npts, 
                lparms = lparms, phiParms = phiParms, yPhi = yPhi, ydPhi = ydPhi, 
                yddPhi = yddPhi)
res1 <- res$yPhi

tmp <- y - res1
mean(tmp)



bumps <- c()
for (i in 1:(length(y) - 1)) {
  if ((temp[i] >= thr.rel && temp[i + 1] < thr.rel) || 
      (temp[i] < thr.rel && temp[i + 1] >= thr.rel)) {
    bumps <- c(bumps, i)
  }
}
nbump <- length(bumps) + 1
obs.ind <- as.list(rep(NA, nbump))
last <- 1
for (i in 1:length(bumps)) {
  obs.ind[[i]] <- s.y[last:bumps[i]]
  last <- bumps[i] + 1
}

# entering sampling data

obs.ind[[nbump]] <- s.y[last:length(s.y)]

ndata <- data.frame()

# For "extreme" C.perc as optimized by grid search:

B <- round(nrow(dat)/nbump, 0)
rescale <- nbump * B/sum(B^2/sapply(obs.ind, length))
obj <- round((B^2/sapply(obs.ind, length)) * rescale, 2)
C.perc <- round(obj/sapply(obs.ind, length), 1)

set.seed(seed)

for (i in 1:nbump) {
  if (C.perc[[i]] == 1) {
    newdata <- rbind(newdata, dat[names(obs.ind[[i]]), 
    ])
  }
  else if (C.perc[[i]] > 1) {
    # newExs <- SMOGNRegress.exs(dat, names(obs.ind[[i]]), 
    #                            ncol(dat), C.perc[[i]], k, dist, p, pert)
    # newdata <- rbind(newdata, newExs, dat[names(obs.ind[[i]]), 
    # ])
    
    #### Examining SMOGNRegress.exs
    
    # function (orig, ind, tgt, N, k, dist, p, pert) 
      
    orig = dat
    ind = names(obs.ind[[i]])
    
    ############# ind is null, passing 0 through the function
    
    tgt = ncol(dat)
    N = C.perc[[i]]
    k = k
    dist = dist
    p = p
    pert = pert
    
    indpos <- match(ind, rownames(orig))
    dat <- orig[indpos, ]
    ConstFeat <- which(apply(dat, 2, function(col) {
      length(unique(col)) == 1
    }))
    
    
  }
  else if (C.perc[[i]] < 1) {
    sel.maj <- sample(1:length(obs.ind[[i]]), 
                      as.integer(C.perc[[i]] * length(obs.ind[[i]])), 
                      replace = repl)
    newdata <- rbind(newdata, dat[names(obs.ind[[i]][sel.maj]), 
    ])
  }
}


###################

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

set.seed(seed)  

train_indices <- sample(seq_len(nrow(newdata)), size = 0.5 * nrow(newdata))
xtrain <- as.matrix(x[train_indices, ])
ytrain <- as.matrix(y[train_indices, , drop = FALSE]) 
xtest <- as.matrix(x[-train_indices, ])
ytest <- as.matrix(y[-train_indices, , drop = FALSE] )

# Extracting best model

cv_model <- cv.glmnet(xtrain, ytrain, alpha = 1)
best_lambda <- cv_model$lambda.min
lasso_model <- glmnet(xtrain, ytrain, alpha = 1, lambda = best_lambda)

# Extracting coefficients of best model

coefficients <- coef(lasso_model)

coef_table <- as.data.frame(as.matrix(coefficients))
names(coef_table) <- "Coefficient"

signif_coefs <- coef_table %>% 
  filter(Coefficient != 0)


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


# Finding optimal value of thr.rel

thr.rel_grid <- expand.grid(
  dist = "Euclidean",
  thr.rel = seq(0.2, 0.8, by = 0.01),
  C.perc = "extreme",
  p = 1,
  k = 2,
  repl = TRUE,
  pert = 0.01
)

thr.rel_results <- data.frame()
for (i in 1:nrow(thr.rel_grid)) {
  params <- thr.rel_grid[i, ]
  eval <- evaluate_model(params, hospital)
  
  eval <- eval %>% 
    mutate(grid_row = i) %>% 
    rename('original' = 'hospital_length_of_stay',
           'residual' = 'hospital_length_of_stay.1') %>% 
    select(grid_row, everything())
  
  thr.rel_results <- rbind(thr.rel_results, eval)
}

thr.rel_results <- thr.rel_results %>% 
  group_by(grid_row) %>% 
  select(-c("original", "predicted", "residual")) %>% 
  distinct() %>% 
  mutate(thr.rel = sapply(grid_row, function(x) thr.rel_grid[x, 2]))

thr.rel_results %>% 
  ggplot(mapping = aes(x = thr.rel, y = MSE)) +
  geom_point() +
  theme_minimal() +
  labs(title = 'Optimal thr.rel', x = 'thr.rel', y = 'MSE')

best_thr.rel <- which.min(thr.rel_results$MSE)
best_thr.rel <- thr.rel_results[best_thr.rel, "thr.rel"]

best_thr.rel

# Best thr.rel is 0.29 with Manhattan distance
# Best thr.rel is 0.77 with Euclidean distance


# Finding optimal value of pert

pert_grid <- expand.grid(
  dist = "Manhattan",
  thr.rel = 0.77,
  C.perc = "extreme",
  p = 1,
  k = 2,
  repl = TRUE,
  pert = seq(0.01, 2.01, by = 0.05)
)

pert_results <- data.frame()
for (i in 1:nrow(pert_grid)) {
  params <- pert_grid[i, ]
  eval <- evaluate_model(params, hospital)
  
  eval <- eval %>% 
    mutate(grid_row = i) %>% 
    rename('original' = 'hospital_length_of_stay',
           'residual' = 'hospital_length_of_stay.1') %>% 
    select(grid_row, everything())
  
  pert_results <- rbind(pert_results, eval)
}

pert_results <- pert_results %>% 
  group_by(grid_row) %>% 
  select(-c("original", "predicted", "residual")) %>% 
  distinct() %>% 
  mutate(pert = sapply(grid_row, function(x) pert_grid[x, 7]))

pert_results %>% 
  ggplot(mapping = aes(x = pert, y = MSE)) +
  geom_point() +
  theme_minimal() +
  labs(title = 'Optimal pert', x = 'pert', y = 'MSE')

best_pert <- which.min(pert_results$MSE)
best_pert <- pert_results[best_pert, "pert"]

best_pert

# pert does not seem to have an impact on the SMOGNRegress












# # Establishing a neural network
# 
# # # can build rnn on patient to patient basis on day by day data that models each patient independantly
# 
# timesteps = 1
# 
# xtrain_rnn <- array_reshape(xtrain, c(nrow(xtrain), timesteps, ncol(xtrain)))
# xtest_rnn <- array_reshape(xtest, c(nrow(xtest), timesteps, ncol(xtest)))
# 
# rnn_model <- keras_model_sequential() %>%
#   layer_lstm(units = 50, activation = 'relu', 
#              input_shape = c(timesteps, nrow(xtrain))) %>%
#   layer_dropout(rate = 0.4) %>%
#   layer_dense(units = 1)  
# 
# rnn_model %>% compile(optimizer = 'adam', loss = 'categorical_crossentropy', 
#                       metrics = c('accuracy'))
# 
# rnn_model %>% fit(
#   x = xtrain_rnn,
#   y = ytrain,
#   epochs = 10,
#   batch_size = 32,
#   validation_data = list(xtest_rnn, ytest)
# )
# 
# 
# modnn <- keras_model_sequential() %>%
#   layer_dense(units = 50, activation = "relu", 
#               input_shape = ncol(xtrain)) %>%
#   layer_dropout(rate = 0.4) %>%
#   layer_dense(units = 1)
# 
# modnn %>% compile(loss = "mse", optimizer = optimizer_rmsprop(),
#                   metrics = list("mean_absolute_error"))
# 
# # Fitting neural network
# 
# 
# history <- modnn %>% fit(
#   xtrain, ytrain, epochs = 500, batch_size = 32,
#   validation_data = list(xtest, ytest)
# )
# 
# class(history)










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


