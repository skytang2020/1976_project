library(tidyverse)
library(glmnet)
library(plotmo)
data = read_csv2("./data/train.csv")[,-c(1,2)] %>% 
  mutate(class = factor(class))
str(data)
X = model.matrix(class ~., data)[,-1]
y = data$class

set.seed(1)
cv.lasso = cv.glmnet(X,y, alpha = 1, lambda =  exp(seq(-10, 10, length = 100)), family = "binomial", nfolds = 5)
cv.lasso$lambda.min
cv.lasso$cvm %>% min # The mean cross-validated error - a vector of length length(lambda).
plot(cv.lasso)
plot_glmnet(cv.lasso$glmnet.fit)

predict(cv.lasso, s = "lambda.min", type = "coefficients")

cv_error = matrix( ncol = 100, nrow = 10)
alpha_grid = c(0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9)
cv.enet = list()
for (i in 1:10) {
  set.seed(1)
  cv.enet[[i]] = cv.glmnet(X,y, alpha = alpha_grid[i], lambda =  exp(seq(-10, 10, length = 100)), family = "binomial", nfolds = 5)
  cv_error[i,] = cv.enet[[i]]$cvm
}

min(cv_error) # alpha = 0.7, lambda =94.20324# exp(seq(-10, 10, length = 100))[73]
