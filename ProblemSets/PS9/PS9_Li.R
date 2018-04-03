# load packages
library(glmnet)
library(mlr)
library(ParamHelpers)

# Q4 
# load Housing Data 
housing <- read.table("http://archive.ics.uci.edu/ml/machine-learning-databases/housing/housing.data")

# check data names 
names(housing)

# replace data names to match code in HW
names(housing) <- c("crim","zn","indus","chas","nox","rm","age","dis","rad","tax","ptratio","b","lstat","medv")

# verify changes 
names(housing)




# Q5

housing$lmedv <- log(housing$medv)
housing$medv <- NULL # drop median value
formula<- as.formula(lmedv ~ .^3 +
                          poly(crim,6) +
                          poly(zn,6) +
                          poly(indus,6) +
                          poly(nox,6) +
                          poly(rm,6) +
                          poly(age,6) +
                          poly(dis,6) +
                          poly(rad,6) +
                          poly(tax,6) +
                          poly(ptratio,6) +
                          poly(b,6) +
                          poly(lstat,6))

#now replace the intercept column by the response since MLR will do
#"y ~ ." and get the intercept by default
mod_matrix <- data.frame(model.matrix(formula,housing))

mod_matrix [,1] = housing$lmedv

# Q6 
# LASSO Regression 

# rename col names
colnames(mod_matrix)[1] = "lmedv"

# Split training and test data for mod matrix
n <- nrow(mod_matrix)
train <- sample(n, size = .8*n)
test <- setdiff(1:n, train)
housing.train <- mod_matrix[train,]
housing.test <- mod_matrix[test,]


dim(housing.train)
# Feature engineering 
#housing$lmedv <- log(housing$medv)
housing$medv <- NULL
housing$dis2 <- housing$dis^2
housing$chasNOX <- housing$crim * housing$nox

# Split training and test data
n <- nrow(housing)
train <- sample(n, size = .8*n)
test  <- setdiff(1:n, train)
housing.train <- housing[train,]
housing.test  <- housing[test, ]

# Define the task:
theTask <- makeRegrTask(id = "taskname", data = housing.train, target = "lmedv")
print(Task)

# tell mlr what prediction algorithm we'll be using (OLS)
predAlg <- makeLearner("regr.lm")

# 6-fold CV
resampleStrat <- makeResampleDesc(method = "CV", iters = 6)

# Do the resampling
sampleResults <- resample(learner = predAlg, task = theTask, resampling = resampleStrat, measures=list(rmse))

# Mean RMSE across the 6 folds
print(sampleResults$aggr)
#rmse.test.rmse 
#0.1943245 

#L1 LASSO Regression 

# prediction algorithm
predAlg <- makeLearner("regr.glmnet")

# Search over penalty parameter lambda and force elastic net parameter to be 1 (LASSO)
modelParams <- makeParamSet(makeNumericParam("lambda",lower=0,upper=1),makeNumericParam("alpha",lower=1,upper=1))

# Take 50 random guesses at lambda within the interval I specified above
tuneMethod <- makeTuneControlRandom(maxit = 50L)

# Do the tuning
tunedModel <- tuneParams(learner = predAlg,
                         task = theTask,
                         resampling = resampleStrat,
                         # RMSE performance measure, this can be changed to one or many
                         measures = rmse,      
                         par.set = modelParams,
                         control = tuneMethod,
                         show.info = TRUE)

# Apply the optimal algorithm parameters to the model
predAlg <- setHyperPars(learner=predAlg, par.vals = tunedModel$x)

# Verify performance on cross validated sample sets
resample(predAlg,theTask,resampleStrat,measures=list(rmse))

# Train the final model
finalModel6 <- train(learner = predAlg, task = theTask)

# Predict in test set!
prediction6 <- predict(finalModel6, newdata = housing.test)

print(head(prediction6$data))

# From the email 
OSE6<-sqrt(mean((prediction6$data$truth-prediction6$data$response)^2))
print(OSE6)

#Question 7 
#L2 Ridge Regression 

# Search over penalty parameter lambda and force elastic net parameter to be 0 (ridge)
modelParams <- makeParamSet(makeNumericParam("lambda",lower=0,upper=1),makeNumericParam("alpha",lower=0,upper=0))

# Do the tuning again
tunedModel <- tuneParams(learner = predAlg,
                         task = theTask,
                         resampling = resampleStrat,
                         measures = rmse,       # RMSE performance measure, this can be changed to one or many
                         par.set = modelParams,
                         control = tuneMethod,
                         show.info = TRUE)

# Apply the optimal algorithm parameters to the model
predAlg <- setHyperPars(learner=predAlg, par.vals = tunedModel$x)

# Verify performance on cross validated sample sets
resample(predAlg,theTask,resampleStrat,measures=list(rmse))

# Train the final model
finalModel7 <- train(learner = predAlg, task = theTask)

# Predict in test set!
prediction7 <- predict(finalModel7, newdata = housing.test)

print(prediction7)
# From the email 
OSE7<-sqrt(mean((prediction7$data$truth-prediction7$data$response)^2))
print(OSE7)

#Question 8

# Search over penalty parameter lambda and force elastic net parameter to be 0 (ridge)
modelParams <- makeParamSet(makeNumericParam("lambda",lower=0,upper=1),makeNumericParam("alpha",lower=0,upper=1))

# Do the tuning again
tunedModel <- tuneParams(learner = predAlg,
                         task = theTask,
                         resampling = resampleStrat,
                         measures = rmse,       # RMSE performance measure, this can be changed to one or many
                         par.set = modelParams,
                         control = tuneMethod,
                         show.info = TRUE)

# Apply the optimal algorithm parameters to the model
predAlg <- setHyperPars(learner=predAlg, par.vals = tunedModel$x)

# Verify performance on cross validated sample sets
resample(predAlg,theTask,resampleStrat,measures=list(rmse))

# Train the final model
finalModel8 <- train(learner = predAlg, task = theTask)

# Predict in test set
prediction8 <- predict(finalModel8, newdata = housing.test)

print(prediction8)

# From the email 
OSE8<-sqrt(mean((prediction8$data$truth-prediction8$data$response)^2))
print(OSE8)








