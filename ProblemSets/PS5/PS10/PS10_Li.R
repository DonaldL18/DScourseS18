library(rpart)
library(class)
library(nnet)
library(mlr)
library(e1071)
library(kknn)



#set seed for repreducability 
set.seed(100)

#load data 
income <- read.table("https://archive.ics.uci.edu/ml/machine-learning-databases/adult/adult.data")

#set names for data set 
names(income) <- c("age","workclass","fnlwgt","education","education.num","marital.status","occupation","relationship","race","sex","capital.gain","capital.loss","hours","native.country","high.earner")

# Clean up the data
# Drop unnecessary columns
income$native.country <- NULL
income$fnlwgt         <- NULL

# Make sure continuous variables are stored as numeric values 
income$age            <- as.numeric(income$age)
income$hours          <- as.numeric(income$hours)
income$education.num  <- as.numeric(income$education.num)
income$capital.gain   <- as.numeric(income$capital.gain)
income$capital.loss   <- as.numeric(income$capital.loss)

# Combine levels of categorical variables that currently have too many levels
levels(income$education) <- list(Advanced = c("Masters,","Doctorate,","Prof-school,"), Bachelors = c("Bachelors,"), "Some-college" = c("Some-college,","Assoc-acdm,","Assoc-voc,"), "HS-grad" = c("HS-grad,","12th,"), "HS-drop" = c("11th,","9th,","7th-8th,","1st-4th,","10th,","5th-6th,","Preschool,"))
levels(income$marital.status) <- list(Married = c("Married-civ-spouse,","Married-spouse-absent,","Married-AF-spouse,"), Divorced = c("Divorced,","Separated,"), Widowed = c("Widowed,"), "Never-married" = c("Never-married,"))
levels(income$race) <- list(White = c("White,"), Black = c("Black,"), Asian = c("Asian-Pac-Islander,"), Other = c("Other,","Amer-Indian-Eskimo,"))
levels(income$workclass) <- list(Private = c("Private,"), "Self-emp" = c("Self-emp-not-inc,","Self-emp-inc,"), Gov = c("Federal-gov,","Local-gov,","State-gov,"), Other = c("Without-pay,","Never-worked,","?,"))
levels(income$occupation) <- list("Blue-collar" = c("?,","Craft-repair,","Farming-fishing,","Handlers-cleaners,","Machine-op-inspct,","Transport-moving,"), "White-collar" = c("Adm-clerical,","Exec-managerial,","Prof-specialty,","Sales,","Tech-support,"), Services = c("Armed-Forces,","Other-service,","Priv-house-serv,","Protective-serv,"))

# Break up the data:
n <- nrow(income)
train <- sample(n, size = .8*n)
test  <- setdiff(1:n, train)
income.train <- income[train,]
income.test  <- income[test, ]
##############################
# Question 5 
##############################

# Define the task:
task.highearner <- makeClassifTask(data = income.train, target = "high.earner")
print(task.highearner)

#Set resampling strategy (here let's do 3-fold CV)
resampleStrat <- makeResampleDesc(method = "CV", iters = 3)

#Take 10 random guesses at lambda
tuneMethod <- makeTuneControlRandom(maxit = 10L)

# Assign Laearners
trees <- makeLearner("classif.rpart", predict.type = "response")
logit <- makeLearner("classif.glmnet", predict.type = "response")
nn <- makeLearner("classif.nnet", predict.type = "response")
nb <- makeLearner("classif.naiveBayes", predict.type = "response")
knn <-makeLearner("classif.kknn", predict.type = "response")
svm <- makeLearner("classif.svm", predict.type = "response")

##############################
# Question 6 
# Setting the hyperparameters 
##############################

# Decesion Tree
PARAM.Tree <- makeParamSet(
  makeIntegerParam("minsplit", lower = 10, upper = 50),
  makeIntegerParam("minbucket", lower = 5, upper = 50),
  makeNumericParam("cp", lower = 0.001, upper = 0.2))

# Logit  
PARAM.Logit <- makeParamSet(
   makeNumericParam("lambda",lower=0,upper=3),
   makeNumericParam("alpha",lower=0,upper=1))

# NN
PARAM.NN <- makeParamSet(
  makeIntegerParam("size", lower = 1, upper = 10),
  makeNumericParam("decay", lower = 0.1, upper = 0.5),
  makeIntegerParam("maxit", lower = 1000, upper = 1000))


# KNN
PARAM.KNN<- makeParamSet(
  makeIntegerParam("k", lower = 1, upper = 30))

#SVM
PARAM.SVM  <- makeParamSet(
  makeDiscreteParam("cost", values = 2^c(-2, -1, 0, 1, 2, 10)), 
  makeDiscreteParam("gamma", values = 2^c(-2, -1, 0, 1, 2, 10)))

#Question 7 
##############################
# TUNE #
##############################

# Tree
TUNE.Tree <- tuneParams(learner = trees,
                         task = task.highearner,
                         resampling = resampleStrat,
                         measures = list(f1, gmean),
                         par.set = PARAM.Tree,
                         control = tuneMethod,
                         show.info = TRUE)
# Logit
TUNE.Logit <- tuneParams(learner = logit,
                         task = task.highearner,
                         resampling = resampleStrat,
                         measures = list(f1, gmean),
                         par.set = PARAM.Logit,
                         control = tuneMethod,
                         show.info = TRUE)
# NN
TUNE.NN <- tuneParams(learner = nn,
                         task = task.highearner,
                         resampling = resampleStrat,
                         measures = list(f1, gmean),      
                         par.set = PARAM.NN,
                         control = tuneMethod,
                         show.info = TRUE)

# KNN
TUNE.KNN <- tuneParams(learner = knn,
                         task = task.highearner,
                         resampling = resampleStrat,
                         measures = list(f1, gmean),      
                         par.set = PARAM.KNN,
                         control = tuneMethod,
                         show.info = TRUE)
# SVM
TUNE.SVM <- tuneParams(learner = svm,
                         task = task.highearner,
                         resampling = resampleStrat,
                         measures = list(f1, gmean),      
                         par.set = PARAM.SVM,
                         control = tuneMethod,
                         show.info = TRUE)

##############################
# Question 8

# choo-choo TRAIN

##############################

# apply the optimal algorithm parameters to the model
PRED.Trees <- setHyperPars(learner=trees, par.vals = TUNE.Trees$x)
PRED.Logit <- setHyperPars(learner=logit, par.vals = TUNE.Logit$x)
PRED.NN    <- setHyperPars(learner=nn, par.vals = TUNE.NN$x)
PRED.KNN   <- setHyperPars(learner=knn, par.vals = TUNE.KNN$x)
PRED.SVM   <- setHyperPars(learner=svm, par.vals = TUNE.SVM$x)

# verify performance on cross validated sample 
SAMPLE.Tree  <- resample(learner = PRED.Trees, task = task.highearner, resampling = resampleStrat, measures=list(gmean))
SAMPLE.Logit <- resample(learner = PRED.Logit, task = task.highearner, resampling = resampleStrat, measures=list(gmean))
SAMPLE.NN    <- resample(learner = PRED.NN, task = task.highearner, resampling = resampleStrat, measures=list(gmean))
SAMPLE.KNN   <- resample(learner = PRED.KNN, task = task.highearner, resampling = resampleStrat, measures=list(gmean))
SAMPLE.SVM   <- resample(learner = PRED.SVM, task = task.highearner, resampling = resampleStrat, measures=list(gmean))

# Output 
SAMPLE.Tree
SAMPLE.Logit
SAMPLE.NN
SAMPLE.KNN
SAMPLE.SVM

# Modeling
FINAL.Tree  <- train(learner = PRED.Trees, task = task.highearner)
FINAL.Logit <- train(learner = PRED.Logit, task = task.highearner)
FINAL.NN    <- train(learner = PRED.NN, task = task.highearner)
FINAL.KNN   <- train(learner = PRED.KNN, task = task.highearner)
FINAL.SVM   <- train(learner = PRED.SVM, task = task.highearner)
FINAL.NB    <- train(learner = nb, task = task.highearner)

# Predict 
RESULTS.Tree  <- predict(FINAL.Tree, newdata = income.test)
RESULTS.Logit <- predict(FINAL.Logit, newdata = income.test)
RESULTS.NN    <- predict(FINAL.NN, newdata = income.test)
RESULTS.KNN   <- predict(FINAL.KNN, newdata = income.test)
RESULTS.SVM   <- predict(FINAL.SVM, newdata = income.test)
RESULTS.NB    <- predict(FINAL.NB, newdata = income.train)

# Outut
RESULTS.Tree
RESULTS.Logit
RESULTS.NN
RESULTS.KNN
RESULTS.SVM
RESULTS.NB

# Out of sample f1 and gmean 
performance(RESULTS.Tree, measures = list(f1, gmean))
performance(RESULTS.Logit, measures = list(f1, gmean))
performance(RESULTS.NN, measures = list(f1, gmean))
performance(RESULTS.KNN, measures = list(f1, gmean))
performance(RESULTS.SVM, measures = list(f1, gmean))
performance(RESULTS.NB, measures = list(f1, gmean))


