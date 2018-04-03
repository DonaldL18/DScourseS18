#Problem 4
# install.packages("nloptr)
library(nloptr)
library(dplyr)
library(stargazer)
####################################### Problem 4 ########################################
# Generatedata set
set.seed(100)

#Setting Variables
N<- 100000
K<- 10

# Random Number Generation for matrix, X
X<- matrix(rnorm(N*K,mean=0,sd=1), N, K)

# First matrix column with 1's
X [,1] <- 1

# Vector Length of N
eps<- rnorm(N, mean = 0, sd = 0.5)

glimpse(eps)

beta<- c(1.5, -1, 0.25, 0.75, 3.5, -2, 0.5, 1, 1.25, 2)

Y<- X%*%beta + eps

glimpse(Y)
###################################### Problem 5 #########################################


a<- (t(X)%*%X)
b<- solve(a)
c<- (t(X))
d<- b%*%c%*%Y

print(d)


# The Estimate is almost exact
##################################### Problem 6 #########################################
#
# Worked with Alex on number 6 and 7 

# Step size
stepsize<- .0000003

# number of iterations
maxiter<- 500

# Objective function
objfunc<- function(beta, Y, X) {
  return ( sum((Y-X%*%beta)^2))
}

# define the gradient function f(x)
gradient<- function(beta, Y, X) {
  return ( as.vector(-2*t(X)%*%(Y-X%*%beta)) )
}

# initial values
beta<- runif(dim(X)[2])

# create a vector to contain all beta
beta_all<- matrix("numeric",length(beta),maxiter)

# Using gradient descent to find the local minimum
iter<- 1
beta0<- 0*beta

while (norm(as.matrix(beta0)-as.matrix(beta))>1e-8) {
  beta0 <- beta
  beta <- beta0 - stepsize*gradient(beta0,Y,X)
  beta_all[,iter] <- beta
  if (iter%%10000==0) {
    print(beta)
  }
  iter <- iter+1
}

# print results and plot all X's for everY iteration
print(iter)
print(paste("The minimum of f(beta,Y,X) is ", beta, sep = ""))

###################################### Problem 7 ########################################

# Problem 7
# Worked with Alex on number 6 and 7 
# Objective function #7 
ofunction.1<- function(beta,Y,X) {
  return (sum((Y-X%*%beta)^2))

}

# Gradient of our objective function
gradient<- function(beta,Y,X) {
  return ( as.vector(-2*t(X)%*%(Y-X%*%beta)) )
}

# initial values
beta0<- runif(dim(X)[2]) #start at uniform random numbers equal to number of coefficients

# Algorithm parameters
options<- list("algorithm"="NLOPT_LD_LBFGS","xtol_rel"=1.0e-6,"maxeval"=1e3)

# Optimize
result<- nloptr( x0=beta0,eval_f=ofuncton.1,eval_grad_f=gradient,opts=options,Y=Y,X=X)
print(result)



# The values are nearly the same 
##################################### Problem 8 #########################################
#
# Objective function
ofunction.2<-function(theta,Y,X) {
  
  # need to slice our parameter vector into beta and sigma components
  beta<- theta[1:(length(theta)-1)]
  sigma<- theta[length(theta)]
  # write objective function as *negative* log likelihood (since NLOPT minimizes)
  loglike <- -sum( -.5*(log(2*pi*(sig^2)) + ((Y-X%*%beta)/sig)^2) ) 
  return (loglike)
}

# initial values
theta<- runif(dim(X)[2]+1) #start at uniform random numbers equal to number of coefficients


# Algorithm parameters
options<- list("algorithm"="NLOPT_LN_NELDERMEAD","xtol_rel"=1.0e-6,"maxeval"=1e4)

# Optimization
results<- nloptr( 
  x0=theta0,eval_f=ofunction.2,opts=options,Y=Y,X=X)

print(results)

betahat<- results$solution[1:(length(results$solution)-1)]
sigmahat<- results$solution[length(results$solution)]

##################################### Problem 9 #########################################
#
# Computing OLS with lm()
EOLS<- lm(Y~X -1)
EOLS

# Stargazer Table
stargazer(EOLS)
