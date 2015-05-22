X <- matrix(c(1,1,1,1,0,0,1,1),nrow=4)
rownames(X) <- c("a","a","b","b")

beta <- c(5, 2)
X

X %*% beta

# What is the fitted value for the A samples? (The fitted Y values.)
# 5

# What is the fitted value for the B samples? (The fitted Y values.)
# 7

X <- matrix(c(1,1,1,1,1,1,0,0,1,1,0,0,0,0,0,0,1,1),nrow=6)
rownames(X) <- c("a","a","b","b","c","c")

beta <- c(10,3,-3)

X %*% beta

# What is the fitted value for the B samples?
# 13

# What is the fitted value for the C samples?
# 7

# In the previous assessment, we used a Monte Carlo technique to see that the
# linear model coefficients are random variables when the data is a random
# sample. Now we will use the matrix algebra from the previous video to try to
# estimate the standard error of the linear model coefficients. Again, take a
# random sample of the father.son heights data:
    
library(UsingR)
x = father.son$fheight
y = father.son$sheight
n = length(y)
N = 50

set.seed(1)
index = sample(n,N)
sampledat = father.son[index,]
x = sampledat$fheight
y = sampledat$sheight

betahat = lm(y~x)$coef

# First, we want to estimate sigma^2, the variance of Y. As we have seen in the
# previous unit, the random part of Y is only coming from epsilon, because we
# assume X*beta is fixed. So we can try to estimate the variance of the epsilons
# from the residuals, the Y_i minus the fitted values from the linear model.

# What is the sum of the squared residuals?

fit = lm(y ~ x)
fitted = fit$fitted.values
SSR = sum((y - fitted)^2)
SSR

# Our estimate of sigma^2 will be the sum of squared residuals divided by (N -
# p), the sample size minus the number of terms in the model. Since we have a
# sample of 50 and 2 terms in the model (an intercept and a slope), our estimate
# of sigma^2 will be the sum of squared residuals divided by 48. Save this to a
# variable 'sigma2':
    
sigma2 = SSR / 48

# Now calculate (X^T X)^-1, the inverse of X transpose times X. Use the solve()
# function for the inverse and t() for the transpose. What is the element in the
# first row, first column?

X = cbind(rep(1,N), x)

XtXinv = solve(crossprod(X))
XtXinv

# Now we are one step away from the standard error of beta-hat. Take the
# diagonals from the (X^T X)^-1 matrix above, using the diag() function. Now
# multiply our estimate of sigma^2 and the diagonals of this matrix. This is the
# estimated variance of beta-hat, so take the square root of this. You should
# end up with two numbers, the standard error for the intercept and the standard
# error for the slope.

stderrs = sqrt(sigma2 * diag(XtXinv))

#What is the standard error for the slope?
stderrs

# day:          A   B   C
# condition: --------------
# treated    |  2   2   2
# control    |  2   2   2

# Given the factors we have defined above, and not defining any new ones, which
# of the following R formula will produce a design matrix (model matrix) that
# let's us analyze the effect of condition, controlling for the different days:

# model.matrix(~ day + condition )