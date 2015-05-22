g = 9.8 ## meters per second
h0 = 56.67
v0 = 0
n = 25
tt = seq(0,3.4,len=n) ##time in secs, t is a base function
y = h0 + v0 *tt - 0.5* g*tt^2 + rnorm(n,sd=1)

plot(tt, y)

# Now we act as if we didn't know h0, v0 and -0.5*g and use regression to estimate these. 
# We can rewrite the model as y = b0 + b1 t + b2 t^2 + e and obtain the LSE we have used in
# this class. Note that g = -2 b2. To obtain the LSE in R we could write:

X = cbind(1,tt,tt^2)
A = solve(crossprod(X))%*%t(X)

# Given how we have defined A, which of the following is the LSE of g, the acceleration due to gravity ?
-2 * (A %*% y)[3]

# Use the code above in conjunction with the function replicate() to generate 100,000
# Monte Carlo simulated datasets. For each dataset compute an estimate of g
# (remember to multiply by -2). What is the standard error of this estimate?

estimates <- replicate(100000, {
    y = h0 + v0 *tt - 0.5 * g * tt^2 + rnorm(n,sd=1)
    coefs = solve(crossprod(X)) %*% crossprod(X, y)
    -2 * coefs[3]
})

mean(estimates)
sd(estimates)

# In the father and son height examples we have randomness because we have a random sample of
# father and son pairs. For the sake of illustration let's assume that this is the entire population:

library(UsingR)
x = father.son$fheight
y = father.son$sheight
n = length(y)

# Now let's run a Monte Carlo simulation in which we take a sample of size 50 over and over again.
# What is the standard error of the slope estimate?

N = 50

slopes <- replicate(10000, {
    index = sample(n,N)
    sampledat = father.son[index,]
    x = sampledat$fheight
    y = sampledat$sheight
    betahat = lm(y~x)$coef
    betahat[2]
})

mean(slopes)
sd(slopes)

# Which of the following is closest to the covariance between father heights and son heights?
cov(father.son$fheight, father.son$sheight)

# You can make a design matrix X for a two group comparison either using model.matrix or simply with:

nx = 5
ny = 7
X = cbind(rep(1,nx + ny),rep(c(0,1),c(nx, ny)))

# For a comparison of two groups, where the first group has nx=5 samples, and
# the second group has ny=7 samples, what is the element in the 1st row and 1st
# column of X^T X?

crossprod(X)

# What are all the other elements of (X^t X)?
# 7

