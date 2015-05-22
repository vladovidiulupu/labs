# Which of the above design matrices does NOT have the problem of collinearity?
# 3

# Let's use the example from the lecture to visualize how there is not a single best beta-hat, when the design matrix has collinearity of columns.

# An example can be made with:

sex <- factor(rep(c("female","male"),each=4))
trt <- factor(c("A","A","B","B","C","C","D","D"))

# The model matrix can then be formed with:

X <- model.matrix( ~ sex + trt)

# And we can see that the number of independent columns is less than the number of columns of X:

qr(X)$rank

# Suppose we observe some outcome, Y. For simplicity we will use synthetic data:

Y <- 1:8

# Now, we will fix the value for two beta's and optimize the remaining betas. We
# will fix beta_male and beta_D. And then we will find the optimal value for the
# remaining betas, in terms of minimizing sum((Y - X beta)^2).

# The optimal value for the other betas is the one that minimizes:

# sum( ( (Y - X* beta_male - X** beta_D) - X*** beta*** )^2 )

# Where X* is the male column of the design matrix, X** is the D column, and
# X*** has the remaining columns.

# So all we need to do is redefine Y as Y* = Y - X* beta_male - X** beta_D and
# fit a linear model. The following line of code creates this  variable Y*,
# after fixing beta_male to a value 'a', and beta_D to a value, 'b':

makeYstar <- function(a,b) Y - X[,2] * a - X[,5] * b

# Now we'll construct a function which, for a given value a and b, gives us back
# the the sum of squared residuals after fitting the other terms.

fitTheRest <- function(a,b) {
Ystar <- makeYstar(a,b)
Xrest <- X[,-c(2,5)]
betarest <- solve(t(Xrest) %*% Xrest) %*% t(Xrest) %*% Ystar
residuals <- Ystar - Xrest %*% betarest
sum(residuals^2)
}

# What is the sum of squared residuals when the male coefficient is 1 and the D
# coefficient is 2, and the other coefficients are fit using the linear model
# solution?
fitTheRest(1, 2)

# We can apply our function fitTheRest to a grid of values for beta_male and
# beta_D, using the outer() function in R. outer() takes three arguments, a grid
# of values for the first argument, a grid of values for the second argument,
# and finally a function which takes two arguments.

# Try it out:
    
outer(1:3,1:3,`*`)

# We can run fitTheRest on a grid of values, using the following code (the
# Vectorize() is necessary as outer() requires only vectorized functions):
    
outer(-2:8,-2:8,Vectorize(fitTheRest))

# In the grid of values, what is the smallest sum of squared residuals?
# 2

library(rafalib)
imagemat(outer(-2:8,-2:8,Vectorize(fitTheRest)))

# We will use the spider dataset to try out the QR decomposition as a solution
# to linear models. Load the full spider dataset, by using the code in the
# Interactions and Contrasts book page. Run a linear model of the friction
# coefficient with two variable (no interactions):
    
fit <- lm(friction ~ type + leg, data=spider)

# The solution we are interested in solving is:
    
betahat <- coef(fit)

# So for our matrix work, 

Y <- matrix(spider$friction, ncol=1)

X <- model.matrix(~ type + leg, data=spider)

# In the material on QR decomposition, we saw that the solution for beta is:
# R beta = Q^T Y

qrx <- qr(X)
Q <- qr.Q(qrx)
R <- qr.R(qrx)

# What is the first row, first column element in the Q matrix for this linear model?
Q[1, 1]

# What is the first row, first column element in the R matrix for this linear model?
R[1, 1]

# What is the first row, first column element of Q^T Y (use crossprod() as in the book page)
crossprod(Q, Y)
