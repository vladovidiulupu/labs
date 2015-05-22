# Suppose we have an experiment with two species A and B, and two conditions: control and treated.

species <- factor(c("A","A","B","B"))
condition <- factor(c("control","treated","control","treated"))

X <- model.matrix(~ species + condition)
X

fit <- lm(I(1:nrow(X)) ~ species + condition)
fit

# What should the contrast vector be, for the contrast of (species=B and
# condition=control) vs (species=A and condition=treatment)? Assume that the
# beta vector from the model fit by R is: Intercept, speciesB, conditiontreated.

contrast(fit, list(species="B",condition="control"),list(species="A",condition="treated"))$X
# 0 1 -1

url <- "https://raw.githubusercontent.com/genomicsclass/dagdata/master/inst/extdata/spider_wolff_gorb_2013.csv"
filename <- "spider_wolff_gorb_2013.csv"
library(downloader)
if (!file.exists(filename)) download(url, filename)
spider <- read.csv(filename, skip=1)

fitTL <- lm(friction ~ type + leg, data=spider)
summary(fitTL)

# What is the t-value for the contrast of leg pair L4 vs leg pair L2?
fitTL <- lm(friction ~ type + leg, data=spider)
summary(fitTL)

contrast(fitTL, list(leg="L4", type="push"),list(leg="L2", type="push"))

# In the book page, we computed Sigma using:

X <- model.matrix(~ type + leg, data=spider)
(Sigma <- sum(fitTL$residuals^2)/(nrow(X) - ncol(X)) * solve(t(X) %*% X))

# Our contrast matrix is:
C <- matrix(c(0,0,-1,0,1),1,5)    

# Using Sigma, what is Cov(beta-hat_L4, beta-hat_L2)?
contrastVar = C %*% Sigma %*% t(C)
contrastVar
covL4L2 = (Sigma["legL4","legL4"] + Sigma["legL2","legL2"] - contrastVar) / 2
covL4L2

# Suppose that we notice that the within-group variances for the groups with
# smaller frictional coefficients are generally smaller, and so we try to apply
# a transformation to the frictional coefficients to make the within-group
# variances more constant.

# Add a new variable log2friction to the spider dataframe:
    
spider$log2friction <- log2(spider$friction)

# The 'Y' values now look like:
    
boxplot(log2friction ~ type*leg, data=spider)

fit <- lm(log2friction ~ type*leg, data=spider)
summary(fit)

# What is the t-value for the interaction of type push and leg L4? If this
# t-value is sufficiently large, we would reject the null hypothesis that the
# push vs pull effect on log2(friction) is the same in L4 as in L1.

# -3.689

# What is the F-value for all of the type:leg interaction terms, in an analysis
# of variance? If this value is sufficiently large, we would reject the null
# hypothesis that the push vs pull effect on log2(friction) is the same for all
# leg pairs.

fit2 <- lm(log2friction ~ type + leg, data=spider)
summary(fit2)

anova(fit2, fit)

anova(fit)

# What is the L2 vs L1 estimate in log2friction for the pull samples?
# 0.34681

summary(fit)

# What is the L2 vs L1 estimate in log2friction for the push samples? 
# 0.34681 + 0.09967

# In the video we briefly mentioned the analysis of variance (or ANOVA,
# performed in R using the anova() function), which allows us to test whether a
# number of coefficients are equal to zero, by comparing a linear model
# including these terms to a linear model where these terms are set to 0.

# The book page for this section has a section, "Testing all differences of
# differences", which explains the ANOVA concept and the F-test in some more
# detail. You can read over that section before or after the following question.

# In this last question, we will use Monte Carlo techniques to observe the
# distribution of the ANOVA's "F-value" under the null hypothesis, that there
# are no differences between groups.

# Suppose we have 4 groups, and 10 samples per group, so 40 samples overall:

N <- 40
p <- 4
group <- factor(rep(1:p,each=N/p))
X <- model.matrix(~ group)

# We will show here how to calculate the "F-value", and then we will use random
# number to observe the distribution of the F-value under the null hypothesis.

# The F-value is the mean sum of squares explained by the terms of interest (in
# our case, the 'group' terms) divided by the mean sum of squares of the
# residuals of a model including the terms of interest. So it is the explanatory
# power of the terms divided by the leftover variance.

# Intuitively, if this number is large, it means that the group variable
# explains a lot of the variance in the data, compared to the amount of variance
# left in the data after using group information. We will calculate these values
# exactly here:
    
# First generate some random, null data, where the mean is the same for all groups:

Y <- rnorm(N,mean=42,7)

# The base model we wil compare against is simply Y-hat = mean(Y), which we will
# call mu0, and the initial sum of squares is the Y values minus mu0:
    
mu0 <- mean(Y)
initial.ss <- sum((Y - mu0)^2)

# We then need to calculate the fitted values for each group, which is simply
# the mean of each group, and the residuals from this model, which we will call
# "after.group.ss" for the sum of squares after using the group information:
    
s <- split(Y, group)
after.group.ss <- sum(sapply(s, function(x) sum((x - mean(x))^2)))

# Then the explanatory power of the group variable is the initial sum of squares
# minus the residual sum of squares:
    
(group.ss <- initial.ss - after.group.ss)

# We calculate the mean of these values, but we divide by terms which remove the
# number of fitted parameters. For the group sum of squares, this is number of
# parameters used to fit the groups (3, because the intercept is in the initial
# model). For the after group sum of squares, this is the number of samples
# minus the number of parameters total (So N - 4, including the intercept).

group.ms <- group.ss / (p - 1)
after.group.ms <- after.group.ss / (N - p)

# The F-value is simply the ratio of these mean sum of squares.

f.value <- group.ms / after.group.ms

# What's the point of all these calculations? The point is that, after following
# these steps, the exact distribution of the F-value has a nice mathematical
# formula under the null hypothesis. We will see this below.

fVals <- replicate(1000, {
    Y <- rnorm(N,mean=42,7)
    mu0 <- mean(Y)
    
    initial.ss <- sum((Y - mu0)^2)
    
    s <- split(Y, group)
    after.group.ss <- sum(sapply(s, function(x) sum((x - mean(x))^2)))
    
    (group.ss <- initial.ss - after.group.ss)
    
    group.ms <- group.ss / (p - 1)
    after.group.ms <- after.group.ss / (N - p)
    
    f.value <- group.ms / after.group.ms
    f.value
})

hist(fVals)
mean(fVals)

# Plot the distribution of the 1000 F-values:

hist(fVals, col="grey", border="white", breaks=50, freq=FALSE)

# Overlay the theoretical F-distribution, with parameters df1=p - 1, df2=N - p.

xs <- seq(from=0,to=6,length=100)
lines(xs, df(xs, df1 = p - 1, df2 = N - p), col="red")
