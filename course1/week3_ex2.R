#################### Inference II Assessment

babies <- read.table("../dagdata/inst/extdata/babies.txt", header = T)

bwt.nonsmoke <- babies$bwt[babies$smoke==0]
bwt.smoke <- babies$bwt[babies$smoke==1]


sample.nonsmoke <- sample(bwt.nonsmoke, 30)
sample.smoke <- sample(bwt.smoke, 30)

testResult <- t.test(sample.nonsmoke, sample.smoke)
testResult$p.value
testResult$conf.int

# What is the average length of the confidence interval?
intervals <- replicate(1000, simplify = TRUE, {
    sample.nonsmoke <- sample(bwt.nonsmoke, 30)
    sample.smoke <- sample(bwt.smoke, 30)
    
    testResult <- t.test(sample.nonsmoke, sample.smoke)
    testResult$conf.int
})

mean(intervals[2,] - intervals[1,])

# How often (what proportion of times) did the confidence intervals 
# contain the population-level difference?

popdiff <- mean(bwt.nonsmoke) - mean(bwt.smoke)

contains <- apply(intervals, 2, function(interval) {
    interval[1] <= popdiff & popdiff <= interval[2]
})

mean(contains)

# Fill in the blank: the difference in means (X.ns - X.s) must have absolute value 
# greater than 1.96 times sd.diff in order for the result to be significant (at alpha=0.05).

# What is the power at alpha=0.1?
N <- 15
alphas <- c(0.01, 0.05, 0.1)
powers <- sapply(alphas, function(alpha) {
    rejectNull <- replicate(1000, {
        sample.nonsmoke <- sample(bwt.nonsmoke, N)
        sample.smoke <- sample(bwt.smoke, N)
        t.test(sample.smoke, sample.nonsmoke)$p.value <= alpha
    })
    mean(rejectNull)
})

d = read.csv("assoctest.csv")

# Compute the Chi-square test for the association of genotype with case/control status.
# What is the X-squared statistic?

tab <- table(d)
tab

chisq.test(tab)

# Compute the Fisher's exact test ( fisher.test() ) for the same table. What is the p-value?
fisher.test(tab)
