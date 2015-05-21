#################### Inference I Assessment

babies <- read.table("../dagdata/inst/extdata/babies.txt", header = T)

bwt.nonsmoke <- babies$bwt[babies$smoke==0]
bwt.smoke <- babies$bwt[babies$smoke==1]

hist(bwt.nonsmoke)
hist(bwt.smoke)

mean(bwt.nonsmoke)-mean(bwt.smoke)
sd(bwt.nonsmoke)
sd(bwt.smoke)

# Compute the t-value (t-statistic) for the first 30 weights of non-smoking mothers and 
# the first 30 weights of smoking mothers. Confirm that the t-statistic calculated manually and 
# by t.test() is the same. What is the t-value (t-statistic)?
ns <- bwt.nonsmoke[1:30]
s <- bwt.smoke[1:30]

t.test(ns, s)

diff <- mean(ns) - mean(s)
se <- sqrt(var(ns) / length(ns) + var(s) / length(s))
t <- diff / se
pval <- 1 - pnorm(abs(t)) + pnorm(-abs(t))

# Because of the symmetry of the standard normal distribution, there is a simpler way to 
# calculate the probability that a t-value under the null could have a larger absolute value than tval.
2 * pnorm(-abs(t))