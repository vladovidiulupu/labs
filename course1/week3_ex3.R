########################## Inference III Assignment

babies = read.table("babies.txt", header=TRUE)

bwt.nonsmoke = babies$bwt[babies$smoke==0]

pop.var = var(bwt.nonsmoke)

# How often (what proportion of simulations) is the sample variance greater than 1.5 
# times the population variance? Use 1000 simulations.

sample.vars <- replicate(1000, var(sample(bwt.nonsmoke, 10)))

hist(sample.vars)
abline(v = pop.var, lwd = 2, col = "red")

mean(sample.vars > (1.5 * pop.var))

# Now use a sample size of 50. How often (what proportion) is the sample variance 
# larger than 1.5 times the population variance?

sample.vars <- replicate(1000, var(sample(bwt.nonsmoke, 50)))

hist(sample.vars)
abline(v = pop.var, lwd = 2, col = "red")

mean(sample.vars > (1.5 * pop.var))

# Plot of sample variance and population variance

sample.size = 2:400
var.estimate = sapply(sample.size, function(n) var(sample(bwt.nonsmoke, n)))
plot(sample.size, var.estimate)
abline(h=pop.var, col="blue")


# Let's take 50 samples from each of the two groups, and suppose that we want 
# to use a permutation test to compare these:

set.seed(0)
N <- 50
smokers <- sample(babies$bwt[babies$smoke==1], N)
nonsmokers <- sample(babies$bwt[babies$smoke==0], N)

obs <- mean(smokers) - mean(nonsmokers)

avgdiff <- replicate(1000, {
    all <- sample(c(smokers,nonsmokers))
    smokersstar <- all[1:N]
    nonsmokersstar <- all[(N+1):(2*N)]
    return(mean(smokersstar) - mean(nonsmokersstar))
})

mean(abs(avgdiff) > abs(obs))

# Use a permutation test with 1000 replications to generate a p-value for the 
# observed difference in medians. What is the p-value for the two groups of 50 defined above?

obs <- median(smokers) - median(nonsmokers)

avgdiff <- replicate(1000, {
    all <- sample(c(smokers,nonsmokers))
    smokersstar <- all[1:N]
    nonsmokersstar <- all[(N+1):(2*N)]
    return(median(smokersstar) - median(nonsmokersstar))
})

mean(abs(avgdiff) >= abs(obs))