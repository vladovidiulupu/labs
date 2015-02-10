################# Introduction to Random Variables Assessment

dat <- read.csv("../dagdata/inst/extdata/femaleMiceWeights.csv")

dat

control <- dat[1:12,]
highfat <- dat[13:24,]

mean(highfat$Bodyweight - control$Bodyweight)

stripchart(Bodyweight ~ Diet, data = dat, vertical = T, col = 1:2)
abline(h = tapply(dat$Bodyweight, dat$Diet, mean), col = 1:2)

# How many of the high fat mice weigh less than the mean of the control mice (chow)?
sum(highfat$Bodyweight < mean(control$Bodyweight))

# How many of the control mice weigh more than the mean of the high fat mice?
sum(control$Bodyweight > mean(highfat$Bodyweight))

# What is the proportion of high fat diet mice over 30?
mean(highfat$Bodyweight > 30)

################# Introduction to Random Variables II Assessment

population <- read.csv("../dagdata/inst/extdata/femaleControlsPopulation.csv")
population <- population[,1]

# What's the control population mean?
mean(population)

################# Introduction to the Null Distribution Assessment

sampleMeans <- replicate(10000, mean(sample(population, 12)))
hist(sampleMeans)

null <- replicate(10000, mean(sample(population, 12)) - mean(sample(population, 12)))
hist(null)

diff <- mean(highfat$Bodyweight - control$Bodyweight)

abline(v=diff, col="red")
abline(v=-diff, col="red")

# What is the one-tailed probability of seeing as big a difference as we observed, 
# calculated from your null distribution?
mean(null >= diff)

# What is the two-tailed probability of seeing as big a difference as we observed, 
# calculated from your null distribution?
mean(abs(null) >= diff)
