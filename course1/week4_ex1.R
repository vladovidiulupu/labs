######################## Exploratory Data Analysis I

# Given the above histogram: how many people are between the ages of 35 and 45?
# 9

load("skew.RData")

dim(dat)

par(mfrow = c(3,3))

for (i in 1:9) {
    qqnorm(dat[,i])
    qqline(dat[,i])
}

# Which column has positive skew (a long tail to the right)?
# 4

# Which column has negative skew (a long tail to the left)?
# 9

head(InsectSprays)

boxplot(split(InsectSprays$count, InsectSprays$spray))

boxplot(count ~ spray, data = InsectSprays)

# Which spray seems the most effective (has the lowest median)?
# C