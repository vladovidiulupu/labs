data(ChickWeight)

plot(ChickWeight$Time, ChickWeight$weight, col=ChickWeight$Diet)

chick = reshape(ChickWeight,idvar=c("Chick","Diet"),timevar="Time",direction="wide")
head(chick)

chick = na.omit(chick)

# We will focus on the chick weights on day 4 (check the column names of 'chick' and note the numbers).
# How much does the average of chick weights at day 4 increase if we add an outlier measurement 
# of 3000 grams? Specifically what is the average weight of the day 4 chicks including the outlier 
# chick divided by the average of the weight of the day 4 chicks without the outlier. 

weights <- chick$weight.4
mean(c(weights, 3000))/mean(weights)

# In the problem above we saw how sensitive the mean is to outliers. Now let's see what happens
# when we use the median instead of the mean.
# Compute the same ratio but now using median instead of the mean.

median(c(weights, 3000))/median(weights)

# Now try the same thing with the sample standard deviation, (the sd() function in R).
# Add a chick with weight 3000 gramsto the chick weights from day 4.How much does the
# standard deviation change?

sd(c(weights, 3000))/sd(weights)
mad(c(weights, 3000))/mad(weights)

# Calculate the Pearson correlation of the weights of chicks from day 4 and day 21.
# Now calculate how much the Pearson correlation changes if we add a chick that weighs
# 3000 on day4 and 3000 on day 21. Again, divide the Pearson correlation with the outlier
# chick over the Pearson correlation computed without the outliers.

weights4 <- chick$weight.4
weights21 <- chick$weight.21

plot(weights4, weights21)

cor(c(weights4, 3000), c(weights21, 3000)) / cor(weights4, weights21)

cor(c(weights4, 3000), c(weights21, 3000), method = "spearman") / 
    cor(weights4, weights21, method = "spearman")

# Save the weights of the chicks on day 4 from diet 1 as a vector 'x'. Save the weights of the
# chicks on day 4 from diet 4 as a vector 'y'. Now perform a t test comparing x and y (in R the 
# function t.test(x,y) will perform the test). Now, perform a Wilcoxon test of x and y
# (in R the function wilcox.test(x,y) will perform the test). 
# Now, perform a t-test of x and y, after adding a single chick of weight 200 grams to 'x'
# (the diet 1 chicks). What is the p-value from this test?

stripchart(chick$weight.4 ~ chick$Diet, method="jitter", vertical=TRUE)

x <- filter(chick, Diet == "1")$weight.4
y <- filter(chick, Diet == "4")$weight.4

stripchart(list(x, y), vertical = TRUE)

t.test(x, y)
wilcox.test(x, y)

testResult <- t.test(c(x, 200), y)
testResult

testResult <- wilcox.test(c(x, 200), y)
testResult


# What is the difference in t-test statistic (the statistic is obtained by t.test(x,y)$statistic)
# between adding 10 and adding 100 to all the values in the group 'y'? So take the the t-test
# statistic with x and y+10 and substract away the t-test statistic with x and y+100.
# (The value should be positive).

par(mfrow=c(1,3))

boxplot(x,y)

boxplot(x,y+10)

boxplot(x,y+100)

t10 <- t.test(x, y + 10)$statistic
t100 <- t.test(x, y + 100)$statistic

t10 - t100

w10 <- wilcox.test(x, y + 10)$statistic
w100 <- wilcox.test(x, y + 100)$statistic

w10 - w100
