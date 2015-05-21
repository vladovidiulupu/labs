######################## Exploratory Data Analysis II

library(UsingR)

plot(father.son$fheight, father.son$sheight)
identify(father.son$fheight, father.son$sheight)

plot(scale(x), scale(y))
abline(h=0, v=0)

# Calculate the average of (scaled x values times scaled y values)
mean(scale(x) * scale(y))

# correlation:
sum(scale(x) * scale(y)) / (nrow(father.son) - 1)

data(nym.2002)
str(nym.2002)

hist(nym.2002$time)

plot(nym.2002$age, nym.2002$time)

plot(nym.2002$time, nym.2002$place)

qqnorm(nym.2002$time)
qqline(nym.2002$time)

barplot(tail(sort(table(nym.2002$home)),10))

boxplot(time ~ gender, data = nym.2002)


time = sort(nym.2002$time)

# What is the fastest time divided the median time?
head(time, 1) / median(time)

# What is the slowest time divided the median time?
tail(time, 1) / median(time)

plot(time/median(time), ylim=c(1/4,4))
abline(h=c(1/2,1,2))

plot(log2(time/median(time)),ylim=c(-2,2))
abline(h=-1:1)

# When is it appropriate to use pie charts?
# Never

# The use of pseudo-3D plots in the literature mostly adds
# Confusion

# When is it appropirate to use a barplot
# To compare percentages that add up to 100%