##################### Probability Distributions Assessment

#library(devtools)
#install_github("jennybc/gapminder")

library(gapminder)
data(gapminder)
head(gapminder)

plot(ecdf(gapminder$lifeExp))

prop <- function(q) {
    mean(gapminder$lifeExp <= q)
}

qs <- seq(from=min(gapminder$lifeExp), to=max(gapminder$lifeExp), length=40)
props <- sapply(qs, prop)
plot(qs, props, type = "l", col = "red")

# What is the proportion of countries in 1952 that have a life expectancy less than or equal to 40?
gapminder1952 <- subset(gapminder, year == 1952)
mean(gapminder1952$lifeExp <= 40)

# What is the proportion of countries in 1952 that have a life expectancy between 40 and 60 years?
mean(gapminder1952$lifeExp <= 60) - mean(gapminder1952$lifeExp <= 40)

######################## The Normal Distribution Assessment

hist(gapminder1952$pop)
hist(log10(gapminder1952$pop))

#What is the standard deviation of the log10 of population size of the countries in 1952?
sd(log10(gapminder1952$pop))

x <- log10(gapminder1952$pop)
qqnorm(x)

z <- (x - mean(x)) / sd(x)
qqnorm(z)
abline(0, 1)

# What is the z-score of the country with the largest population size?
max(z)

F <- function(q) pnorm(q, mean=mean(x), sd=sd(x))
n <- length(x)

# Finally, using the Normal approximation, estimate the number of countries that should 
# have a log10 1952 population between 6 and 7 
(F(7) - F(6)) * n

# What is the quantile of the standard normal distribution which matches to the smallest number in x?
qnorm(0.5/n)

ps = ((1:n) - 0.5)/n
plot(qnorm(ps), sort(x))
