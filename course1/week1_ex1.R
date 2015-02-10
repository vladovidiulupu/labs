library(dplyr)

######################## R Basics Assessment

tab <- read.csv("../dagdata/inst/extdata/msleep_ggplot2.csv")  

class(tab)
dim(tab)
str(tab)

plot(tab$brainwt, tab$sleep_total)
plot(tab$brainwt, tab$sleep_total, log = "x")

sort(tab$sleep_total)

# How many hours of total sleep are there for the first animal in the table?
tab$sleep_total[1]

# What is the 3rd quartile of the total sleep of all the animals?
summary(tab$sleep_total)

# What is the average total sleep, using the function mean() and vector subsetting, 
# for the animals with total sleep greater than 18 hours?
mean(tab$sleep_total[tab$sleep_total > 18])

####################### Useful Vector Operations

# What is the row number of the animal which has more than 18 hours of total sleep 
# and less than 3 hours of REM sleep?
which(tab$sleep_total > 18 & tab$sleep_rem < 3)

# What is the index of the animal (so the row number) with the least total sleep?
order(tab$sleep_total)[1]

# What's the rank of the animal in the first row of the table in terms of total sleep?
rank(tab$sleep_total)[1]

# What is the row number for "Cotton rat" in the tab dataframe?
match("Cotton rat", tab$name)

####################### Factors

# How many rodents (Rodentia) are in the table?
table(tab$order)["Rodentia"]

# What is the mean hours of total sleep of the rodents?
tab %>% 
    group_by(order) %>% 
    summarize(mean_sleep_total = mean(sleep_total)) %>%
    filter(order == "Rodentia")

# What is the standard deviation of total hours of sleep for the Primates Order?
tab %>% 
    group_by(order) %>% 
    summarize(sd_sleep_total = sd(sleep_total)) %>%
    filter(order == "Primates")