library(dplyr)

msleep <- read.csv("msleep_ggplot2.csv")

msleep %>%
    mutate(prop_sleep = sleep_rem / sleep_total) %>%
    group_by(order) %>%
    summarise(median_prop = median(prop_sleep)) %>%
    arrange(median_prop) %>%
    head

