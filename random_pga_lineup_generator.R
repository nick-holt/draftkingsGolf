# PGA Championship DraftKings.com Lineup Generator
# August, 7th, 2017
#
# Used script to generate 20 random lineups optimized for
# maximizing the use of draft budget

library(tidyverse)
library(stringr)
library(XML)

salary <- read_csv("pgachamp_salaries.csv") %>% 
        select(Name, Salary, AvgPointsPerGame)

colnames(salary) <- c("name", "salary", "ppg")

# function to build a random lineup
draft <- function(df = salary) {
        min_sal <- min(df$salary)
        target <- NULL
        budget <- 50000
        df$rank <- abs(rank(df$salary)-max(rank(df$salary)+1))
        top10 <- df %>%
                filter(rank <= 10)
        top25 <- df %>%
                filter(rank <= 25)
        one <- top10[sample(1:10, 1),]
        top25 <- top25 %>%
                filter(name != one$name)
        two <- top25[sample(1:24, 1),]
        target <- rbind(one, two)
        budget <- 50000 - sum(target$salary)
        remppl <- budget / 4
        remaining <- df %>%
                filter(salary <= remppl)
        three <- remaining[sample(1:length(remaining$salary),1),]
        target <- rbind(target, three)
        budget <- 50000 - sum(target$salary)
        remppl <- budget / 3
        remaining <- df %>%
                filter(salary <= remppl & name != target$name)
        four <- remaining[sample(1:length(remaining$salary),1),]
        target <- rbind(target, four)
        budget <- 50000 - sum(target$salary)
        remppl <- budget / 2
        top10 <- top10 %>%
                filter(name != target$name)
        top25 <- top25 %>%
                filter(name != target$name)
        remaining <- df %>%
                filter(salary <= remppl & name != target$name)
        if(budget - min_sal > max(top10$salary)){
                five <- top10[sample(1:9, 1),]
        } else if(budget - min_sal > max(top25$salary)){
                five <- top25[sample(1:23, 1),]
        } else {
                five <- remaining[sample(1:length(remaining$salary), 1),]
        }
        target <- rbind(target, five)
        budget <- 50000 - sum(target$salary)
        remaining <- df %>%
                filter(salary <= budget) %>%
                filter(rank <= min(rank) + 5) %>%
                filter(name != target$name)
        six <- remaining[sample(1:length(remaining$salary), 1),]
        target <- rbind(target, six)
        remainder <- 50000 - sum(target$salary)
        target$rem_budget <- remainder
        return(target)
}

draft()



