library(tidyverse)
library(broom)

votes <- readRDS("votes.rds")
votes$year = votes$session + 1945
countrycode <- read.csv("COW country codes.csv")
votes_processed <- inner_join(votes, countrycode, by = c("ccode" = "CCode"))
colnames(votes_processed)[colnames(votes_processed) == 'StateNme'] <- 'Country'

votes_processed %>%
  group_by(year) %>%
  summarize(total = n(),
            percent_yes = mean(vote == 1))

