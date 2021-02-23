library(tidyverse)
library(gt)

tuesdata <- tidytuesdayR::tt_load(2020, week = 43)

beer <- tuesdata$beer_awards

beer %>%
  head() %>%
  gt()

by_state <- beer %>%
  group_by(state) %>%
  dplyr::count(state) %>%
  arrange(desc(n)) %>%
  ungroup()
top_10 <- by_state %>%
  slice_head(n = 10) %>%
  mutate(state = droplevels(state))

