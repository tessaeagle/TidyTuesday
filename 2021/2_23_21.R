library(tidyverse)
library(gt)

earn <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2021/2021-02-23/earn.csv')

#code help from Thomas Mock's post: https://themockup.blog/posts/2020-05-16-gt-a-grammer-of-tables/

df <- earn %>%
  filter(sex != "Both Sexes" & race != "All Races") %>%
  select(1,2,5,8) %>%
  group_by(sex, race, year) %>%
  summarise(avMedian = mean(median_weekly_earn)) %>%
  spread(key = year, value = avMedian) #long to wide



df %>% 
  mutate(sex = str_to_title(sex)) %>% 
  group_by(sex) %>% 
  mutate(pct_change = round((`2020`/`2010`-1)*100,1)) %>% 
  gt(
    rowname_col = "race"
  ) %>%
  fmt_number(3:13) %>% 
  cols_label(
    pct_change = md("% Change")
  ) %>% 
  tab_style(
    style = list(
      cell_text(color = "#e0960d", weight = "bold")
    ),
    locations = cells_body(
      columns = vars(pct_change)
    )
  ) %>%
  summary_rows(
    groups = TRUE,
    columns = vars(`2010`, `2011`, `2012`,`2013`, `2014`, `2015`, `2016`,`2017`, `2018`, `2019`, `2020`), 
    fns = list(
      avg = ~mean(.)
     )
  ) %>%
  tab_options(
    summary_row.background.color = "#bbd9f2"
  ) %>%
  fmt_number(
    columns = 2:13, 
    decimals = 1 
  ) %>%
  tab_spanner(
    label = "Median Weekly Earnings ($USD)", 
    columns = 2:13
  ) %>%
  tab_style(
    style = list(
      cell_text(weight = "bold")
    ),
    locations = list(
      cells_column_labels(everything())
    )
  ) %>%
  tab_style(
    style = cell_text(color = "#068c87", weight = "bold"),
    locations = list(
      cells_row_groups()
    )
  ) %>%
  tab_source_note(md("**Table**: @tessuheagle | **Data**: US Bureau of Labor Statistics")) %>%
  tab_header(
    title = md("**US Median Weekly Earnings, 2010-2020**"),
    subtitle = html("<em>Broken down by sex and race, adjusted for current dollars. Percent change indicates change from 2010 to 2020.</em>")
  ) %>%
  tab_options(
    heading.subtitle.font.size = 16,
    heading.title.font.size = 22,
    heading.align = "center",
    table.border.top.color = "white", 
    column_labels.border.top.color = "black",
    column_labels.border.top.width = px(3),
    column_labels.border.bottom.width = px(2),
    column_labels.border.bottom.color = "black",
    row_group.border.top.color = "black",
    row_group.border.bottom.color = "black"
  ) 

#gtsave(table, "2_23_21.png")

