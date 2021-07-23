setwd("~/Desktop/Sociology/RA/Data")

library(tidyverse)
library(wesanderson)
library(lubridate)

dates_members <- read_csv("dates_members.csv")
molly_dat <- read_csv("staff_rank_data.csv")
salary_title <- read_delim("columbia/salary_title.csv", ";")


d_congress <- d %>% filter(!is.na(member_office_id)) %>%
  mutate(congress = case_when(
                              start_date >= "2001-01-03" & end_date < "2003-01-03" ~ 107,
                              start_date >= "2003-01-03" & end_date < "2005-01-03" ~ 108,
                              start_date >= "2005-01-03" & end_date < "2007-01-03" ~ 109,
                              start_date >= "2007-01-03" & end_date < "2000-01-03" ~ 110,
                              start_date >= "2009-01-03" & end_date < "2011-01-03" ~ 111,
                              start_date >= "2011-01-03" & end_date < "2013-01-03" ~ 112,
                              start_date >= "2013-01-03" & end_date < "2015-01-03" ~ 113,
                              TRUE ~ NA_real_)) %>%
  group_by(congress, core_person_id, member_office_id, salary_title_id) %>%
  filter(!is.na(congress)) %>%
  summarise(salary_congress = sum(salary, na.rm=T)) 

## merging with molly there's duplicates. This is because in mollys data 
## some people (5000k?) hold multiple salary rank in a congress and same office. 
## This is actually not what they report in the paper?? but this is a problem for us. 

## Eliminating those with multiple ranks. 

only_once <- molly_dat %>% 
  dplyr::select(congress,member_office_id,core_person_id, rank) %>% 
  group_by(congress,member_office_id,core_person_id) %>% 
  summarise(n=n()) %>% filter(n==1) %>%
  dplyr::select(-n)

molly_dat_correction <- only_once %>% left_join(molly_dat)

## applying the same correction for our data:

only_once2 <- d_congress %>% 
  dplyr::select(congress, member_office_id, core_person_id, salary_title_id) %>% 
  group_by(congress,member_office_id,core_person_id) %>% 
  summarise(n = n()) %>% filter(n == 1) %>%
  dplyr::select(-n)

d_corrected <- only_once2 %>% left_join(d_congress)

d_merged <- d_corrected %>% left_join(molly_dat_correction) %>% filter(!is.na(rank))

d_salaries_correspondence <- d_merged %>% ungroup() %>%
  dplyr::select(salary_title_id, rank) %>% 
  left_join(salary_title) %>%
  distinct()

## checking if there's doubly-classified salary_title_ids (42 repeated out of 3k)

d_repeated <- d_salaries_correspondence %>% 
  group_by(salary_title_id) %>% 
  summarise(n = n()) %>% 
  filter(n>1)

d_salaries_correspondence2 <- d_salaries_correspondence %>% 
  filter(!salary_title_id %in% d_repeated$salary_title_id) 

d_salaries_correspondence2 %>% mutate(rank_name = case_when(rank == 1 ~ "Chief of Staff",
                                                            rank == 2 ~ "Deputy CoS",
                                                            rank == 3 ~ "Legislative Director",
                                                            rank == 4 ~ "Comm Dir/Pres. Sec.",
                                                            rank == 5 ~ "Legislative Ass.",
                                                            rank == 6 ~ "Policy & Press support",
                                                            rank == 7 ~ "Admin/District support")) %>%
  dplyr::select(-full_name) %>%
  write_csv("molly_correspondence.csv")

## creates csv with unclassified salary_title_id after molly (~1500 unclassified). 

dates_members %>% filter(total_days>=180) %>% 
  left_join(d_salaries_correspondence2) %>% filter(is.na(rank)) %>% 
  group_by(salary_title_id) %>% summarise (n=n()) %>%
  dplyr::select(salary_title_id) %>% left_join(salary_title) %>% dplyr::select(-full_name) %>%
  write_csv("unclassified_titles.csv")

## loading & merging james's classification + molly's :

classified_james <- read_csv("unclassified_titles_JRJ.csv") %>% 
  rename(rank = Rank) %>%
  mutate(rank_name = case_when(rank == 1 ~ "Chief of Staff",
                               rank == 2 ~ "Deputy CoS",
                               rank == 3 ~ "Legislative Director",
                               rank == 4 ~ "Comm Dir/Pres. Sec.",
                               rank == 5 ~ "Legislative Ass.",
                               rank == 6 ~ "Policy & Press support",
                               rank == 7 ~ "Admin/District support")) 
classified_molly <- read_csv("molly_correspondence.csv")

salary_classification_final <- bind_rows(classified_james, classified_molly)

dates_members %>% filter(total_days>=180) %>% 
  left_join(salary_classification_final) %>%
  write_csv("staff_dates.csv")


## Doing the same but with salary_title_group_id

salary_title_group <- read_delim("columbia/salary_title_group.csv", ";")

## figuring out repeated salary_title classifications.
repeated <- salary_title_group %>% group_by(salary_title_id) %>%
  summarise (n=n()) %>% filter(n>1)
title_group <- read_delim("columbia/title_group.csv", ";")
salary_title <- read_delim("columbia/salary_title.csv", ";")

## creates a csv with the double classification to choose (46 total). 
salary_title_group %>% filter(salary_title_id %in% repeated$salary_title_id) %>% 
  arrange (salary_title_id) %>% left_join(title_group) %>% left_join(salary_title) %>% 
  write_csv("salary_title_doubles.csv")

salary_title_group2 <- salary_title_group %>% 
  filter(!(salary_title_id %in% repeated$salary_title_id))

### 3500 unclassified ones. 
dates_members %>% filter(total_days>=180) %>% 
  left_join(salary_title_group2) %>% 
  filter(is.na(salary_title_group_id)) %>% 
  group_by(salary_title_id) %>% 
  summarise (n=n()) %>%
  dplyr::select(salary_title_id) %>% left_join(salary_title) %>%
  arrange(salary_title_id) %>% write_csv("salary_title_unclassified.csv")


### getting job-person-rank and nondc-ptime correspondence.

extra_info <- molly_dat_correction %>% 
  select(congress, core_person_id, member_office_id, rank, ptime, nondc)

d_merged2 <- d_corrected %>% left_join(extra_info)

d_extra_correspondence <- d_merged2 %>% ungroup() %>%
  dplyr::select(salary_title_id, ptime, nondc) %>% 
  left_join(salary_title) %>%
  select(salary_title_id, ptime, nondc) %>%
  distinct() %>%
  filter(!is.na(ptime)) %>%
  filter(!is.na(nondc))

d_repeated2 <- d_extra_correspondence %>% 
  group_by(salary_title_id) %>% 
  summarise(n = n()) %>% 
  filter(n > 1) 

d_extra_correspondence2 <- d_extra_correspondence %>%
  filter(!salary_title_id %in% d_repeated2$salary_title_id)

## open correspondence extra titles manually classified by Mireia

extra2 <- read_csv("extras2.csv") %>% 
  select(-original_name, -full_name) %>%
  distinct()

extra_final <- bind_rows(d_extra_correspondence2, extra2)



dates_member_extra <- dates_members %>% left_join(extra_final) %>% write_csv("dates_member_extra.csv")

