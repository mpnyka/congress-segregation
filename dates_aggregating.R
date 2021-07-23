library(tidyverse)
library(lubridate)
setwd("~/Desktop/Sociology/RA/Data")

d_full <- read_csv("d_full.csv")

## Correcting the dates for those who work in the member's offices
d_dates_member <- d_full %>% 
  dplyr::select(core_person_id, member_id, salary_title_id, start_date, end_date) %>% 
  filter(!is.na(member_id)) %>%
  pivot_longer(c(start_date, end_date), names_to = "date_period", values_to="date") %>% 
  arrange(date) %>%
  mutate(year = as.numeric(format(date, "%Y"))) %>% 
  group_by(core_person_id, year, member_id, salary_title_id) %>% 
  mutate(year2 = year)

people_errors <- d_dates_member %>% 
  filter(first(date_period)=="end_date") 

people_start_fixed <- people_errors %>%
  group_modify(~ add_row(.x, date_period="start_date_", 
                         date= ymd(as.numeric(paste(.$year2, "01","01", sep = ""))), 
                         .before = 1))

people_errors2 <- d_dates_member %>% 
  filter(last(date_period)=="start_date") 

people_end_fixed <- people_errors2 %>%
  group_modify(~ add_row(.x, 
                         date_period="end_date_", 
                         date= ymd(as.numeric(paste(.$year2, "12","31", sep = "")))))


d_dates_member2<- d_dates_member %>% 
  full_join(people_start_fixed) %>% 
  full_join(people_end_fixed)

d_dates_final_m2 <- d_dates_member2 %>% 
  arrange(date) %>% distinct() %>% 
  mutate(days = c(0, diff(date, lag =1))) %>%
  mutate(non_continuous = ifelse(grepl("start", date_period) & days > 1, 1, 0)) %>% 
  group_by (core_person_id, member_id, year, salary_title_id, non_continuous) %>%
  summarise(total_days = sum(days)) %>% 
  filter(non_continuous == 0) %>% 
  dplyr::select(-non_continuous)

write_csv(d_dates_final_m2, "dates_members.csv")

## correct dates for all congress (including non-member offices)

d_dates <- d_full %>% 
  dplyr::select(core_person_id, office_id, salary_title_id, start_date, end_date) %>% 
  pivot_longer(c(start_date, end_date), names_to = "date_period", values_to="date") %>% 
  arrange(date) %>%
  mutate(year = as.numeric(format(date, "%Y"))) %>% 
  group_by(core_person_id, year, office_id, salary_title_id) %>% 
  mutate(year2 = year)

people_errors_full <- d_dates %>% 
  filter(first(date_period) == "end_date") %>%
  filter(row_number() == 1)

people_start_fixed_full <- people_errors_full %>%
  group_modify(~ add_row(.x, date_period="start_date_", 
                         date = ymd(as.numeric(paste(.$year2, "01","01", sep = ""))), 
                         .before = 1))

people_errors2_full <- d_dates %>% 
  filter(last(date_period)=="start_date") %>%
  filter(row_number() == n())

people_end_fixed_full <- people_errors2_full %>%
  group_modify(~ add_row(.x, 
                         date_period="end_date_", 
                         date= ymd(as.numeric(paste(.$year2, "12","31", sep = "")))))


d_dates2<- d_dates %>% 
  full_join(people_start_fixed_full) %>% 
  full_join(people_end_fixed_full)

d_dates_final2 <- d_dates2 %>% 
  arrange(date) %>% distinct() %>% 
  mutate(days = c(0, diff(date, lag = 1))) %>%
  mutate(non_continuous = ifelse(grepl("start", date_period) & days > 1, 1, 0)) %>% 
  group_by (core_person_id, office_id, year, salary_title_id, non_continuous) %>%
  summarise(total_days = sum(days)) %>% 
  filter(non_continuous == 0) %>% 
  dplyr::select(-non_continuous)

write_csv(d_dates_final2, "dates_congress.csv")

  
# function to add end dates at the end and beginning of the year for periods that span over time. 
     
add_date_fn <- function(x){
  yr = x$year2[1]
  if (last(x$date_period) == "start_date" & first(x$date_period) == "end_date"){
    x2 <- add_row(x, 
            date_period = "end_date_", 
            date = ymd(as.numeric(paste(yr,12,31, sep=""))))
    x3 <- add_row(x2, 
                  date_period = "start_date_", 
                  date = ymd(as.numeric(paste(yr,"01","01", sep=""))),
                  .before = 1)
  } else if (first(x$date_period) == "end_date"){
    x3 <- add_row(x, 
                  date_period = "start_date_", 
                  date = ymd(as.numeric(paste(yr,"01","01", sep=""))),
                  .before = 1)
  } else if (last(x$date_period) == "start_date"){
    x3 <- add_row(x, 
                  date_period = "end_date_", 
                  date = ymd(as.numeric(paste(yr,12,30, sep=""))))
  } else {
      x3 = x
    }
  return(x3)
}                                                                                                                                                                                                
                   
add_end_date_fn <- function(x){
  year = x$year2[1]
  paste(year,12,31, sep="")
  print(year)
  if(last(x$date_period) == "start_date"){
      print("we need end row")
      x2 <- add_row(x, 
                  date_period = "end_date_", 
                  date = ymd(as.numeric(paste(year,12,31, sep=""))))
    } else {
    x2 = x
  }
  return(x2)
}        

add_start_date_fn <- function(x){
  year = x$year2[1]
  paste(year,01,01, sep="")
  if(first(x$date_period) == "end_date"){
    print("we need end row")
    x2 <- add_row(x, 
                  date_period = "start_date_", 
                  date = ymd(as.numeric(paste(year,01,01, sep=""))),
                  .before = 1)
  } else {
    x2 = x
  }
  return(x2)
}


### salary titles
salary_title_group_new <- read_csv("salary_title_group_2020-06-15.csv")

d_full %>% select(core_person_id, member_id, salary_title_id) %>% 
  filter(!is.na(member_id)) %>% 
  left_join(salary_title_group_new)

repeated <- salary_title_group_new %>% group_by(salary_title_id) %>% summarise (n=n()) %>% filter(n>1)
title_group <- read_csv("title_group_2020-06-15.csv")
salary_title <- read_delim("columbia/salary_title.csv", ";")

salary_title_group_new %>% filter(salary_title_id %in% repeated$salary_title_id) %>% 
  arrange (salary_title_id) %>% left_join(title_group) %>% left_join(salary_title) %>% 
  write_csv("salary_title_doubles.csv")
