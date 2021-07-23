setwd("~/Desktop/Sociology/RA/Data")

library(tidyverse)
library(wesanderson)
library(lubridate)

staff_dates <- read_csv("staff_dates.csv")
race_staffer <- read_csv("race_staffer.csv") %>% dplyr::select(-X1)
member_full <- read_csv("member_full.csv")
CPI <- read_csv("cpi2.csv") %>% 
  mutate(year = as.numeric(format(DATE, "%Y")),
         cpi = CPIAUCSL_NBD20130101/100) %>%
  dplyr::select(year, cpi)
molly_data <- read_csv("staff_rank_data.csv")

## getting core_person and education data

ed_data <- molly_data %>% 
  dplyr::select(core_person_id, degreeinfo, grad_degree, jdphd, eliteuniv) %>%
  distinct()

d_salaries_year <- d %>% 
  dplyr::select(core_person_id, member_office_id, salary, start_date, end_date) %>% 
  mutate(year = as.numeric(format(end_date, "%Y"))) %>%
  group_by(core_person_id, year) %>%
  summarise(salary_year = sum(salary, na.rm=T))

staff_salaries <- staff_dates %>% left_join(d_salaries_year) %>% 
  filter(year > 2000) %>%
  filter(salary_year > 0) %>%
  mutate(yearly_salary = salary_year / total_days * 365) %>%
  left_join(race_staffer) %>%
  left_join(member_full) %>%
  mutate(black = ifelse(race == "nh_black", 1, 0),
         white = ifelse(race == "nh_white", 1, 0),
         asian = ifelse(race == "asian", 1, 0),
         hispanic = ifelse(race == "hispanic", 1, 0)) %>%
  left_join(CPI) %>%
  mutate(salary_adj = yearly_salary * cpi)  %>%
  mutate(gender = fct_relevel(gender, "M")) %>%
  left_join(ed_data) %>%
  left_join(dates_member_extra)

fit_house <- felm(salary_adj ~  black + hispanic + asian + rank 
           + gender + total_days + party_id + race_member + gender_member | year, 
           data = subset(staff_salaries, office_type_id == "HM"))

fit_senate <- felm(salary_adj ~  black + hispanic + asian + rank 
                + gender + total_days + party_id + race_member + gender_member 
                | year, 
                data = subset(staff_salaries, office_type_id == "SM"))

stargazer(fit_house, fit_senate, type="text")

### Discarding those who appear in the first year to do total days and hill experience

staffers_2000 <- staff_salaries %>% 
  filter(year==2001) %>% 
  dplyr::select(core_person_id) %>% 
  distinct()

staff_lean <- staff_salaries %>% filter(!core_person_id %in% staffers_2000$core_person_id) %>% 
  group_by(core_person_id) %>% 
  arrange(year, core_person_id) %>%
  mutate(hill_experience = 1:n()) %>%
  ungroup() %>%
  group_by(core_person_id, member_office_id) %>%
  mutate(totdays = sum(total_days))

staff_ed <- staff_lean %>% filter(degreeinfo == 1)

fit_house2 <- lm(salary_adj ~  black + hispanic + asian + rank
                + gender + totdays + hill_experience + party_id + race_member + gender_member + as.factor(year), 
                data = subset(staff_lean, office_type_id == "HM"))

fit_house2_fe <- felm(salary_adj ~  black + hispanic + asian + rank
                 + gender + totdays + hill_experience + party_id + race_member + gender_member 
                 | year, 
                 data = subset(staff_lean, office_type_id == "HM"))

fit_house3 <- felm(salary_adj ~  black + hispanic + asian + rank
                      + gender + totdays + hill_experience + grad_degree + jdphd
                   + party_id + race_member + gender_member 
                      | year, 
                      data = subset(staff_lean, office_type_id == "HM"))

fit_senate2 <- felm(salary_adj ~  black + hispanic + asian + rank
                 + gender + totdays + hill_experience + party_id + race_member 
                 + gender_member | year, 
                 data = subset(staff_lean, office_type_id == "SM"))

fit_senate3 <- felm(salary_adj ~  black + hispanic + asian + rank
                   + gender + totdays + hill_experience + grad_degree + jdphd
                   + party_id + race_member + gender_member 
                   | year, 
                   data = subset(staff_lean, office_type_id == "SM"))

stargazer(fit_house2, fit_senate2, type="text")

fit_house_fe <- felm(salary_adj ~  black + hispanic + asian + rank
                 + gender + totdays + hill_experience | member_office_id + year, 
                 data = subset(staff_lean, office_type_id == "HM"))

fit_senate_fe <- felm(salary_adj ~  black + hispanic + asian + rank
                  + gender + totdays + hill_experience | member_office_id + year, 
                  data = subset(staff_lean, office_type_id == "SM"))

stargazer(fit_house_fe, fit_senate_fe, type="text")

stargazer(title = "Compensation differences for the House", 
          fit_house, fit_house2_fe, fit_house3, fit_house_fe, 
          type="html", 
          dep.var.labels = "Annual salary (in 2013 dollars)", out = "house.htm")

stargazer(title = "Compensation differences for the Senate", 
          fit_senate, fit_senate2, fit_senate3, fit_senate_fe, type="html", 
          dep.var.labels = "Annual salary (in 2013 dollars)", out= "senate.htm")

## Adding part-time and nondc

fit_house_extra <- felm(salary_adj ~  black + hispanic + asian + rank + ptime + nondc
                  + gender + total_days + party_id + race_member + gender_member | year, 
                  data = subset(staff_salaries, office_type_id == "HM"))

fit_senate_extra <- felm(salary_adj ~  black + hispanic + asian + rank + ptime + nondc
                   + gender + total_days + party_id + race_member + gender_member 
                   | year, 
                   data = subset(staff_salaries, office_type_id == "SM"))


fit_house2_extra <- felm(salary_adj ~  black + hispanic + asian + rank + ptime + nondc
                 + gender + totdays + hill_experience + party_id + race_member + gender_member 
                 | year, , 
                 data = subset(staff_lean, office_type_id == "HM"))


fit_house3_extra <- felm(salary_adj ~  black + hispanic + asian + rank + ptime + nondc
                   + gender + totdays + hill_experience + grad_degree + jdphd
                   + party_id + race_member + gender_member 
                   | year, 
                   data = subset(staff_lean, office_type_id == "HM"))

fit_senate2_extra <- felm(salary_adj ~  black + hispanic + asian + rank + ptime + nondc
                    + gender + totdays + hill_experience + party_id + race_member 
                    + gender_member | year, 
                    data = subset(staff_lean, office_type_id == "SM"))

fit_senate3_extra <- felm(salary_adj ~  black + hispanic + asian + rank + ptime + nondc
                    + gender + totdays + hill_experience + grad_degree + jdphd
                    + party_id + race_member + gender_member 
                    | year, 
                    data = subset(staff_lean, office_type_id == "SM"))

fit_house_fe_extra <- felm(salary_adj ~  black + hispanic + asian + rank
                     + gender + totdays + hill_experience | member_office_id + year, 
                     data = subset(staff_lean, office_type_id == "HM"))

fit_senate_fe_extra <- felm(salary_adj ~  black + hispanic + asian + rank
                      + gender + totdays + hill_experience | member_office_id + year, 
                      data = subset(staff_lean, office_type_id == "SM"))

stargazer(title = "Compensation differences for the House", 
          fit_house_extra, fit_house2_extra, fit_house3_extra, fit_house_fe_extra, 
          type="html", 
          dep.var.labels = "Annual salary (in 2013 dollars)", out = "house_extra.htm")

stargazer(title = "Compensation differences for the Senate", 
          fit_senate_extra, fit_senate2_extra, fit_senate3_extra, fit_senate_fe_extra, type="html", 
          dep.var.labels = "Annual salary (in 2013 dollars)", out= "senate_Extra.htm")


staff_salaries %>% filter(office_type_id == "SM") %>% 
  filter(!is.na(rank)) %>%
  group_by(rank) %>% 
  summarise_at(c("asian", "hispanic", "nh_black", "nh_white"), mean) %>% 
  mutate_if(is.numeric, round, 2)
  
  