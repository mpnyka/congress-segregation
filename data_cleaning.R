setwd("~/Desktop/Sociology/RA/Data")
library(tidyverse)

library(wesanderson)
library(lubridate)

## Personnel_compensation is the main datafile

d <- read_delim("columbia/personnel_compensation.csv", ";", col_types=cols(
  .default = col_double(),
  member_name = col_character(),
  staffer_name = col_character(),
  start_date = col_date(format = ""),
  end_date = col_date(format = ""),
  image = col_character(),
  page = col_character(),
  notes = col_character(),
  private_notes = col_character(),
  original_title = col_character()
))

## member_office contains the different ids for merging with other datasets
## use member_office_id to look up member_id and then use member_id to merge 
## other tables.

member_office <- read_delim("columbia/member_office.csv", ";") %>%
  rename(member_start_date = start_date,
         member_end_date = end_date) %>% 
  dplyr::select(-term_id)

## "member_party" contains party_id info

member_party <- read_delim("columbia/member_party.csv", ";") %>%
  dplyr::select(member_id, party_id)

## member_race_gender contains gender and race of members

member_race_gender <- read_csv("columbia/member_race_gender.csv", skip = 1) %>%
  mutate(race = ifelse(is.na(Race),0, Race),
         gender = ifelse(is.na(Gender), 0, Gender)) %>%
  dplyr::select(-core_person_id, -Race, -Gender)

## create csv with all the useful member's info

member_office %>% left_join(member_race_gender) %>%
  left_join(member_party) %>% 
  left_join(office) %>%
  select(member_office_id, member_id, race, gender, party_id, office_type_id) %>%
  rename(race_member = race,
         gender_member = gender) %>%
  write_csv("member_full.csv")
  

## core_person contains gender of staffers

core_person <-read_delim("columbia/core_person.csv", ";") %>%
  dplyr::select(core_person_id, gender)

## "office" contains an office_id 
## (which matches member_office$office_id but not personnel_compensation$office_id)

office <- read_delim("columbia/office.csv", ";") %>% dplyr::select(-ends_with("date"))

## Cleaning FY according to email? putting this on hold

## Create csv file with just last name, first name and core_person_id


## Salary title group id. Matches *some* salary_title_id to a group_id which summarizes and aggregate job titles

salary_title_group <- read_delim("columbia/salary_title_group.csv", ";")

## figuring out repeated salary_title classifications.
repeated <- salary_title_group %>% group_by(salary_title_id) %>%summarise (n=n()) %>% filter(n==2)
title_group <- read_delim("columbia/title_group.csv", ";")
salary_title <- read_delim("columbia/salary_title.csv", ";")

salary_title_group %>% filter(salary_title_id %in% repeated$salary_title_id) %>% 
  arrange (salary_title_id) %>% left_join(title_group) %>% left_join(salary_title) %>% 
  write_csv("salary_title_doubles.csv")

salary_title_group2 <- salary_title_group %>% filter(!(salary_title_id %in% repeated$salary_title_id))

## Adding office ID/member ID to main dataset:

d_full <- d %>% left_join(member_office, by="member_office_id") %>%
  mutate(office_id = ifelse(is.na(office_id.x), office_id.y, office_id.x)) %>% ## fill office_id NAs with office id from member_office dataset.
  dplyr::select(-office_id.x, - office_id.y) %>%
  left_join(office, by = "office_id") %>%
  write_csv("d_full.csv")


## CPI to adjust for inflation (originally 1984 prices) I adjusted it to make 1999 = 100

CPI <- read_csv("cpi.csv") %>% 
  mutate(CPI_1999 = CPI *0.6086427)

## Create a dataset with only members (excludes staff for other offices):

d_members <- d %>% filter (!is.na(member_office_id)) %>% ## select only those working for a congress person's office 
  dplyr::select(-office_id, -title_group_id) %>%
  #left_join(salary_title_group2) %>%
  left_join(member_office, by="member_office_id") %>%
  #left_join(member_party) %>% blocking until i figure out repeated senator
  left_join(member_race_gender) %>%
  left_join(office, by="office_id") %>%
  #left_join(CPI, by=c("fiscal_year"="year")) %>%
  mutate(salary_adjusted = salary,
         salary_adjusted = ifelse(is.na(salary_adjusted), salary, salary_adjusted))


## Distribution of salaries

d_members_plots <- d_members %>% group_by(office_type_id, race, core_person_id) %>% 
  summarise(salary_total = sum(salary_adjusted), n = n()) %>%
  mutate(salary_person = salary_total/n) %>% 
  ungroup() %>%
  mutate(race = recode(race, `0` = "White", `1` = "Black", `2` ="Hispanic", `3`="Asian/PI"))

d_members_plots_sums <- d_members_plots %>% group_by(office_type_id, race) %>%
  summarise (mean = mean(salary_person), median = median(salary_person), sd = sd(salary_person))

salary_distribution <- d_members_plots %>% 
  ggplot(aes(x=salary_person, fill=race)) + geom_histogram() + 
  facet_grid(race ~office_type_id, scales="free") + 
  guides(fill=FALSE) +
  scale_fill_manual(values=wes_palette(n=4, name="GrandBudapest1")) +
  labs (title="Distribution of salary by Congress Member race", x="Average salary per person") 


## Dates for full data set

d_dates <- d_full %>% select(core_person_id, start_date, end_date, office_id) %>% 
  group_by(core_person_id, office_id) %>% arrange(core_person_id, office_id, start_date) %>%
  mutate(lag_end_date = lag(end_date),
         previously_working = as.numeric(start_date - lag_end_date),
         total_days_worked_period = as.numeric(end_date - start_date))


## dates for only staff working for members:

d_dates_m <- d_members %>% dplyr::select(core_person_id, start_date, end_date, member_id, office_type_id) %>% 
  group_by(core_person_id, member_id) %>% arrange(core_person_id, member_id, start_date) %>%
  mutate(lag_end_date = lag(end_date),
         continuous_work = as.numeric(start_date - lag_end_date),
         total_days_worked_period = as.numeric(end_date - start_date)) %>%
  arrange(core_person_id, start_date) %>%
  mutate(continuous_work2 = ifelse(is.na(continuous_work), 0, ## substitutes NAs for 0s
                                   ifelse(continuous_work > 10,0, ## creates employment break if more than 10 days since last period
                                          ifelse(continuous_work < 11 & continuous_work > 0, 1, continuous_work))), # 10 days gap is not a gap
         overlap = ifelse(continuous_work2 > 0, 0, continuous_work2), ## days of overlap between pay periods as negative values
         previously_working = ifelse(continuous_work2 < 0, 0, continuous_work2), # indicator for whether or not staffer was employed in the same office in the previous period
         continuous_period = cumsum(previously_working), ## indicates how many pay periods person has worked for the same office continuously. 
         total_days_worked_period = ifelse(overlap < 0, total_days_worked_period + overlap, total_days_worked_period)) ## add a row with negative days to subtract the overlap

## first and last period of a person in one office can be any length. 
## Periods inbetween should be 180+/-10 for people in Senate and 90 for people in the House.
## what are negative numbers in "previously_working" (there are 42,545 rows) (a) overlap in the dates of start/end for two periods, (b) completely repeated periods. In terms of what to do for "total days worked" 
## these overlap days need to be deleted
## For some people there's a lot of 1 day hires in between their contrat (prob some salary adjustement?)

## Average days worked by member's race:

d_total_days_worked <- d_dates_m %>% group_by(core_person_id, member_id) %>% 
  summarise(total_days = sum(as.numeric(total_days_worked_period), na.rm = T)) %>% 
  ungroup() %>% left_join(member_race_gender, by="member_id") %>% 
  mutate(race = recode(race, `0` = "White", `1` = "Black", `2` ="Hispanic", `3`="Asian/PI")) %>%
  mutate(black = ifelse(race == "Black", 1, 0),
         hispanic = ifelse(race == "Hispanic", 1, 0),
         asian = ifelse(race == "Asian/PI", 1, 0))

d_total_days_worked %>% 
  ggplot(aes(x=total_days, fill=race)) + geom_histogram() + 
  facet_grid(race ~ ., scales="free") + 
  guides(fill=FALSE) +
  scale_fill_manual(values=wes_palette(n=4, name="GrandBudapest1")) +
  labs (title="Distribution of days worked by Congress Member race", x="Days worked per person/office", y="") 

summary(lm(total_days ~ black + hispanic + asian, data=d_total_days_worked))

## when I group by person, some rows have repeated dates 
##(potentially cause person was working in different offices)

## Offices worked on per person (only looking at member's offices)

d_offices <- d_dates_m %>% group_by(core_person_id, member_id) %>% 
  summarise(total_days = sum(as.numeric(total_days_worked_period), na.rm = T)) %>%
  mutate(office_experience = 1:n(),
         total_office = max(office_experience)) %>% ungroup() %>%
  left_join(member_race_gender) %>% 
  mutate(race = recode(race, `0` = "White", `1` = "Black", `2` ="Hispanic", `3`="Asian/PI"))

## for people whose first job was with a black person: how many offices will they eventually work in? vs. white

d_offices_first <- d_offices %>% filter(office_experience==1) %>% 
  mutate(black = ifelse(race == "Black", 1, 0),
         hispanic = ifelse(race == "Hispanic", 1, 0),
         asian = ifelse(race == "Asian/PI", 1, 0))

## I need to incorporate other offices that aren't members of congress's offices. 
# 
# d_dates_full <- d_full %>% 
#   dplyr::select(core_person_id, start_date, end_date, office_id, office_type_id, member_id, salary_title_id, office_type_id) %>% 
#   group_by(core_person_id, office_id) %>% arrange(core_person_id, office_id, start_date, end_date) %>%
#   mutate(lag_end_date = lag(end_date),
#          continuous_work = as.numeric(start_date - lag_end_date),
#          total_days_worked_period = as.numeric(end_date - start_date)) %>%
#   arrange(core_person_id, start_date) 
# 
# d_dates_full %>%
#   mutate(continuous_work2 = ifelse(is.na(continuous_work), 0, ## substitutes NAs for 0s
#                                    ifelse(continuous_work > 10,0, ## creates employment break if more than 10 days since last period
#                                           ifelse(continuous_work < 11 & continuous_work > 0, 1, continuous_work))), # 10 days gap is not a gap
#          overlap = ifelse(continuous_work2 > 0, 0, continuous_work2), ## days of overlap between pay periods as negative values
#          previously_working = ifelse(continuous_work2 < 0, 0, continuous_work2), # indicator for whether or not staffer was employed in the same office in the previous period
#          continuous_period = cumsum(previously_working), ## indicates how many pay periods person has worked for the same office continuously.
#          total_days_worked_period = ifelse(overlap < 0, total_days_worked_period + overlap, total_days_worked_period)) %>% ## add a row with negative days to subtract the overlap
#   ungroup()

d_dates_member <- d_full %>% select(core_person_id, member_id, salary_title_id, start_date, end_date) %>% 
  filter(!is.na(member_id)) %>%
  pivot_longer(c(start_date, end_date), names_to = "date_period", values_to="date") %>% arrange(date) %>%
  mutate(year = as.numeric(format(date, "%Y"))) %>% group_by(core_person_id, year) %>% 
  mutate(year2 = year) %>%
  group_modify(~ add_date_fn(.x)) 

%>%
  arrange(date) %>% 
  mutate(total_days = interval(first(date), last(date)) %>%
           as.duration()/ddays()) %>%
  summarise(total_days = unique(total_days))

d_offices_full_wrong <- d_dates_full %>% 
  group_by(core_person_id, office_id, member_id) %>% 
  summarise(total_days = sum(as.numeric(total_days_worked_period), na.rm = T)) %>%
  ungroup() %>%
  group_by(core_person_id) %>%
  mutate(office_experience = 1:n(),
         total_office = max(office_experience)) %>% ungroup() %>%
  left_join(member_race_gender, by = "member_id") %>%
  mutate(race = recode(race, `0` = "White", `1` = "Black", `2` ="Hispanic", `3`="Asian/PI"))

d_offices_first_full_wrong <- d_offices_full_wrong %>% filter(office_experience==1) %>% 
  filter(!is.na(race)) %>%
  mutate(black = ifelse(race == "Black", 1, 0),
         hispanic = ifelse(race == "Hispanic", 1, 0),
         asian = ifelse(race == "Asian/PI", 1, 0))


# ~78% worked only in 1 office. ~ 22% worked in more than 1 office 
# ~ 7 % worked in more than 2 offices

## Cleaning the data for first office so that the 1 = candidate appears in 2000 or later and has not appeared in the dataset before (ie 1998/1999)


old_staffers <- d_dates_full %>% filter(start_date<"2001-01-01") %>% ungroup() %>%
  distinct(core_person_id)

d_offices_full <- d_dates_full %>% 
  filter(!core_person_id %in% old_staffers$core_person_id) %>%
  group_by(core_person_id, office_id, member_id) %>%
  arrange(start_date) %>%
  summarise(total_days = sum(as.numeric(total_days_worked_period), na.rm = T)) %>%
  ungroup() %>%
  group_by(core_person_id) %>%
  mutate(office_experience = 1:n(),
         total_office = max(office_experience)) %>% ungroup() %>%
  left_join(member_race_gender, by = "member_id") %>%
  mutate(race = recode(race, `0` = "White", `1` = "Black", `2` ="Hispanic", `3`="Asian/PI"))

d_offices_first_full <- d_offices_full %>% filter(office_experience==1) %>% 
  filter(!is.na(race)) %>%
  mutate(black = ifelse(race == "Black", 1, 0),
         hispanic = ifelse(race == "Hispanic", 1, 0),
         asian = ifelse(race == "Asian/PI", 1, 0))

## create an average salary during tenure variable

d_salaries_term <- d_members %>% 
  dplyr::select(core_person_id, salary_adjusted, member_id, start_date, end_date, salary_title_id, office_type_id) %>% 
  mutate(year = as.numeric(format(end_date, "%Y")))


d_salaries_office_year <- d_salaries_term %>% 
  mutate(year = as.numeric(format(end_date, "%Y"))) %>%
  group_by(core_person_id, year, office_id) %>% arrange(core_person_id, year, office_id) %>%
  summarize(total_salary = sum(salary_adjusted),
            mean_wage = mean(wage),
            first = dplyr::first(salary_adjusted)/dplyr::first(total_days_worked_period),
            last = dplyr::last(salary_adjusted)/dplyr::last(total_days_worked_period),
            total_days_worked_year = sum(total_days_worked_period))

d_salaries_member_year <- d_salaries_term %>% filter(!is.na(member_id)) %>%
  group_by(core_person_id, year, member_id, salary_title_id) %>% 
  arrange(core_person_id, year, member_id) %>%
  summarise(total_salary = sum(salary_adjusted)) %>%
  left_join(d_dates_final_m2) 

d_salaries_year <- d_salaries_term %>%
  mutate(year = as.numeric(format(end_date, "%Y"))) %>%
  group_by(core_person_id, year) %>% arrange(core_person_id, year) %>%
  summarize(total_salary = sum(salary_adjusted),
            mean_wage = mean(wage),
            first = dplyr::first(salary_adjusted)/dplyr::first(total_days_worked_period),
            last = dplyr::last(salary_adjusted)/dplyr::last(total_days_worked_period),
            total_days_worked_year = sum(total_days_worked_period))

d_salaries_year_senate <- d_salaries_term %>% 
  filter(office_type_id=="SM") %>%
  group_by(core_person_id, year, member_id, salary_title_id) %>% 
  arrange(core_person_id, year, member_id) %>%
  summarise(total_salary = sum(salary_adjusted)) %>%
  left_join(d_dates_final_m2) 

d_salaries_year_house <- d_salaries_term %>% 
  filter(office_type_id=="HM") %>%
  group_by(core_person_id, year, member_id, salary_title_id) %>% 
  arrange(core_person_id, year, member_id) %>% 
  summarise(total_salary = sum(salary_adjusted)) %>%
  left_join(d_dates_final_m2) 

d_salary_gender <- d_salaries_year %>% left_join(core_person) %>% ungroup() %>%
  filter(total_days_worked_year>0) %>%
  filter(total_salary>0) %>%
  mutate(year_salary = total_salary/total_days_worked_year*365) 






