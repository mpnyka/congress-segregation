setwd("~/Desktop/Sociology/RA/Data")

library(tidyverse)
library(wesanderson)
library(lubridate)
library(lme4)

votes <- read_csv("HS111_votes.csv")
rollcalls <- read_csv("HS111_rollcalls.csv")

member_ideology <- read_csv("HSall_members.csv")

race_staffer <- read_csv("race_staffer.csv") %>% dplyr::select(-X1)
molly_dat <- read_csv("staff_rank_data.csv")

## Our data grouped by Congress

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

member_ideology2 <- member_ideology %>% 
  separate(bioname, into = c("last_name", "first_name"), sep = "\\,") %>%
  mutate(first_name = str_squish(first_name),
         last_name = tolower(last_name))

member_office <- read_delim("columbia/member_office.csv", ";") %>%
  rename(member_start_date = start_date,
         member_end_date = end_date) %>% 
  dplyr::select(-term_id)

member_race_gender <- read_csv("columbia/member_race_gender.csv", skip = 1) %>%
  mutate(race = ifelse(is.na(Race),0, Race),
         gender = ifelse(is.na(Gender), 0, Gender),
         last_name = tolower(last_name)) %>%
  dplyr::select(-core_person_id, -Race, -Gender)

member_party <- read_delim("columbia/member_party.csv", ";") %>%
  dplyr::select(member_id, party_id)

office <- read_delim("columbia/office.csv", ";") %>% dplyr::select(-ends_with("date"))


## select ICPSR codes from Molly

molly_icpsr <- molly_dat %>% dplyr::select(congress, member_office_id, icpsr) %>%distinct()

no_code <- d_congress %>% ungroup() %>%
  left_join(member_office) %>%
  left_join(office) %>%
  left_join(member_race_gender) %>% 
  left_join(molly_icpsr) %>%
  filter(is.na(icpsr)) %>%
  select(state_id, congress, first_name, last_name, member_office_id) %>%
  group_by(congress, state_id, last_name, first_name, member_office_id) %>%
  summarise(n=n()) %>% dplyr::select(-n)

coded1 <- d_congress %>% ungroup() %>%
  left_join(member_office) %>%
  left_join(office) %>%
  left_join(member_race_gender) %>% 
  group_by(congress, member_office_id) %>%
  summarise(n = n()) %>%
  left_join(molly_icpsr) %>%
  filter(!is.na(icpsr)) %>% select(-n)

coded2 <- no_code %>% 
  left_join(member_ideology2, by = c("state_id" = "state_abbrev", "last_name" = "last_name", "congress" = "congress")) %>%
  filter(!is.na(icpsr)) %>% ungroup() %>% select(congress, member_office_id, icpsr)

congress_icpsr <- rbind(coded1, coded2)

d_congress2 <- d_congress %>% left_join(congress_icpsr) %>% 
  filter(!is.na(icpsr)) %>% left_join(member_ideology)

congress_race <- d_congress2 %>%
  left_join(race_staffer) %>% 
  mutate(white = ifelse(race == "nh_white", 1, 0),
         black = ifelse(race == "nh_black", 1, 0),
         asian = ifelse(race == "asian", 1, 0),
         hispanic = ifelse(race == "hispanic", 1, 0))

dat_congress <- congress_race %>% group_by(congress, icpsr, member_office_id) %>% 
  summarise(black_pct = mean(black, na.rm = T)) %>%
  left_join(member_ideology) %>% left_join(member_office) %>% 
  left_join(member_race_gender, by = "member_id") %>%
  left_join(member_party, by = "member_id") %>%
  rename(
         race_member = race) %>%
  mutate(female_member = ifelse(gender == 1, 1, 0),
         democrat = ifelse(party_id == 2, 1, 0))
write_csv(dat_congress, "dat_congress.csv")

dim1_1 <- lm(nominate_dim1 ~ black_pct, 
           data = dat_congress)

dim1_2 <- lm(nominate_dim1 ~ black_pct + as.factor(race_member) + chamber  + democrat + gender, 
           data = dat_congress)

dim1_3 <- lm(nominate_dim1 ~ black_pct + as.factor(race_member) + chamber  + democrat + gender + state_abbrev 
             + as.factor(congress), 
             data = dat_congress)

dim1_4 <- lm(nominate_dim1 ~ black_pct * as.factor(race_member) + chamber  + democrat + gender + state_abbrev 
             + as.factor(congress), 
             data = dat_congress)


dim1_republican <- lm(nominate_dim1 ~ black_pct + as.factor(race_member) + chamber
                      + gender + state_abbrev 
             + as.factor(congress), 
             data = subset(dat_congress, democrat == 0))

dim1_democrat<- lm(nominate_dim1 ~ black_pct  + as.factor(race_member)
                      + gender + state_abbrev + chamber
                      + as.factor(congress), 
                      data = subset(dat_congress, democrat == 1))


dim1_senate <- lm(nominate_dim1 ~ black_pct  + as_factor(race_member)
                   + gender + state_abbrev + democrat
                   + as.factor(congress), 
                   data = subset(dat_congress, chamber == "Senate"))

dim1_house <- lm(nominate_dim1 ~ black_pct  + as_factor(race_member) + democrat
                           + gender + state_abbrev 
                           + as.factor(congress), 
                           data = subset(dat_congress, chamber == "House" ))


## random effects model

dat_congress2 <- dat_congress %>% mutate(race_gender = case_when(gender == 0 & race_member == 0 ~ "white_male",
                                                                 gender == 0 & race_member == 1 ~ "black_male",
                                                                 gender == 0 & race_member == 2 ~ "hispanic_male",
                                                                 gender == 0 & race_member == 3 ~ "asian_male",
                                                                 gender == 1 & race_member == 0 ~ "white_female",
                                                                 gender == 1 & race_member == 1 ~ "black_female",
                                                                 gender == 1 & race_member == 2 ~ "hispanic_female",
                                                                 gender == 1 & race_member == 3 ~ "asian_female",
                                                                 TRUE ~ NA_character_))

dim1_re <- lmer(nominate_dim1 ~ black_pct + chamber  + democrat + as.factor(congress) + (black_pct | race_gender)
                + (1 | state_abbrev), data = dat_congress2)

stargazer(dim1_1, dim1_2, dim1_3, dim1_4, type="text", out="~/Downloads/models.htm", 
          keep = c("\\brace_member\\b", "\\bpct_black\\b", "\\bblack_pct\\b", "\\bchamberSenate\\b", "\\bdemocrat\\b", "\\bgender\\b"))

dim2 <-lm(nominate_dim2 ~ black_pct*as.factor(race_member) + chamber + as.factor(race_member) + democrat + gender + state_abbrev 
                      + nominate_number_of_votes + as.factor(congress), 
                      data = dat_congress)






