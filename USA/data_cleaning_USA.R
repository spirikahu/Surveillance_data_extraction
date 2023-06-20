########################################################################
# Pulling together flu clinical and public health lab data
# for post 2015 and then combining with prior 2015 so 
# we have one big dataset for flu:
#
# Data sets post 2015 for combining:
# 1. WHO_NREVSS_Clinical_Labs 2015_22.csv
# 2. WHO_NREVSS_Public_Health_Labs 2015_22.csv
# 
# Data set prior 2015 to combine with the above two once they 
# have been combined 
# 3. WHO_NREVSS_Combined_prior_to_2015_16.csv
# 
# These datasets are sitting here: 
# smb://fileserver.mpiib-berlin.mpg.de/Abt.Domenech/Sarah P/Data sources/Surveillance data/Spreadsheets/USA
# 
# Created by: Sarah Pirikahu 
# Creation date: 28 November 2022
########################################################################

# load libraries
library(tidyverse)
library(readr)
library(janitor)
library(lubridate)

# load in data

### INFLUENZA DATA ####
# post 2015 clinic
clinic <- read_csv("/Volumes/Abt.Domenech/Sarah P/Data sources/Surveillance_data_extraction/USA/WHO_NREVSS_Clinical_Labs 2015_22.csv", skip=1)
head(clinic)
# clean names
clinic <- clean_names(clinic)
names(clinic)

# post 2015 public health labs - all data is total number of positive cases
ph_lab <- read_csv("/Volumes/Abt.Domenech/Sarah P/Data sources/Surveillance_data_extraction/USA/WHO_NREVSS_Public_Health_Labs 2015_22.csv", skip=1)
head(ph_lab)
# clean names
ph_lab <- clean_names(ph_lab)
names(ph_lab) # the public health labs do more subtyping than the clinics do, hence why it has more columns 

# prior 2015 data where clinical labs and public health labs data is combined
prior_2015 <- read_csv("/Volumes/Abt.Domenech/Sarah P/Data sources/Surveillance_data_extraction/USA/WHO_NREVSS_Combined_prior_to_2015_16.csv", skip=1)
head(prior_2015)
# clean names
prior_2015 <- clean_names(prior_2015)
names(prior_2015)


#----- combining the two post 2015 datasets ----#

# checking for duplicates
clinic %>% group_by(year, week) %>% tally() %>% filter(n>1) # 0
ph_lab %>% group_by(year, week) %>% tally() %>% filter(n>1) # 0 

# creating additional columns for each dataset so they have the same number so I can easily rbind them 
names(clinic)
names(ph_lab)

# clinic needs a_2009_h1n1, a_h3, a_subtyping_not_performed, b_vic, b_yam, h3n2v
# can also remove percent positive columns as we shall recalculate these
clinic <- clinic %>% select(-c(percent_a, percent_b, percent_positive))
clinic$a_2009_h1n1 <- NA
clinic$a_h3 <- NA
clinic$a_subtyping_not_performed <- NA
clinic$b <- NA # I think b by itself in the ph_lab dataset is b not subtyped, but it isn't clear
clinic$b_vic <- NA
clinic$b_yam <- NA
clinic$h3n2v <- NA
names(clinic)

# ph_lab needs total a and total b 
# calculate these
# total a 
ph_lab$total_a <- ph_lab$a_2009_h1n1 + ph_lab$a_h3 + ph_lab$a_subtyping_not_performed + ph_lab$h3n2v
# total b 
ph_lab$total_b <- ph_lab$b + ph_lab$b_vic + ph_lab$b_yam

# check if the column dimensions are the same now for the two datasets
dim(ph_lab) # 14 
dim(clinic) # 14 - yay can rbind now

post_2015 <- rbind(clinic, ph_lab)
str(post_2015)
dim(post_2015) # 740 (370 + 370 = 740 as expected)
head(post_2015)

# combining by week and year and summing the appropriate columns to get a reduced dataset that 
# should be of size 370 
influenza_post2015 <- post_2015 %>% group_by(year, week) %>% 
         summarise(total_specimens = sum(total_specimens,na.rm=T),
                   total_a = sum(total_a, na.rm=T), total_b = sum(total_b,na.rm=T),
                   a_2009_h1n1 = sum(a_2009_h1n1, na.rm=T), a_h3 = sum(a_h3,na.rm=T),
                   a_subtyping_not_performed = sum(a_subtyping_not_performed,na.rm=T),
                   b_vic = sum(b_vic, na.rm=T), b_yam = sum(b_yam,na.rm=T),
                   h3n2v = sum(h3n2v,na.rm=T), b = sum(b, na.rm=T))
View(influenza_post2015)
dim(influenza_post2015) # 370
# check no duplicated week/years
influenza_post2015 %>% group_by(year,week) %>% tally() %>% filter(n>1) # 0

# check a couple cases -- looks good! 
post_2015 %>% filter(year==2015 & week == 40) %>% select(year, week, total_specimens, total_a, total_b)
post_2015 %>% filter(year==2015 & week == 41) %>% select(year, week, total_specimens, total_a, total_b)
# relabeling post_2015 as our new datasrt
post_2015 <- influenza_post2015

##--- combine the two datasets pre and post 2015 ---##
names(post_2015) %in% names(prior_2015)
names(prior_2015) %in% names(post_2015)
names(prior_2015)
names(post_2015)

# check prior 2015 for years - are they actually all prior 2015?
prior_2015 %>% group_by(year) %>% tally() # yes all 2015 or prior 

# remove columns not needed from prior dataset
prior_2015 <- prior_2015 %>% select(-c(percent_positive,region, region_type))
names(prior_2015) # good 

# create a_unable_to_subtype for post_2015 dataset so that we can rbind it with the prior 
post_2015$a_unable_to_subtype <- NA
# prior 2015 they were not subtyping influenza B but they do now so add extra columns to prior to be able to join 
prior_2015$b_vic <- NA
prior_2015$b_yam <- NA

# create total a and total b for prior 2015 dataset
prior_2015$total_a <- rowSums(prior_2015[,c("a_2009_h1n1", "a_h1", "a_h3", "a_subtyping_not_performed",
                                            "a_unable_to_subtype")], na.rm=TRUE)
# check - looks good 
prior_2015 %>% select(year, week, total_a, a_2009_h1n1 , a_h1, a_h3, a_subtyping_not_performed, a_unable_to_subtype)
# create total b column 
prior_2015$total_b <- rowSums(prior_2015[,c("b", "b_yam", "b_vic")], na.rm=TRUE)
# check - looks good
prior_2015 %>% select(year, week, total_b,b,b_vic,b_yam)
# checking no duplicates now
prior_2015 %>% group_by(year,week) %>% tally() %>% filter(n>1) # 0

# checking for any more discrepencies in names
!(names(prior_2015) %in% names(post_2015)) # 
# a_h1 is in prior 2015 but not post
post_2015$a_h1 <- NA
# check names again
names(prior_2015)
names(post_2015)


# any duplicates induce?
prior_2015 %>% group_by(year,week) %>% tally() %>% filter(n>1) # 0
post_2015 %>% group_by(year,week) %>% tally() %>% filter(n>1) # 0 - still good

# rbind the two datasets
dim(post_2015) # 370 x 14
dim(prior_2015) # 940 x 14 
US_influenza <- rbind(prior_2015, post_2015)
View(US_influenza)

# any duplicates induce?
US_influenza %>% group_by(year,week) %>% tally() %>% filter(n>1) # 0


# renaming some of the columns to avoid confusion when merging with RSV data
names(US_influenza)
names(US_influenza) <- c("year", "week", "total_specimens", "n_a_2009_h1n1","n_a_h1", "n_a_h3", "n_a_subtyping_not_performed",
                         "n_a_unable_to_subtype", "n_b", "n_h3n2v", "n_b_vic", "n_b_yam", "n_total_a","n_total_b")
names(US_influenza)

# just influenza data if wish to read out 
# write_csv(US_influenza, file="/Volumes/Abt.Domenech/Sarah P/Data sources/Surveillance data/Spreadsheets/USA/Influenza_1998_2022_cleaned_28Nov2022.csv")

# rm data sets no longer needed from the environment 
rm(clinic, influenza_post2015, ph_lab,post_2015,prior_2015)

###--- RSV DATA ---###

US_RSV <- read_csv("/Volumes/Abt.Domenech/Sarah P/Data sources/Surveillance_data_extraction/USA/Respiratory_Syncytial_Virus_Laboratory_Data__NREVSS_.csv")
dim(US_RSV) # 10313
str(US_RSV) # tbl 
# make into tibble for easier merging later
US_RSV <- as_tibble(US_RSV)
str(US_RSV)
# clean names of columns
US_RSV <- clean_names(US_RSV)
names(US_RSV)

# check out this surveillance year and week code columns 
US_RSV %>% group_by(surveillance_year) %>% tally()
# the week ending code is the year then week number so we have 2010-2020
# year column is the two years the data spans so 1011 for example is the 
# season 2010-2011

# to be able to cbind this with the influenza data we want to make the year and week column actual year and week # 
# rather than these codes 
US_RSV$week <- as.numeric(substr(US_RSV$week_ending_code,3,4))
US_RSV %>% group_by(week) %>% tally() %>% dim() # 53 good as there are 52 weeks in the year + 53 for those leap years
# to pull out the year make the week ending date a date then pull out the year
US_RSV$week_ending_date <- as.Date(US_RSV$week_ending_date, format="%d-%b-%y")
US_RSV$year <- year(US_RSV$week_ending_date)
# check 
US_RSV %>% group_by(year) %>% tally() # 2010 - 2020
# remove surveillance year
US_RSV$surveillance_year <- NULL
# remove week ending code
US_RSV$week_ending_code <- NULL
# rm outlier column
US_RSV$outlier <- NULL

# add rsv to the start of all the columns here 
names(US_RSV)
names(US_RSV) <-c("rsv_diagnostic_test_type", "rsv_week_ending_date", "rsv_hhs_region", "n_rsv", "n_rsv_tests",
                  "week", "year")
names(US_RSV)

# check duplicates
US_RSV %>% group_by(year,week) %>% tally() %>% filter(n>20) # 2 weeks with two rows of data
# check these out 
US_RSV %>% filter(year=="2016" & week=="52") %>% View() # 2 differnet week end dates - looks like they 
# did an extra at the start of the new year but it was labeled the previous year
US_RSV %>% filter(year=="2011" & week=="52") %>% View() # same thing here
# should be ok if we plot by week end date? 

# combine the hhs regions (and thus diagnostic test type) to get an overall rsv count
US_RSV_overall <- US_RSV %>% group_by(year, week, rsv_week_ending_date) %>% 
  summarise(n_rsv = sum(n_rsv), n_rsv_tests=sum(n_rsv_tests))
head(US_RSV_overall) # checked and looks good
dim(US_RSV_overall) # 521

# check duplicates 
US_RSV_overall %>% group_by(year, week) %>% tally() %>% filter(n>1) # just the 2 as expected

# could also look at test type later if wished
US_RSV_test_type <- US_RSV %>% group_by(year, week, rsv_week_ending_date,rsv_diagnostic_test_type) %>% 
  summarise(n_rsv = sum(n_rsv), n_rsv_tests=sum(n_rsv_tests))
head(US_RSV_test_type) # checked and looks good
dim(US_RSV_test_type) # 1042

###----- COMBINE INFLUENZA AND RSV DATA -----###

dim(US_influenza) # 1310  x  14
dim(US_RSV_overall) # 521 x  5

# why is the RSV dataset so much bigger than the influenza one when it is over a shorter period of time? 
# because it has the multiple different types of testing per week e.g. antigen, PCR etc. 

US <- US_RSV_overall %>% left_join(US_influenza)
dim(US) # 521
View(US)
# check we have one row of data for every week of every year and no dups
US %>% group_by(year,week) %>% tally() %>% filter(n>1) # just the 2 new year duplicates

# relabel total specimens as n_influenza_tests
US <- rename(US, n_influenza_tests = total_specimens)

# write out the combined dataset 
#write_csv(US, file="/Volumes/Abt.Domenech/Sarah P/Data sources/Surveillance data/Spreadsheets/USA/US_RSV_and_flu_data_28Nov2022.csv")

