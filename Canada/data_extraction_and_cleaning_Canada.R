##############################################################################################################
# Canadian data extraction 
#
# Data is avaliable for 2013-2023 from below:
# https://www.canada.ca/en/public-health/services/surveillance/respiratory-virus-detections-canada.html
#
# The data is in the form of html tables so we want to extract the tables into r and 
# combine each of the weeks/seasons. The above webpage has the links to all the pages containing data
# but this code does not automatically grab new datasets as they become available on the site. 
# Currently, new code will also need to be produced for each new season
# 
# Created by: Sarah Pirikahu
# Creation date: 29 November 2022
#############################################################################################################

# load libraries
library(tidyverse)
library(rvest)
library(polite)
library(janitor)

### ~~~~~ 2022 - 2023 season ~~~~ ###

# scraping the links off the website to be able to extract each of the datasets
dat_canada <- 
  bow("https://www.canada.ca/en/public-health/services/surveillance/respiratory-virus-detections-canada.html") %>%  
  scrape()
# reading out the website links
urls_canada <- 
  dat_canada %>%  
  html_elements("a[href]") %>% 
  html_attr("href") 
urls_canada

# the latest 2022 - 2023 data is available at this top page in the links that contain "week-xx-ending"
# filter out the links that do not related to data
urls_can_data <- urls_canada[grep("week", urls_canada)]
urls_can_data
# first entry isn't a report so remove
urls_can_data <- urls_can_data[-1]
length(urls_can_data) 

# add on the additional part of web address missing in these urls 
urls_can_data <- paste0("https://www.canada.ca/", urls_can_data) 
urls_can_data # good

## Initialise a national data frame to put the data into ##
# read in the actual links containing data
site <- read_html(urls_can_data[1]) 
# pull out the html nodes corresponding to tabulated data
tables <- html_nodes(site, "table")
# extract Table 1 which is always the overview of the total number of weekly case for each disease
table1 <- html_table(tables[1], fill=T)[[1]]
# initalise the national dataframe 
nat_dat <- table1[FALSE,]

# pulling out data tables for 2022-2023 season 
for(i in 1:length(urls_can_data)){
  site <- read_html(urls_can_data[i]) # read in the link 
  tables = html_nodes(site, "table")  # pull out the html nodes that represent tables
  length(tables) 
  table1 <- html_table(tables[1], fill=T)[[1]] # read out the table of interest
  
  # extracting the national canada data from table 1
  # split this into two parts as SARS-CoV-2 only got added to the regular reporting part
  # way through this season 
 if(length(grep("SARS-CoV-2", names(table1))) > 0 ){
   nat_dat[i,] <- table1 %>% filter(`Reporting Laboratory`=="CANADA"| `Reporting Laboratory`=="Canada") 
 } else{
   temp_tab <- table1 %>% filter(`Reporting Laboratory`=="CANADA"| `Reporting Laboratory`=="Canada") 
   temp_tab$`SARS-CoV-2 Tested` <- NA
   temp_tab$`SARS-CoV-2 Positive` <- NA
   nat_dat <- rbind(nat_dat, temp_tab)
 }
  
   print(nat_dat)
} 

# add week end and year labels 
web_address_parts <- strsplit(urls_can_data, "/+") # split web address into parts separated by /
web_address_parts <- matrix(unlist(web_address_parts),ncol=9,byrow=TRUE) # unlisting web address parts to access the last parts
last <- web_address_parts[,9] # pulling out the last part 
nat_dat$week <- sub("-ending.*", "", last)
nat_dat$week <- as.numeric(str_remove(nat_dat$week, "week-"))
nat_dat$week_end <- str_remove(last,".html") # rm the .html
nat_dat$week_end <- sub(".*ending-", "", nat_dat$week_end)
nat_dat$year <- as.numeric(substr(nat_dat$week_end, start=nchar(nat_dat$week_end)-3, stop=nchar(nat_dat$week_end)))

nat_dat_2022_2023 <- nat_dat 

# save out 2022-2023 season data
#write_csv(nat_dat_2022_2023, file="/Volumes/Abt.Domenech/Sarah P/Data sources/Surveillance data/Spreadsheets/Canada/nat_dat_2022_2023.csv")

# rm things not needed in the environment now before next lot of data downloading
rm(dat_canada, urls_canada, urls_can_data, site, tables, table1, web_address_parts, last, nat_dat)

### ~~~~~ 2021 - 2022 season ~~~~ ###
# scraping the links
dat_canada <- 
  bow("https://www.canada.ca/en/public-health/services/surveillance/respiratory-virus-detections-canada/2021-2022.html") |> 
  scrape()
# reading out the website links
urls_canada <- 
  dat_canada |> 
  html_elements("a[href]") |> 
  html_attr("href") 
urls_canada

# pull out just the reports that have week 
urls_can_data <- urls_canada[grep("week", urls_canada)]
urls_can_data
# first entry isn't a report so remove
urls_can_data <- urls_can_data[-1]
length(urls_can_data) # 51 for 2021-2022 season

# add on the additional part of web address missing in these urls 
urls_can_data <- paste0("https://www.canada.ca/", urls_can_data) 
urls_can_data # good

# initalise a national dataframe to put data into 
nat_dat_2021_2022 <- nat_dat_2022_2023[FALSE,]
names(nat_dat_2021_2022) # need to remove week and year from the initial so we can write from table directly 
nat_dat_2021_2022 <- nat_dat_2021_2022 %>% select(-c(week,week_end, year))
names(nat_dat_2021_2022)

# pulling out tables 2021-2022 season 
for(i in 1:length(urls_can_data)){
  site <- read_html(urls_can_data[i]) # read in the link 
  tables = html_nodes(site, "table")  # pull out the html nodes that represent tables
  length(tables)  
  table1 <- html_table(tables[1], fill=T)[[1]] # read out the table of interest
  table1 <- table1 %>% mutate(across(everything(), as.character)) # make everything character to avoid mismatched types
  
  # have SARS-COV-2 reporting for only part of the season so accounting for this by addition of SARS-CoV-2 columns
  # to bind the two datasets
  if(length(grep("SARS-CoV-2", names(table1))) > 0 ){
    nat_dat_2021_2022[i,] <- table1 %>% filter(`Reporting Laboratory`=="CANADA"| `Reporting Laboratory`=="Canada") 
  } else{
    temp_tab <- table1 %>% filter(`Reporting Laboratory`=="CANADA"| `Reporting Laboratory`=="Canada") 
    temp_tab$`SARS-CoV-2 Tested` <- NA
    temp_tab$`SARS-CoV-2 Positive` <- NA
    nat_dat_2021_2022 <- rbind(nat_dat_2021_2022, temp_tab)
  }
  
  print(nat_dat_2021_2022)
} 


# add week end and year labels 
web_address_parts <- strsplit(urls_can_data, "/+") # split web address into parts separated by /
# for some reason [[13]] has 11 parts to its address rather than 9
# make this one 9 to be able to do the following way of extracting the last parts
web_address_parts[[13]] <- web_address_parts[[13]][-c(1,2)] 
# unlisting web address parts to access the last parts
web_address_parts <- matrix(unlist(web_address_parts),ncol=9,byrow=TRUE) 
last <- web_address_parts[,9] # pulling out the last part 
nat_dat_2021_2022$week <- sub("-ending.*", "", last)
nat_dat_2021_2022$week <- as.numeric(str_remove(nat_dat_2021_2022$week, "week-"))
nat_dat_2021_2022$week_end <- str_remove(last,".html") # rm the .html
nat_dat_2021_2022$week_end <- sub(".*ending-", "", nat_dat_2021_2022$week_end)
nat_dat_2021_2022$year <- as.numeric(substr(nat_dat_2021_2022$week_end, start=nchar(nat_dat_2021_2022$week_end)-3, stop=nchar(nat_dat_2021_2022$week_end)))
#View(nat_dat_2021_2022)
# write out season 2021-2022 data
#write_csv(nat_dat_2021_2022, file="/Volumes/Abt.Domenech/Sarah P/Data sources/Surveillance data/Canada/nat_dat_2021_2022.csv")

# rm things not needed in the environment now before next lot of data downloading
rm(dat_canada, urls_canada, urls_can_data, site, tables, table1, web_address_parts, last,temp_tab)

###~~~~~~~~~ 2020 - 2021 season ~~~~~~~ ### 

# scraping the links
dat_canada <- 
  bow("https://www.canada.ca/en/public-health/services/surveillance/respiratory-virus-detections-canada/2020-2021.html") |> 
  scrape()
# reading out the website links
urls_canada <- 
  dat_canada |> 
  html_elements("a[href]") |> 
  html_attr("href") 
urls_canada

# pull out just the reports that have week 
urls_can_data <- urls_canada[grep("week", urls_canada)]
urls_can_data
# first entry isn't a report so remove
urls_can_data <- urls_can_data[-1]
length(urls_can_data) # 53 for 2020-2021 season

# add on the additional part of web address missing in these urls 
urls_can_data <- paste0("https://www.canada.ca/", urls_can_data) 
urls_can_data # good

# initalise a national dataframe to put data into 
nat_dat_2020_2021 <- nat_dat_2021_2022[FALSE,]
names(nat_dat_2020_2021) # need to remove week, year and covid stuff from the initial so we can write from table directly 
nat_dat_2020_2021 <- nat_dat_2020_2021 %>% select(-c(week,week_end, year,`SARS-CoV-2 Tested`,`SARS-CoV-2 Positive`))
names(nat_dat_2020_2021)

# pulling out tables 2020-2021 season 
for(i in 1:length(urls_can_data)){
  site <- read_html(urls_can_data[i]) # read in the link 
  tables = html_nodes(site, "table")  # pull out the html nodes that represent tables
  length(tables)  # 11 tables
  table1 <- html_table(tables[1], fill=T)[[1]] # read out the table of interest
  table1 <- table1 %>% mutate(across(everything(), as.character)) # make everything character to avoid mismatched types
  
  # extracting the national canada data from table 1
  nat_dat_2020_2021[i,] <- table1 %>% filter(`Reporting Laboratory`=="CANADA" | `Reporting Laboratory`=="Canada") # pull out just the national level data
  
  print(nat_dat_2020_2021)
} 

# add SARS-CoV-2 columns for joining with other data later
nat_dat_2020_2021$`SARS-CoV-2 Tested` <- NA
nat_dat_2020_2021$`SARS-CoV-2 Positive` <- NA

# add week end and year labels 
web_address_parts <- strsplit(urls_can_data, "/+") # split web address into parts separated by /
# unlisting web address parts to access the last parts
web_address_parts <- matrix(unlist(web_address_parts),ncol=9,byrow=TRUE) 
last <- web_address_parts[,9] # pulling out the last part 
nat_dat_2020_2021$week <- sub("-ending.*", "", last)
nat_dat_2020_2021$week <- as.numeric(str_remove(nat_dat_2020_2021$week, "week-"))
nat_dat_2020_2021$week_end <- str_remove(last,".html") # rm the .html
nat_dat_2020_2021$week_end <- sub(".*ending-", "", nat_dat_2020_2021$week_end)
nat_dat_2020_2021$year <- as.numeric(substr(nat_dat_2020_2021$week_end, start=nchar(nat_dat_2020_2021$week_end)-3, stop=nchar(nat_dat_2020_2021$week_end)))
#View(nat_dat_2020_2021)
# write out the 2020-2021 season data
#write_csv(nat_dat_2020_2021, file="/Volumes/Abt.Domenech/Sarah P/Data sources/Surveillance data/Spreadsheets/Canada/nat_dat_2020_2021.csv")

# rm things not needed in the environment now before next lot of data downloading
rm(dat_canada, urls_canada, urls_can_data, site, tables, table1, web_address_parts, last)

###~~~~~~~~~ 2019 - 2020 season ~~~~~~~ ### 

# scraping the links
dat_canada <- 
  bow("https://www.canada.ca/en/public-health/services/surveillance/respiratory-virus-detections-canada/2019-2020.html") |> 
  scrape()
# reading out the website links
urls_canada <- 
  dat_canada |> 
  html_elements("a[href]") |> 
  html_attr("href") 
urls_canada

# pull out just the reports that have week 
urls_can_data <- urls_canada[grep("week", urls_canada)]
urls_can_data
# first entry isn't a report so remove
urls_can_data <- urls_can_data[-1]
length(urls_can_data) # 52 for 2019-2020 season

# add on the additional part of web address missing in these urls 
urls_can_data <- paste0("https://www.canada.ca/", urls_can_data) 
urls_can_data # good

# initalise a national dataframe to put data into 
nat_dat_2019_2020 <- nat_dat_2021_2022[FALSE,]
names(nat_dat_2019_2020) # need to remove week and year from the initial so we can write from table directly 
nat_dat_2019_2020 <- nat_dat_2019_2020 %>% select(-c(week,week_end, year, `SARS-CoV-2 Tested`, `SARS-CoV-2 Positive`))
names(nat_dat_2019_2020)

# pulling out tables 2019-2020 season 
for(i in 1:length(urls_can_data)){
  site <- read_html(urls_can_data[i]) # read in the link 
  tables = html_nodes(site, "table")  # pull out the html nodes that represent tables
  length(tables)  # 11 tables
  table1 <- html_table(tables[1], fill=T)[[1]] # read out the table of interest
  table1 <- table1 %>% mutate(across(everything(), as.character)) # make everything character to avoid mismatched types
  
  # there was a week where the data was missing from the webpage - make this entry NA for now. Have submitted a 
  # report that the data is missing on the site
  if(dim(table1)[1]==0){
    nat_dat_2019_2020[i,] <- NA
  }else{
  # extracting the national canada data from table 1
  nat_dat_2019_2020[i,] <- table1 %>% filter(`Reporting Laboratory`=="CANADA" | `Reporting Laboratory`=="Canada") # pull out just the national level data
  }
  print(nat_dat_2019_2020)
} 


# add SARS-CoV-2 columns for joining with other data later
nat_dat_2019_2020$`SARS-CoV-2 Tested` <- NA
nat_dat_2019_2020$`SARS-CoV-2 Positive` <- NA

# add week end and year labels 
web_address_parts <- strsplit(urls_can_data, "/+") # split web address into parts separated by /
# unlisting web address parts to access the last parts
web_address_parts <- matrix(unlist(web_address_parts),ncol=9,byrow=TRUE) 
last <- web_address_parts[,9] # pulling out the last part 
nat_dat_2019_2020$week <- sub("-ending.*", "", last)
nat_dat_2019_2020$week <- str_remove(nat_dat_2019_2020$week, "week-")
nat_dat_2019_2020$week <- str_remove(nat_dat_2019_2020$week, "respiratory-virus-detections-isolations-")
nat_dat_2019_2020$week <- as.numeric(str_remove(nat_dat_2019_2020$week, "espiratory-virus-detections-isolations-"))
nat_dat_2019_2020$week_end <- str_remove(last,".html") # rm the .html
nat_dat_2019_2020$week_end <- sub(".*ending-", "", nat_dat_2019_2020$week_end)
nat_dat_2019_2020$year <- as.numeric(substr(nat_dat_2019_2020$week_end, start=nchar(nat_dat_2019_2020$week_end)-3, stop=nchar(nat_dat_2019_2020$week_end)))
#View(nat_dat_2019_2020)
# write out the 2019-2020 season data
#write_csv(nat_dat_2019_2020, file="/Volumes/Abt.Domenech/Sarah P/Data sources/Surveillance data/Spreadsheets/Canada/nat_dat_2019_2020.csv")

# rm things not needed in the environment now before next lot of data downloading
rm(dat_canada, urls_canada, urls_can_data, site, tables, table1, web_address_parts, last)

###~~~~~~~~~ 2018 - 2019 season ~~~~~~~ ### 

# scraping the links
dat_canada <- 
  bow("https://www.canada.ca/en/public-health/services/surveillance/respiratory-virus-detections-canada/2018-2019.html") |> 
  scrape()
# reading out the website links
urls_canada <- 
  dat_canada |> 
  html_elements("a[href]") |> 
  html_attr("href") 
urls_canada

# pull out just the reports that have week 
urls_can_data <- urls_canada[grep("week", urls_canada)]
urls_can_data
# first entry isn't a report so remove
urls_can_data <- urls_can_data[-1]
length(urls_can_data) # 52 for 2018-2019 season

# add on the additional part of web address missing in these urls 
urls_can_data <- paste0("https://www.canada.ca/", urls_can_data) 
urls_can_data # good

# initalise a national dataframe to put data into 
nat_dat_2018_2019 <- nat_dat_2021_2022[FALSE,]
names(nat_dat_2018_2019) # need to remove week and year from the initial so we can write from table directly 
nat_dat_2018_2019 <- nat_dat_2018_2019 %>% select(-c(week,week_end, year,`SARS-CoV-2 Tested`, `SARS-CoV-2 Positive`))
names(nat_dat_2018_2019)

# pulling out tables 2020-2021 season 
for(i in 1:length(urls_can_data)){
  site <- read_html(urls_can_data[i]) # read in the link 
  tables = html_nodes(site, "table")  # pull out the html nodes that represent tables
  length(tables)  # 11 tables
  table1 <- html_table(tables[1], fill=T)[[1]] # read out the table of interest
  table1 <- table1 %>% mutate(across(everything(), as.character)) # make everything character to avoid mismatched types
  
  # extracting the national canada data from table 1
  nat_dat_2018_2019[i,] <- table1 %>% filter(`Reporting Laboratory`=="CANADA" | `Reporting Laboratory`=="Canada") # pull out just the national level data
  print(nat_dat_2018_2019)
} 

# add SARS-CoV-2 columns for joining with other data later
nat_dat_2018_2019$`SARS-CoV-2 Tested` <- NA
nat_dat_2018_2019$`SARS-CoV-2 Positive` <- NA

# add week end and year labels 
web_address_parts <- strsplit(urls_can_data, "/+") # split web address into parts separated by /
# unlisting web address parts to access the last parts
web_address_parts <- matrix(unlist(web_address_parts),ncol=9,byrow=TRUE) 
last <- web_address_parts[,9] # pulling out the last part 
nat_dat_2018_2019$week <- sub("-ending.*", "", last)
nat_dat_2018_2019$week <- as.numeric(str_remove(nat_dat_2018_2019$week, "respiratory-virus-detections-isolations-week-"))
nat_dat_2018_2019$week_end <- str_remove(last,".html") # rm the .html
nat_dat_2018_2019$week_end <- sub(".*ending-", "", nat_dat_2018_2019$week_end)
nat_dat_2018_2019$year <- as.numeric(substr(nat_dat_2018_2019$week_end, start=nchar(nat_dat_2018_2019$week_end)-3, stop=nchar(nat_dat_2018_2019$week_end)))
#View(nat_dat_2018_2019)
# write out the 2018-2019 season data
#write_csv(nat_dat_2018_2019, file="/Volumes/Abt.Domenech/Sarah P/Data sources/Surveillance data/Spreadsheets/Canada/nat_dat_2018_2019.csv")

# rm things not needed in the environment now before next lot of data downloading
rm(dat_canada, urls_canada, urls_can_data, site, tables, table1, web_address_parts, last)


###~~~~~~~~~ 2017 - 2018 season ~~~~~~~ ### 

# scraping the links
dat_canada <- 
  bow("https://www.canada.ca/en/public-health/services/surveillance/respiratory-virus-detections-canada/2017-2018.html") |> 
  scrape()
# reading out the website links
urls_canada <- 
  dat_canada |> 
  html_elements("a[href]") |> 
  html_attr("href") 
urls_canada

# pull out just the reports that have week 
urls_can_data <- urls_canada[grep("week", urls_canada)]
urls_can_data
length(urls_can_data) # 52 for 2017-2018 season

# add on the additional part of web address missing in these urls 
urls_can_data <- paste0("https://www.canada.ca/", urls_can_data) 
urls_can_data # good

# initalise a national dataframe to put data into 
nat_dat_2017_2018 <- nat_dat_2021_2022[FALSE,]
names(nat_dat_2017_2018) # need to remove week and year from the initial so we can write from table directly 
nat_dat_2017_2018 <- nat_dat_2017_2018 %>% select(-c(week,week_end, year, `SARS-CoV-2 Tested`, `SARS-CoV-2 Positive`))
names(nat_dat_2017_2018)

# pulling out tables 2017-2018 season 
for(i in 1:length(urls_can_data)){
  site <- read_html(urls_can_data[i]) # read in the link 
  tables = html_nodes(site, "table")  # pull out the html nodes that represent tables
  length(tables)  # 11 tables
  table1 <- html_table(tables[1], fill=T)[[1]] # read out the table of interest
  
  # cases existed where the table with all virus counts didn't exist, when this is the case set the data to NA 
  # reported problem to website maintainers 
  if(dim(table1)[2]<23){
    nat_dat_2017_2018[i,] <- NA
  } else{
    # there were some tables where the naming structure changed but the order of the viruses remained the same 
  if(length(setdiff(names(table1), names(nat_dat_2017_2018)))>0) {
    names(table1) <- names(nat_dat_2017_2018)
    }
    table1 <- table1 %>% mutate(across(everything(), as.character)) # make everything character to avoid mismatched types
    # extracting the national canada data from table 1
    nat_dat_2017_2018[i,] <- table1 %>% filter(`Reporting Laboratory`=="CANADA" | `Reporting Laboratory`=="Canada" | `Reporting Laboratory`=="CAN.A.") # pull out just the national level data
  }
  print(nat_dat_2017_2018)
} 

# add SARS-CoV-2 columns for joining with other data later
nat_dat_2017_2018$`SARS-CoV-2 Tested` <- NA
nat_dat_2017_2018$`SARS-CoV-2 Positive` <- NA

# add week end and year labels 
web_address_parts <- strsplit(urls_can_data, "/+") # split web address into parts separated by /
# unlisting web address parts to access the last parts
web_address_parts <- matrix(unlist(web_address_parts),ncol=9,byrow=TRUE) 
last <- web_address_parts[,9] # pulling out the last part 
nat_dat_2017_2018$week <- sub("-ending.*", "", last)
nat_dat_2017_2018$week <- as.numeric(str_remove(nat_dat_2017_2018$week, "respiratory-virus-detections-isolations-week-"))
nat_dat_2017_2018$week_end <- str_remove(last,".html") # rm the .html
nat_dat_2017_2018$week_end <- sub(".*ending-", "", nat_dat_2017_2018$week_end)
nat_dat_2017_2018$year <- as.numeric(substr(nat_dat_2017_2018$week_end, start=nchar(nat_dat_2017_2018$week_end)-3, stop=nchar(nat_dat_2017_2018$week_end)))
#View(nat_dat_2017_2018)
# write out the 2017-2018 season data
#write_csv(nat_dat_2017_2018, file="/Volumes/Abt.Domenech/Sarah P/Data sources/Surveillance data/Spreadsheets/Canada/nat_dat_2017_2018.csv")

# rm things not needed in the environment now before next lot of data downloading
rm(dat_canada, urls_canada, urls_can_data, site, tables, table1, web_address_parts, last)


###~~~~~~~~~ 2016 - 2017 season ~~~~~~~ ### 

# scraping the links
dat_canada <- 
  bow("https://www.canada.ca/en/public-health/services/surveillance/respiratory-virus-detections-canada/2016-2017.html") |> 
  scrape()
# reading out the website links
urls_canada <- 
  dat_canada |> 
  html_elements("a[href]") |> 
  html_attr("href") 
urls_canada

# pull out just the reports that have week 
urls_can_data <- urls_canada[grep("week", urls_canada)]
urls_can_data
# first entry isn't a report so remove
urls_can_data <- urls_can_data[-1]
length(urls_can_data) # 41 for 2016-2017 season

# add on the additional part of web address missing in these urls 
urls_can_data <- paste0("https://www.canada.ca/", urls_can_data) 
urls_can_data # good

# initalise a national dataframe to put data into 
nat_dat_2016_2017 <- nat_dat_2021_2022[FALSE,]
names(nat_dat_2016_2017) # need to remove week and year from the initial so we can write from table directly 
nat_dat_2016_2017 <- nat_dat_2016_2017 %>% select(-c(week,week_end, year, `SARS-CoV-2 Tested`,`SARS-CoV-2 Positive`))
names(nat_dat_2016_2017)

# pulling out tables 2016-2017 season 
for(i in 1:length(urls_can_data)){
  site <- read_html(urls_can_data[i]) # read in the link 
  tables = html_nodes(site, "table")  # pull out the html nodes that represent tables
  length(tables)  # 11 tables
  table1 <- html_table(tables[1], fill=T)[[1]] # read out the table of interest
  table1 <- table1 %>% mutate(across(everything(), as.character)) # make everything character to avoid mismatched types
  
  # extracting the national canada data from table 1
  nat_dat_2016_2017[i,] <- table1 %>% filter(`Reporting Laboratory`=="CANADA" | `Reporting Laboratory`=="Canada" | `Reporting Laboratory`=="CAN.A.") # pull out just the national level data
  print(nat_dat_2016_2017)
} 

# add SARS-CoV-2 columns for joining with other data later
nat_dat_2016_2017$`SARS-CoV-2 Tested` <- NA
nat_dat_2016_2017$`SARS-CoV-2 Positive` <- NA

# add week end and year labels 
web_address_parts <- strsplit(urls_can_data, "/+") # split web address into parts separated by /
# unlisting web address parts to access the last parts
web_address_parts <- matrix(unlist(web_address_parts),ncol=9,byrow=TRUE) 
last <- web_address_parts[,9] # pulling out the last part 
nat_dat_2016_2017$week <- sub("-ending.*", "", last)
nat_dat_2016_2017$week <- as.numeric(str_remove(nat_dat_2016_2017$week, "respiratory-virus-detections-isolations-week-"))
nat_dat_2016_2017$week_end <- str_remove(last,".html") # rm the .html
nat_dat_2016_2017$week_end <- sub(".*ending-", "", nat_dat_2016_2017$week_end)
nat_dat_2016_2017$year <- as.numeric(substr(nat_dat_2016_2017$week_end, start=nchar(nat_dat_2016_2017$week_end)-3, stop=nchar(nat_dat_2016_2017$week_end)))
#View(nat_dat_2016_2017)
# write out the 2016-2017 season data
#write_csv(nat_dat_2016_2017, file="/Volumes/Abt.Domenech/Sarah P/Data sources/Surveillance data/Spreadsheets/Canada/nat_dat_2016_2017.csv")

# rm things not needed in the environment now before next lot of data downloading
rm(dat_canada, urls_canada, urls_can_data, site, tables, table1, web_address_parts, last)

###~~~~~~~~~ 2015 - 2016 season ~~~~~~~ ### 

# scraping the links
dat_canada <- 
  bow("https://www.canada.ca/en/public-health/services/surveillance/respiratory-virus-detections-canada/2015-2016.html") |> 
  scrape()
# reading out the website links
urls_canada <- 
  dat_canada |> 
  html_elements("a[href]") |> 
  html_attr("href") 
urls_canada

# pull out just the reports that have week 
urls_can_data <- urls_canada[grep("week", urls_canada)]
urls_can_data
length(urls_can_data) # 52 for 2015-2016 season

# add on the additional part of web address missing in these urls 
urls_can_data <- paste0("https://www.canada.ca/", urls_can_data) 
urls_can_data # good

# initalise a national dataframe to put data into 
nat_dat_2015_2016 <- nat_dat_2021_2022[FALSE,]
names(nat_dat_2015_2016) # need to remove week and year from the initial so we can write from table directly 
nat_dat_2015_2016 <- nat_dat_2015_2016 %>% select(-c(week,week_end, year,`SARS-CoV-2 Tested`,`SARS-CoV-2 Positive`))
names(nat_dat_2015_2016)

# pulling out tables 2016-2017 season 
for(i in 1:length(urls_can_data)){
  site <- read_html(urls_can_data[i]) # read in the link 
  tables = html_nodes(site, "table")  # pull out the html nodes that represent tables
  length(tables)  # 11 tables
  table1 <- html_table(tables[1], fill=T)[[1]] # read out the table of interest
  table1 <- table1 %>% mutate(across(everything(), as.character)) # make everything character to avoid mismatched types
  
  # extracting the national canada data from table 1
  nat_dat_2015_2016[i,] <- table1 %>% filter(`Reporting Laboratory`=="CANADA" | `Reporting Laboratory`=="Canada" | `Reporting Laboratory`=="CAN.A.") # pull out just the national level data
  print(nat_dat_2015_2016)
} 

# add SARS-CoV-2 columns for joining with other data later
nat_dat_2015_2016$`SARS-CoV-2 Tested` <- NA
nat_dat_2015_2016$`SARS-CoV-2 Positive` <- NA

# add week end and year labels 
web_address_parts <- strsplit(urls_can_data, "/+") # split web address into parts separated by /
# unlisting web address parts to access the last parts
web_address_parts <- matrix(unlist(web_address_parts),ncol=9,byrow=TRUE) 
last <- web_address_parts[,9] # pulling out the last part 
nat_dat_2015_2016$week <- sub("-ending.*", "", last)
nat_dat_2015_2016$week <- as.numeric(str_remove(nat_dat_2015_2016$week, "respiratory-virus-detections-isolations-week-"))
nat_dat_2015_2016$week_end <- str_remove(last,".html") # rm the .html
nat_dat_2015_2016$week_end <- sub(".*ending-", "", nat_dat_2015_2016$week_end)
nat_dat_2015_2016$year <- as.numeric(substr(nat_dat_2015_2016$week_end, start=nchar(nat_dat_2015_2016$week_end)-3, stop=nchar(nat_dat_2015_2016$week_end)))
#View(nat_dat_2015_2016)
# write out the 2016-2017 season data
#write_csv(nat_dat_2015_2016, file="/Volumes/Abt.Domenech/Sarah P/Data sources/Surveillance data/Spreadsheets/Canada/nat_dat_2015_2016.csv")

# rm things not needed in the environment now before next lot of data downloading
rm(dat_canada, urls_canada, urls_can_data, site, tables, table1, web_address_parts, last)

###~~~~~~~~~ 2014 - 2015 season ~~~~~~~ ### 

# scraping the links
dat_canada <- 
  bow("https://www.canada.ca/en/public-health/services/surveillance/respiratory-virus-detections-canada/2014-2015.html") |> 
  scrape()
# reading out the website links
urls_canada <- 
  dat_canada |> 
  html_elements("a[href]") |> 
  html_attr("href") 
urls_canada

# pull out just the reports that have week 
urls_can_data <- urls_canada[grep("week", urls_canada)]
urls_can_data
length(urls_can_data) # 52 for 2014-2015 season

# add on the additional part of web address missing in these urls 
urls_can_data <- paste0("https://www.canada.ca/", urls_can_data) 
urls_can_data # good

# initalise a national dataframe to put data into 
nat_dat_2014_2015 <- nat_dat_2021_2022[FALSE,]
names(nat_dat_2014_2015) # need to remove week and year from the initial so we can write from table directly 
nat_dat_2014_2015 <- nat_dat_2014_2015 %>% select(-c(week,week_end, year,`SARS-CoV-2 Tested`,`SARS-CoV-2 Positive`))
names(nat_dat_2014_2015)

# pulling out tables 2016-2017 season 
for(i in 1:length(urls_can_data)){
  site <- read_html(urls_can_data[i]) # read in the link 
  tables = html_nodes(site, "table")  # pull out the html nodes that represent tables
  length(tables)  # 11 tables
  table1 <- html_table(tables[1], fill=T)[[1]] # read out the table of interest
  table1 <- table1 %>% mutate(across(everything(), as.character)) # make everything character to avoid mismatched types
  names(table1)[1] <- "Reporting Laboratory"
  
  # extracting the national canada data from table 1
  nat_dat_2014_2015[i,] <- table1 %>% filter(`Reporting Laboratory`=="CANADA" | `Reporting Laboratory`=="Canada" | `Reporting Laboratory`=="CAN.A.") # pull out just the national level data
  print(nat_dat_2014_2015)
} 

# add SARS-CoV-2 columns for joining with other data later
nat_dat_2014_2015$`SARS-CoV-2 Tested` <- NA
nat_dat_2014_2015$`SARS-CoV-2 Positive` <- NA

# add week end and year labels 
web_address_parts <- strsplit(urls_can_data, "/+") # split web address into parts separated by /
# unlisting web address parts to access the last parts
web_address_parts <- matrix(unlist(web_address_parts),ncol=9,byrow=TRUE) 
last <- web_address_parts[,9] # pulling out the last part 
nat_dat_2014_2015$week <- sub("-ending.*", "", last)
nat_dat_2014_2015$week <- as.numeric(str_remove(nat_dat_2014_2015$week, "respiratory-virus-detections-isolations-week-"))
nat_dat_2014_2015$week_end <- str_remove(last,".html") # rm the .html
nat_dat_2014_2015$week_end <- sub(".*ending-", "", nat_dat_2014_2015$week_end)
nat_dat_2014_2015$year <- as.numeric(substr(nat_dat_2014_2015$week_end, start=nchar(nat_dat_2014_2015$week_end)-3, stop=nchar(nat_dat_2014_2015$week_end)))
#View(nat_dat_2014_2015)
# write out the 2016-2017 season data
#write_csv(nat_dat_2014_2015, file="/Volumes/Abt.Domenech/Sarah P/Data sources/Surveillance data/Spreadsheets/Canada/nat_dat_2014_2015.csv")

# rm things not needed in the environment now before next lot of data downloading
rm(dat_canada, urls_canada, urls_can_data, site, tables, table1, web_address_parts, last)

###~~~~~~~~~ 2013 - 2014 season ~~~~~~~ ### 

# scraping the links
dat_canada <- 
  bow("https://www.canada.ca/en/public-health/services/surveillance/respiratory-virus-detections-canada/2013-2014.html") |> 
  scrape()
# reading out the website links
urls_canada <- 
  dat_canada |> 
  html_elements("a[href]") |> 
  html_attr("href") 
urls_canada

# pull out just the reports that have week 
urls_can_data <- urls_canada[grep("week", urls_canada)]
urls_can_data
length(urls_can_data) # 51 for 2013-2014 season

# add on the additional part of web address missing in these urls 
urls_can_data <- paste0("https://www.canada.ca/", urls_can_data) 
urls_can_data # good

# initalise a national dataframe to put data into 
nat_dat_2013_2014 <- nat_dat_2021_2022[FALSE,]
names(nat_dat_2013_2014) # need to remove week and year from the initial so we can write from table directly 
nat_dat_2013_2014 <- nat_dat_2013_2014 %>% select(-c(week,week_end, year,`SARS-CoV-2 Tested`,`SARS-CoV-2 Positive`))
names(nat_dat_2013_2014)

# pulling out tables 2013-2014 season 
for(i in 1:length(urls_can_data)){
  site <- read_html(urls_can_data[i]) # read in the link 
  tables = html_nodes(site, "table")  # pull out the html nodes that represent tables
  length(tables)  # 11 tables
  table1 <- html_table(tables[1], fill=T)[[1]] # read out the table of interest
  table1 <- table1 %>% mutate(across(everything(), as.character)) # make everything character to avoid mismatched types
  
  # extracting the national canada data from table 1
  nat_dat_2013_2014[i,] <- table1 %>% filter(`Reporting Laboratory`=="CANADA" | `Reporting Laboratory`=="Canada" | `Reporting Laboratory`=="CAN.A.") # pull out just the national level data
  print(nat_dat_2013_2014)
} 

# add SARS-CoV-2 columns for joining with other data later
nat_dat_2013_2014$`SARS-CoV-2 Tested` <- NA
nat_dat_2013_2014$`SARS-CoV-2 Positive` <- NA

# add week end and year labels 
web_address_parts <- strsplit(urls_can_data, "/+") # split web address into parts separated by /
# unlisting web address parts to access the last parts
web_address_parts <- matrix(unlist(web_address_parts),ncol=9,byrow=TRUE) 
last <- web_address_parts[,9] # pulling out the last part 
nat_dat_2013_2014$week <- sub("-ending.*", "", last)
nat_dat_2013_2014$week <- as.numeric(str_remove(nat_dat_2013_2014$week, "respiratory-virus-detections-isolations-week-"))
nat_dat_2013_2014$week_end <- str_remove(last,".html") # rm the .html
nat_dat_2013_2014$week_end <- sub(".*ending-", "", nat_dat_2013_2014$week_end)
nat_dat_2013_2014$year <- as.numeric(substr(nat_dat_2013_2014$week_end, start=nchar(nat_dat_2013_2014$week_end)-3, stop=nchar(nat_dat_2013_2014$week_end)))
View(nat_dat_2013_2014)
# write out the 2016-2017 season data
#write_csv(nat_dat_2013_2014, file="/Volumes/Abt.Domenech/Sarah P/Data sources/Surveillance data/Spreadsheets/Canada/nat_dat_2013_2014.csv")

# rm things not needed in the environment now before next lot of data downloading
rm(dat_canada, urls_canada, urls_can_data, site, tables, table1, web_address_parts, last)


####------- COMBINE ALL DATASETS TOGETHER ------####

# combine all datasets
CAN <- rbind(nat_dat_2013_2014, nat_dat_2014_2015, nat_dat_2015_2016, nat_dat_2016_2017, nat_dat_2017_2018, 
      nat_dat_2018_2019, nat_dat_2019_2020, nat_dat_2020_2021, nat_dat_2021_2022, nat_dat_2022_2023)
      
# check duplicates
names(CAN)
CAN %>% group_by(year, week) %>% tally() %>% filter(n>1) # 4 that have two rows of data for 1 week they will
# likely have different week ends
#CAN %>% filter(year=="2014" & week==29) %>% View() # yeah they do have different week ends two sets of data
# just collected within this week

# check years of data are as expected 
CAN %>% group_by(year) %>% tally() # 2013-2022

# clean names 
CAN <- clean_names(CAN)
names(CAN)
# remove reporting lab - just all canada
CAN$reporting_laboratory <- NULL
# adding n to names
names(CAN)
names(CAN) <- c("n_influenza_tests", "n_a_h1_pdm09", "n_a_h3", 
               "n_a_unspecified_type", "n_total_a", "n_total_b",
                "n_rsv_tests", "n_rsv", "n_hpiv_tests",
                "n_hpiv_1", "n_hpiv_2", "n_hpiv_3", "n_hpiv_4",   
                "n_other_hpiv" , "n_adv_tests" , "n_adv" , "n_hmpv_tests",
                "n_hmpv" ,  "n_ev_rv_tests", "n_ev_rv" ,  "n_h_co_v_tests" ,      
                "n_h_co_v","sars_co_v_2_tested", "sars_co_v_2_positive","week", "week_end","year")

# write out data 
#write_csv(CAN, file="/Volumes/Abt.Domenech/Sarah P/Data sources/Surveillance data/Canada/Canada_data_2013_2022.csv")




