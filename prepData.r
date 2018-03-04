######################################################################################################## 
# Author:   Hannah Fresques
# Date:     March 2, 2018
# Project:  NICAR logistic regression training
# Purpose:  clean data from Illinois DoE to use in the example
# 
# Version/update notes:
# 
########################################################################################################

library(dplyr)
library(rstudioapi)
library(readr)
library(xlsx)
library(stringr)

getActiveDocumentContext()$path %>% dirname() %>% setwd()

# data is from here: https://www.isbe.net/pages/illinois-state-report-card-data.aspx

# load metadata as datasets
layout_desc <- read.xlsx("data/RC17_layout.xlsx",sheetName = "RC17"      , header = FALSE, colIndex = 1:9)
layout_assm <- read.xlsx("data/RC17_layout.xlsx",sheetName = "Assessment", header = FALSE, colIndex = 1:9)

# add column names to metadata
myNames <- c("colnum","empty","subgroup","position","length","description","type","start","end")
colnames(layout_desc) <- myNames
colnames(layout_assm) <- myNames



# load school descriptive info and demographics
desc <- read_delim("data/rc17.txt", delim = ";", col_names = FALSE)
desc <- desc[,c(1,4,5,6,7,12,14:21,54)] # select only the fields I want
layout_desc %>% filter(colnum %in% c(1,4,5,6,7,12,14:21,54)) %>% select(colnum,subgroup,description)
colnames(desc) <- c("id","nameSCH","nameDIST","city","county","type",
                    "pctWhite","pctBlack","pctHisp","pctAsian","pctPCISL","pctNativ","pctMulti",
                    "enrollment","pctLowInc")

# load school achievement info
assm <- read_delim("data/rc17_assessment.txt", delim = ";", col_names = FALSE)
assm <- assm[,c(1,4,247,250)] # select only the fields I want
layout_assm %>% filter(colnum %in% c(1,4,247,250)) %>% select(colnum,subgroup,description)
colnames(assm) <- c("id","nameSCH","PARCCpct","PARCCpct_state")

assm %>% select(PARCCpct_state) %>% unique() # all rows have 34.1 as the percent proficiency for the state


# join tables
schools <- desc %>% 
  full_join(
    assm %>% select(id,PARCCpct), 
    by="id"
  )
# has 3796 rows, same number as the assm and desc tables. good.

# cleanup <- function(x){as.numeric(str_replace_all(test,"[^0-9]",""))}

schools <- schools %>% 
  # make some columns numeric
  mutate_at(
    c("pctWhite", "pctBlack", "pctHisp", "pctAsian", "pctPCISL", "pctNativ", "pctMulti",
      "pctLowInc","PARCCpct"),
    funs(as.numeric)
  ) %>%
  mutate(
    # flag schools that did better than the state average
    didWell = ifelse(PARCCpct >= 34.1,T,F),
    # flag Chicago public schools
    CHI = ifelse(str_trim(nameDIST)=="City of Chicago SD 299",T,F)
  ) %>% 
  # reorder columns
  select(
    id, nameSCH, nameDIST, city, county, CHI, type, 
    pctWhite, pctBlack, pctHisp, pctAsian, pctPCISL, pctNativ, pctMulti, 
    pctLowInc,  
    PARCCpct, didWell
  )


# look at the data
schools %>% count(didWell)
schools %>% count(CHI)
schools %>% filter(CHI) %>% count(city,county)
schools %>% head(30) %>% View()
schools %>% count(type)
schools %>% str()


# save file
write_rds(schools, "data/schools.rds")






