library(tidyverse)
library(janitor)
library(skimr)
library(ggplot2)
library(corrplot) # for correlation plots
library(scales) # to add scales to plots e.g. percentages
library(ggforce) # lots of plotting stuff
library(knitr) # for tables
library(hexbin) # for tables
library(janitor) # to clean data and in this code to add percentages and totals
library(readxl)
library(lubridate)
library(writexl)
library(readxl)
setwd("C:/Users/Xandru/OneDrive/PS3/Placement/Rosie PHD")

raw_field <- read.csv("all_data.csv") %>%
  clean_names() 

raw_field[raw_field=="n/a"|raw_field=="999"|raw_field=="700000000"|raw_field=="9999"]<-NA

excess_title <- as.data.frame(as.matrix(c("basic_info_", "criteria_check_", "personal_info_",
                                          "milk_sell_milk_","info_groups_","formal_networks_info_","informal_networks_",
                                          "feed_fodder_", "animal_health_", "breeding_", "animal_health_",
                                          "info_transmission_")))


for (m in 1:nrow(excess_title)) {
  colnames(raw_field)<-ifelse(str_detect(colnames(raw_field), excess_title$V1[m]),
                              gsub(excess_title$V1[m], "", colnames(raw_field)),
                              colnames(raw_field))
}
skim(raw_field)

raw_field%>% 
  mutate(enum_name=str_to_lower(enum_name)) %>% 
  filter(str_detect(enum_name,"steve"))

raw_field <- raw_field%>% 
  mutate(enum_name=str_to_lower(enum_name)) %>% 
  rowwise() %>% 
          mutate(enum_name = case_when(str_detect(enum_name,"steve")==T ~ "steve",
                                         # str_detect(enum_name,"eunice")==T~"eunice",
                                         # str_detect(enum_name,"valerie")==T~"valerie",
                                         # str_detect(enum_name,"virginia")==T~"virginia",
                                         # str_detect(enum_name,"valentine")==T~"valentine",
                                         TRUE~NA)) 
  drop_na(enum_name)



raw_field$enum_name <- as.character(ifelse(str_detect(raw_field$enum_name, "steve"),
                                           "steve",
                                           ifelse(str_detect(raw_field$enum_name, "eunice"),
                                                  "eunice",
                                                  ifelse(str_detect(raw_field$enum_name, "valerie"),
                                                         "valerie",
                                                         ifelse(str_detect(raw_field$enum_name, "virginia"),
                                                                "virginia",
                                                                ifelse(str_detect(raw_field$enum_name, "valentine"),
                                                                       "valentine", NA))))))






info_vars <-raw_field %>% 
  select(info_recieved_coop:other_formal_source_1_other_unhelp) %>% 
  remove_empty(c("cols", "rows")) %>% 
  mutate(across(.cols = 1:11, as.logical)) %>% 
  mutate(across(.cols = 1:11, as.integer))

skim(info_vars)

info_vars<-info_vars %>% 
  rowwise() %>% 
  mutate(empty=sum(c_across(cols = 1:11)))
