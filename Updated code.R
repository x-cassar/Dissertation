#packages and key functions####
library(tidyverse)
library(janitor)
library(skimr)
library(ggplot2)
library(corrplot) # for correlation plots
library(scales) # to add scales to plots e.g. percentages
library(ggforce) # lots of plotting stuff
library(knitr) # for tables
#library(hexbin) # for tables
library(janitor) # to clean data and in this code to add percentages and totals
library(readxl)
library(lubridate)
library(writexl)
library(igraph)
'%!in%' <- function(x,y)!('%in%'(x,y))
#data import####
field_data <- read_csv("field_data.csv") %>% 
  mutate_if(is.character, as.factor)

##minor clear####
field_data <- field_data %>% 
  mutate(price_coop=na_if(price_coop, 0)) %>% 
  filter(parti_name!="sp009") %>% 
  filter(parti_name!="rp094")

#network stats and clustering####
##individual network stats calculation####

network_stats<-data.frame(parti_name=character(),
                          degree=numeric(),
                          betweenness=numeric(),
                          eigen=numeric())

all_matrix <- data.frame(nodes=character(),
                         edges=character())
for (county in c("siaya","vihiga", "rongo", "oyugis")) {
  c_matrix <- field_data %>% 
    rename(county2=county) %>% 
    filter(county2==paste(county)) %>% 
    select(parti_name, person_1, person_2, person_3, person_4) %>% 
    mutate_all(as.character) %>% 
    pivot_longer(cols = c(person_1, person_2, person_3, person_4), names_to = "var", values_to = "edges") %>% 
    select(-var) %>% 
    rename(nodes=parti_name) %>% 
    drop_na() %>% 
    unique() %>% 
    as.matrix()
  c_edgelist<-graph.edgelist(c_matrix)
  all_matrix <- rbind(all_matrix, as.data.frame(c_matrix))
  county_network_stats<-  cbind(rownames_to_column(as.data.frame(degree(c_edgelist)), var="parti_name"),
                                as.data.frame(degree(c_edgelist, mode = c("in"))),
                                as.data.frame(degree(c_edgelist, mode = c("out"))),
                                as.data.frame(betweenness(c_edgelist, directed = FALSE)),
                                as.data.frame(evcent(c_edgelist)$vector)) %>%
    rename(degree=2, in_degree=3,out_degree=4, betweenness=5, eigen=6)
  
  network_stats <- rbind(network_stats, county_network_stats)
}

#three participants are are not connected to anyone, below is to include them
network_stats <- rbind(network_stats,
                       field_data %>% 
                         select(parti_name, person_1, person_2, person_3, person_4) %>% 
                         mutate_all(as.character) %>% 
                         pivot_longer(cols = c(parti_name, person_1, person_2, person_3, person_4), names_to = "var", values_to = "parti_name") %>% 
                         select(parti_name) %>% 
                         unique() %>% 
                         drop_na() %>% 
                         filter(parti_name%!in%network_stats$parti_name) %>% 
                         mutate(degree=0,
                                in_degree=0,
                                out_degree=0,
                                betweenness=0,
                                eigen=NA) 
)

network_stats_parti<-network_stats %>% 
  filter(parti_name%in%field_data$parti_name)

network_stats_1<-network_stats %>% 
  filter(parti_name%in%field_data$person_1)%>% 
  rename(person_1=parti_name,
         degree_1=2, in_degree_1=3, out_degree_1=4, betweenness_1=5, eigen_1=6)

network_stats_2<-network_stats %>% 
  filter(parti_name%in%field_data$person_2)%>% 
  rename(person_2=parti_name,
         degree_2=2, in_degree_2=3, out_degree_2=4, betweenness_2=5, eigen_2=6)

network_stats_3<-network_stats %>% 
  filter(parti_name%in%field_data$person_3)%>% 
  rename(person_3=parti_name,
         degree_3=2, in_degree_3=3, out_degree_3=4, betweenness_3=5, eigen_3=6)

network_stats_4<-network_stats %>% 
  filter(parti_name%in%field_data$person_4) %>% 
  rename(person_4=parti_name,
         degree_4=2, in_degree_4=3, out_degree_4=4, betweenness_4=5, eigen_4=6)
###collation of individual network stats and main df####
expanded_data<-merge(merge(merge(merge(merge(field_data, network_stats_parti, all.x = T),network_stats_1, all.x = T),network_stats_2, all.x = T), network_stats_3, all.x = T), network_stats_4, all.x = T)

##clustering####
#assigning cluster
cluster <- data.frame(name=character(),
                      cluster=character())
for (county in c("siaya","vihiga", "rongo", "oyugis")) {
  c_matrix <- field_data %>% 
    rename(county2=county) %>% 
    filter(county2==paste(county)) %>% 
    select(parti_name, person_1, person_2, person_3, person_4) %>% 
    mutate_all(as.character) %>% 
    pivot_longer(cols = c(person_1, person_2, person_3, person_4), names_to = "var", values_to = "edges") %>% 
    select(-var) %>% 
    rename(nodes=parti_name) %>% 
    drop_na() %>% 
    unique() %>% 
    as.matrix()
  c_edgelist<-graph.edgelist(c_matrix)
  modularity_max <- as.data.frame(matrix(ncol=1, nrow=50))
  for (a in 1:50) {
    modularity_max$V1[a]<-modularity(cluster_walktrap(c_edgelist, steps = a))
    
  }
  cluster_c<-cluster_walktrap(c_edgelist, steps = which.max(modularity_max$V1))
  # modularity(cluster_c)
  # length(cluster_c)
  # sizes(cluster_c)
  # crossing(cluster_c,c_edgelist)
  # plot(cluster_c,c_edgelist)
  # plot_dendrogram(cluster_c)
  cluster_membership<-data.frame(name=cluster_c$names,
                                 cluster=cluster_c$membership) %>% 
    mutate(cluster=str_pad(cluster,2,side = c("left"), "0")) %>% 
    mutate(cluster=paste0(substr(county, 1, 1), cluster))
  cluster <- rbind(cluster, cluster_membership)
}
#missing ones, as above, assigned as independent cluster given subsequent number in each county
cluster <- rbind(cluster,
                 data.frame(name= network_stats %>% filter(parti_name%!in%cluster$name) %>% select(parti_name) %>% rename(name=parti_name),
                            cluster=c("s16","s17","r10"))
)

cluster_parti<-cluster %>% 
  filter(name%in%field_data$parti_name) %>% 
  rename(parti_name=name,cluster_parti=cluster)


cluster_1<-cluster %>% 
  filter(name%in%field_data$person_1)%>% 
  rename(person_1=name,cluster_1=cluster)

cluster_2<-cluster %>% 
  filter(name%in%field_data$person_2)%>% 
  rename(person_2=name,cluster_2=cluster)

cluster_3<-cluster %>% 
  filter(name%in%field_data$person_3)%>% 
  rename(person_3=name,cluster_3=cluster)


cluster_4<-cluster %>% 
  filter(name%in%field_data$person_4) %>% 
  rename(person_4=name,cluster_4=cluster)

###collation of clusters and main df####
expanded_data<-merge(merge(merge(merge(merge(expanded_data, cluster_parti, all.x = T),cluster_1, all.x = T),cluster_2, all.x = T), cluster_3, all.x = T), cluster_4, all.x = T)

expanded_data <- expanded_data %>% 
  mutate(across(c(cluster_parti:cluster_4), as.factor))


#other variables####
##household head gender####
expanded_data<-expanded_data %>% 
  mutate(hh_head_gender=case_when(decision_maker==1~parti_gender,
                                  decision_maker==0~decision_maker_gender))

#creation of social capital variable####
social_capital<-expanded_data %>% 
  #mutate(social_capital=0) %>% 
  mutate(across(c(out_degree,farmer_group_repeat_1_group_mem_relationship,farmer_group_repeat_2_group_mem_relationship, village_relationship), replace_na, replace=2.5)) %>% 
  mutate(across(c(finance_group_repeat_1_finance_group_name,finance_group_repeat_2_finance_group_name,finance_group_repeat_3_finance_group_name,social_group_repeat_1_social_groups_info,social_group_repeat_2_social_groups_info),~ifelse(is.na(.) == T, 0, 1))) %>%
  mutate(social_capital=out_degree+
           (farmer_group_repeat_1_group_mem_relationship-3) +(farmer_group_repeat_2_group_mem_relationship-3)+
           (village_relationship-3)+
           finance_group_repeat_1_finance_group_name+finance_group_repeat_2_finance_group_name+finance_group_repeat_3_finance_group_name+
           social_group_repeat_1_social_groups_info+social_group_repeat_2_social_groups_info+
           as.numeric(religious_comm)+
           ( as.numeric(family_village)*((2*family_relationship)-1))#to get 1 and -1
  ) %>% 
  select(social_capital)

expanded_data <- cbind(expanded_data, social_capital)

#cooperative present variable####
expanded_data %>% 
  group_by(county) %>% 
  select(price_coop) %>% 
  skim()

expanded_data<-expanded_data %>% 
  mutate(coop_dummy = ifelse(is.na(price_coop), 0, 1))

#vaccine would be use####
expanded_data<-expanded_data %>% 
  mutate(vaccine_wouldbe_use=case_when(vaccine_use==1~1,
                                       vaccine_use==0&vaccine_access==1~0,
                                       vaccine_use==0&vaccine_access==0&vaccine_noaccess==1~1,
                                       vaccine_use==0&vaccine_access==0&vaccine_noaccess==0~0))
#received info####
expanded_data<-merge(expanded_data,
                     expanded_data %>% 
                       select(parti_name, contains("info_recieved")) %>%
                       #mutate(across(contains("info_recieved"), as.logical)) 
                       # replace(is.na(.), "FALSE") %>% 
                       pivot_longer(cols = contains("info_recieved"), names_to = "recieved", values_to = "value") %>%  
                       mutate(value=as.numeric((value))) %>%
                       group_by(parti_name) %>% 
                       summarise(recieved=sum(value)))
#info source grouping####
expanded_data<-expanded_data %>% separate(feed_training_source, c("feed_source_a","feed_source_b","feed_source_c"), sep=";") %>% 
  separate(vaccine_benefits_source, c("vaccine_source_a","vaccine_source_b","vaccine_source_c" ), sep=";") %>% 
  #select("feed_source_a","feed_source_b","feed_source_c","vaccine_source_a","vaccine_source_b","vaccine_source_c") %>% 
  mutate(across(c("feed_source_a","feed_source_b","feed_source_c","vaccine_source_a","vaccine_source_b","vaccine_source_c"),
                ~case_when(str_detect(., "cooperative|dairy group|vet|fips|ilri|heifer|cooperative|icipe|naric|ngo|other extension")~"recieved_priv",
                           str_detect(., "other farmer|farmers group|elder|lead farmer|chief|baraza|witnessed")~"recieved_farmer",
                           str_detect(., "government|kcap")~"recieved_gov",
                           str_detect(., "television|newspaper|radio|book")~"recieved_open"),
                TRUE~NA_real_ )) %>% 
  unite("feed_training_source",c(feed_source_a,feed_source_b,feed_source_c), sep = ";", na.rm = T) %>% 
  unite("vaccine_benefits_source",c(vaccine_source_a,vaccine_source_b,vaccine_source_c), sep = ";", na.rm = T)

#making data readable####
expanded_data %>% 
  select(-age_check, -dairy_check, ) %>% 
  rename() %>% 

field_data$feed_used

summary(glm(data = field_data,
    feed_used~county+hh_depen+farming_years))
