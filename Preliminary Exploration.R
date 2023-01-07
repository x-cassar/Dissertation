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
#should these be remove ? if so how ? seems they havebeen surveyed twice 
#individual network stats calculation####

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
#collation of network stats and main df####
expanded_data<-merge(merge(merge(merge(merge(field_data, network_stats_parti, all.x = T),network_stats_1, all.x = T),network_stats_2, all.x = T), network_stats_3, all.x = T), network_stats_4, all.x = T)

#clustering####
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
cluster <- rbind(cluster,
          data.frame(name= network_stats %>% filter(parti_name%!in%cluster$name) %>% select(parti_name) %>% rename(name=parti_name),
           cluster=c("s17","s18","r13"))
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

#collation of clusters and main df####
expanded_data<-merge(merge(merge(merge(merge(expanded_data, cluster_parti, all.x = T),cluster_1, all.x = T),cluster_2, all.x = T), cluster_3, all.x = T), cluster_4, all.x = T)

expanded_data <- expanded_data %>% 
  mutate(across(c(cluster_parti:cluster_4), as.factor))
#adding group linls as social links####
#####
all_matrix_2<-merge(all_matrix, expanded_data %>% select(parti_name, vaccine_aware, aware_feed_benefits, improv_breed), by.x = "nodes", by.y = "parti_name", all.x=T) %>% 
  dplyr::mutate(across(c(vaccine_aware, aware_feed_benefits, improv_breed), ~ifelse(.=="TRUE", "square", "circle")))

test<-graph_from_data_frame(all_matrix_2 %>% 
                              filter(str_detect(nodes,"r")), directed=T)
E(test)$vaccine_aware
V(test)
plot(test, vertex.shape =E(test)$vaccine_aware,
     edge.arrow.size=.2, edge.color="orange",
     vertex.color="orange", vertex.frame.color="#ffffff", vertex.size=10)

#private sector variables exploration####
expanded_data %>% 
  select(contains("ext_off"), contains("ngo_proj"),contains("gov_proj")) %>% 
  #select(-contains("access")) %>% 
  skim()

expanded_data %>% 
  select(contains("info_recieved"), contains("info_source")) %>% 
  #mutate_if(is.numeric, standardize) %>% 
  skim()

library(rstatix)
expanded_data %>% 
  select(contains("helpful"), -other_formal_source_1_helpful_other) %>% 
  # mutate_if(is.numeric, log) %>% 
  # mutate_if(is.numeric, standardize) %>% 
  pivot_longer(cols =c(1:ncol(.)), names_to = "var", values_to = "val") %>% 
  drop_na() %>% 
  t_test(val~var)
#in no way are helpful variables statistically significant, so can be ignored

#bartlett test####
quantitative_vars<-expanded_data %>% 
  select(is_bare_numeric) %>% 
  select(-contains("person_"), -contains("helpful"), -contains("farmer_group"),-contains("_1"),-contains("_2"),-contains("_3"),-contains("_4"),
         -village_relationship, -family_distance, -vet_access_time,-perc_crossbreed, -degree, -eigen,
        -other_livestock_number, -price_coop,
        -contains("sell")) %>% 
  drop_na()

quantitative_vars %>% 
  cor(method = "pearson") %>% 
  corrplot()

cor_matrix<-cor(quantitative_vars)

library(psych)
cortest.bartlett(cor_matrix, n = nrow(quantitative_vars))

#KMO test####
EFAtools::KMO(  quantitative_vars)
#will result be different if we standardise?
##standardisation####
standardize <- function(a) {
  (a-mean(a, na.rm = T))/sd(a, na.rm = T)
}


standard_vars<-quantitative_vars %>% 
  filter(num_cows<10, amount_milk<400, price_market>6, market_distance<50) %>% 
  mutate_all(standardize)

standard_vars %>% 
  select(-hh_depen) %>% 
  EFAtools::KMO()

# #links through groups
# 
# expanded_data %>% select(parti_name, county, contains("group_name")) %>% 
#        pivot_longer(cols = contains("group_name"), names_to = "type", values_to = "group") %>% 
#        drop_na(group) %>% 
#   mutate_all(as.character) %>% 
#   filter(group=="sange group")
#        select(-type) %>%pivot_wider(names_from = group, values_from = parti_name)
# 
# for (g in expanded_data %>% select(parti_name, contains("group_name")) %>% 
#      pivot_longer(cols = contains("group_name"), names_to = "type", values_to = "group") %>% 
#      drop_na(group) %>% 
#      select(-type) %>% 
#      mutate(group=as.character(group)) %>%  select(group) %>% unique() %>% as.list()) {
#   
#   gg<-expanded_data %>% select(parti_name, contains("group_name")) %>% 
#     pivot_longer(cols = contains("group_name"), names_to = "type", values_to = "group") %>% 
#     drop_na(group) %>% 
#     select(-type) %>% 
#     mutate_all(as.character) %>% 
#     filter(group==g) %>% 
#     select(parti_name)
#   assign(paste0(g, "_membership"), gg)
# } 
#   
#   
#   
#   #mutate(type=1:301) %>%
#   mutate(parti_name=as.character(parti_name)) %>% 
#   pivot_wider(values_from = parti_name, names_from = group) %>% as.matrix()
# 

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
#creation of network-wide stats####
county_network_wide<-data.frame(county=character(),
                                   transitivity=numeric(),
                                centralisation=numeric(),
                                edge_density=numeric())
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
  county_network_wide_stats<-data.frame(county=county,
                                        transitivity=transitivity(c_edgelist), 
                                        centralisation= centralization.degree(c_edgelist)$centralization,
                                        edge_density= edge_density(c_edgelist))
  county_network_wide <- rbind(county_network_wide, county_network_wide_stats)
}

expanded_data<-merge(expanded_data, county_network_wide)
#education dummy####
expanded_data<-expanded_data %>% 
mutate(educ_dummy=case_when(education=="none"~0,
                            education=="none_read"~1,
                            education=="primary"~2,
                            education=="above_primary"~3))
#farm size dummy####
expanded_data<-expanded_data %>% 
  mutate(size_dummy=case_when(farm_size=="0_0.5"~1,
                              farm_size=="0.6_2.5"~2,
                              farm_size=="greater_2.6"~3,
                              farm_size=="dont_know"~NA_real_))

#county_dummy####
expanded_data<-expanded_data %>% 
  mutate(county_dummy=case_when(county=="oyugis"~1,
                              county=="rongo"~2,
                              county=="vihiga"~3,
                              county=="siaya"~4))
#household head gender####
expanded_data<-expanded_data %>% 
  mutate(hh_head_gender=case_when(decision_maker==1~parti_gender,
                                  decision_maker==0~decision_maker_gender))
#vaccine would be use####
expanded_data<-expanded_data %>% 
  mutate(vaccine_wouldbe_use=case_when(vaccine_use==1~1,
                                  vaccine_use==0&vaccine_access==1~0,
                                  vaccine_use==0&vaccine_access==0&vaccine_noaccess==1~1,
                                  vaccine_use==0&vaccine_access==0&vaccine_noaccess==0~0))
#sources number####
expanded_data<-merge(expanded_data,
                     expanded_data %>% 
                       select(parti_name, contains("info_source"), -info_source_other_2, -info_source_other_farmers) %>%
                       #mutate(across(contains("info_source"), as.logical)) 
                       # replace(is.na(.), "FALSE") %>% 
                       pivot_longer(cols = contains("info_source"), names_to = "source", values_to = "value") %>%  
                       mutate(value=as.numeric((value))) %>%
                       group_by(parti_name) %>% 
                       summarise(sources=sum(value)))

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

test_data<-expanded_data %>% 
  mutate(recieved_priv = case_when(info_recieved_ext_off==1|info_recieved_ngo_proj==1|info_recieved_coop==1|str_detect(feed_training_source, "priv")|str_detect(vaccine_benefits_source, "priv")~1,
                                   TRUE~0),
         recieved_open = case_when(info_recieved_tv==1|info_recieved_radio==1|info_recieved_newspaper==1|str_detect(feed_training_source, "open")|str_detect(vaccine_benefits_source, "open")~1,
                                   TRUE~0),
         recieved_gov=case_when(info_recieved_gov_ext==1|info_recieved_gov_proj==1|str_detect(feed_training_source, "gov")|str_detect(vaccine_benefits_source, "gov")~1,
                                TRUE~0),
         recieved_farmer=case_when(str_detect(feed_training_source, "farmer")|str_detect(vaccine_benefits_source, "farmer")~1,
                                TRUE~0)) %>% 
  mutate(vax_priv = case_when(str_detect(vaccine_benefits_source, "priv")~1,
                                   TRUE~0),
         vax_open = case_when(str_detect(vaccine_benefits_source, "open")~1,
                                   TRUE~0),
         vax_gov=case_when(str_detect(vaccine_benefits_source, "gov")~1,
                                TRUE~0),
         vax_farmer=case_when(str_detect(vaccine_benefits_source, "farmer")~1,
                                   TRUE~0)) %>% 
  mutate(feed_priv = case_when(str_detect(feed_training_source, "priv")~1,
                              TRUE~0),
         feed_open = case_when(str_detect(feed_training_source, "open")~1,
                              TRUE~0),
         feed_gov=case_when(str_detect(feed_training_source, "gov")~1,
                           TRUE~0),
         feed_farmer=case_when(str_detect(feed_training_source, "farmer")~1,
                              TRUE~0))

#same cluster variable####
#obvious - doesnt make sense 
# for (p in 1:4) {
# expanded_data %>% 
#   select(cluster_parti, paste0("person_",p),  paste0("cluster_",p)) %>% 
#   drop_na(paste0("person_",p)) %>% 
#   mutate_all(as.character) %>% 
#   mutate(cluster_match=ifelse(paste0("cluster_",p)==cluster_parti, 1, 0)) %>% print()
# }
# 
# expanded_data %>% select(in_degree) %>% 
#   ggplot()+
#   aes(x=in_degree)+
#   geom_histogram()

#same sex dummy#

#preliminary regression####

expanded_data %>% 
  select(age,hh_depen,farming_years,perc_dairy,num_cows,amount_milk,con_personal, educ_dummy, size_dummy, sources, recieved) %>% 
  drop_na() %>% 
  cor(method = "pearson") %>% 
  corrplot(method = c("number"))

reg1<-(glm(data = expanded_data %>% 
             #filter(county=="oyugis") %>% 
                mutate(across(c(info_recieved_coop:info_recieved_other, elec_access, internet_access), as.numeric)) %>% 
                mutate(milk_per_cow=amount_milk/num_cows), 
                info_source_other_farmers~ 
                age+farming_years+ parti_gender+
               #education+
                educ_dummy+
                hh_depen+#elec_access+#internet_access+
             #farm_size+
             size_dummy+
                #num_cows+amount_milk+
                milk_per_cow+
                perc_dairy+con_personal+# market_distance+ # does it make sense to include given that it is not relevant for who do not sell milk 
              price_market+
                social_capital+in_degree+
                #transitivity+edge_density+centralisation+coop_dummy+
                #county+
             county_dummy+
                recieved+#sources+
               info_recieved_vol_trainers
,family=binomial))
summary(reg1)

#dummies for educ and farm size slightly better
#milk per cow slightly better - not a problem as size captured by size dummy and imp by perc dairy

reg1$fitted.values
library(lmtest)              
lrtest(logreg2, logreg1) 
#no significant difference if county or coop+social network traits used 
#neither does adding the info sources to the model 
#education, rather than dummy, fits better
library(VGAM)
reg2<-(vglm(data = expanded_data%>% mutate(across(c(info_recieved_coop:info_recieved_other, internet_access,elec_access), as.numeric)) %>% 
              mutate(milk_per_cow=amount_milk/num_cows) %>% 
              mutate(info_source_other_farmers=as.numeric(info_source_other_farmers)), 
             in_degree~
              age+farming_years+ parti_gender+
               #education+
               educ_dummy+
               hh_depen+#elec_access+#internet_access+
               #farm_size+
               size_dummy+
               #num_cows+amount_milk+
               milk_per_cow+
               perc_dairy+con_personal+# market_distance+ # does it make sense to include given that it is not relevant for who do not sell milk 
               price_market+
               social_capital+
               #transitivity+edge_density+centralisation+coop_dummy+
               county+
               #county_dummy+
               recieved+sources+
               info_source_other_farmers
         ,tobit(Lower=0 ) ))

summary(reg2)
lmtest::bptest(reg2)
plot(reg2$residuals^2~reg2$fitted.values )
plot(reg2$fitted.values~reg2$model$in_degree)

abline(lm(reg2$fitted.values~reg2$model$in_degree))



expanded_data_w_predict<-cbind(expanded_data, data.frame(predicted=predict(reg1, data.frame(expanded_data %>% mutate(elec_access=as.numeric(elec_access),internet_access=as.numeric(internet_access))))))

summary(lm(sources~predicted, expanded_data_w_predict))

expanded_data_w_predict %>% 
  ggplot()+
  aes(y=predicted)+
  geom_histogram()+
  facet_wrap(vars(sources))

expanded_data_w_predict %>% 
  t_test(predicted~sources,detailed = T) %>% view()

#partial correlation####
pcor(expanded_data %>% select(age,hh_depen,farming_years, perc_dairy,num_cows,amount_milk,con_personal,
                           market_distance,price_market,
                           social_capital,in_degree) %>% drop_na())
#looks quite good 

#VIF####
vif(reg1)

#exporting summary####
skim_summary <- skim(expanded_data)

# #pre testing for pca####
# vars_for_pca <- c("age","hh_depen", "perc_dairy", "num_cows", "amount_milk", "con_personal","degree", "betweenness", "eigen", 
#                   "farming_years","price_coop","price_market", "market_distance")
# 
# 
# expanded_data %>% 
#   select(vars_for_pca) %>%
#   pivot_longer(cols = c(age:market_distance), names_to = "var", values_to = "val") %>% 
#   ggplot()+
#   aes(y=val)+
#   geom_boxplot()+
#   facet_wrap(vars(var), scales = "free_y")
# 
# expanded_data %>% 
#      select(vars_for_pca) %>%
#   cor(method = "pearson", use = "pairwise.complete.obs") %>% 
#   corrplot(#method = "number"
#   )
# 
# expanded_data %>% 
#   select(vars_for_pca) %>% 
#   select(-eigen) %>% 
#   EFAtools::KMO()
#   
# # 
# # pca_data <- expanded_data %>% 
# #   select(vars_for_pca) %>% 
# #   # mutate(farm_size_num=as.numeric(dplyr::case_when(farm_size=="0_0.5"~1,
# #   #                            farm_size=="0.6_2.5"~2,
# #   #                            farm_size=="greater_2.6"~3,
# #   #                            TRUE~NA_real_)),
# #   #        .keep="unused") %>%
# #   mutate_all( as.numeric) %>%
# #   mutate(#prop_age=farming_years/age,
# #     milk_per_cow = amount_milk/num_cows,
# #     milk_per_cow=ifelse(milk_per_cow=="NaN",0,milk_per_cow)
# #     #,.keep="unused"
# #   ) %>%
# #   #select(-num_cows, -second_income_none, - second_income_business#, -amount_milk) %>% 
# #   drop_na()
# 
# 
# pca_data %>% 
#   filter(market_distance<25,
#          amount_milk<450,
#          hh_depen<20,
#          milk_per_cow>0) %>% 
#   pivot_longer(cols = c(1:8), names_to = "var", values_to = "value") %>% 
#   ggplot()+
#   aes(y=value)+
#   geom_boxplot()+
#   facet_wrap(vars(var), scales = "free_y")
# 
# pca_data %>% 
#   cor(method = "pearson", use = "pairwise.complete.obs") %>% 
#   corrplot(#method = "number"
#   )
# 
# pca_data %>% 
#   # filter(market_distance<25,
#   #        amount_milk<450,
#   #        hh_depen<20,
#   #        milk_per_cow>0) %>%
#   EFAtools::KMO()
# pca_clean<-pca_data %>% 
#   filter(market_distance<25,
#          amount_milk<450,
#          hh_depen<20,
#          milk_per_cow>0)
# 
# z <-pca_clean[,-c(1,1)]
# means <- apply(z,2,mean)
# sds <- apply(z,2,sd)
# nor <- scale(z,center=means,scale=sds)
# 
# distance = dist(nor)
# 
# mydata.hclust = hclust(distance)
# plot(mydata.hclust)
# 
# mydata.hclust<-hclust(distance,method="average") 
# plot(mydata.hclust,hang=-1) 
# 
# member = cutree(mydata.hclust,3)
# table(member)
# 
# 
# raw_field %>% 
#   mutate_all(as.numeric) %>% 
#   remove_empty(c("cols")) %>% 
#   select(age:sell_market) %>% 
#   KMO()
# 
# cortest.bartlett(pca_data)
# 
# det(cor(pca_data ))
# 
# test <- prcomp(pca_data, scal =T)
# test$rotation<--1*test$rotation
# test$x <- -1*test$x
# 
# biplot(test, scale = 0)
# 
# var_explained = test$sdev^2/sum(test$sdev^2)
# qplot(c(1:8), var_explained)+
#   geom_line()
# 
# x.famd <- FAMD(pca_data)
# get_famd_var(x.famd)
# get_famd(x.famd)
# 
# info_received <-raw_field %>% 
#   select(info_recieved_coop:other_formal_source_1_other_unhelp,
#          info_source_coop:info_source_other_2) %>% 
#   remove_empty(c("cols", "rows")) %>% 
#   mutate(across(.cols =1:11, as.logical)) %>% 
#   mutate(across(.cols = 1:11, as.integer)) %>% 
#   select_if(is.numeric) %>% 
#   remove_empty(c("cols", "rows")) %>% 
#   replace(is.na(.), 0) %>% 
#   colSums() %>% 
#   as.data.frame() %>%
#   rename("received"=".") %>%
#   mutate(received=received/260) %>% 
#   arrange(desc(received)) %>% 
#   mutate(received=percent(received)) 
# 
# skim(info_source)
# info_source <- raw_field %>% 
#   select(info_recieved_coop:other_formal_source_1_other_unhelp,
#          info_source_coop:info_source_other_2, 
#          village_relationship, vaccine_aware, aware_feed_benefits,improv_breed) %>% 
#   mutate(across(.col=c(vaccine_aware:improv_breed), ~ifelse(.=="yes", "TRUE", "FALSE"))) %>% 
#   mutate(across(.cols =  c(13:35,54), as.numeric)) %>% 
#   mutate(across(.cols =  c(1:11,36:53, 55:57), as.logical)) %>% 
#   mutate(across(.cols =  c(1:11,36:53, 55:57),as.integer)) %>% 
#   remove_empty(c("cols", "rows")) %>% 
#   select(-info_other )
# 
# info_source %>% 
#   drop_na(info_recieved_ext_off) %>% 
#   group_by((as.character(info_recieved_ext_off))) %>%
#   skim(village_relationship)
# 
# #differences between private and non private####
# 
# recipients_priv<-info_source %>% 
#   #select(c(1:11,26:42)) %>% 
#   filter(info_recieved_ext_off==1|info_recieved_ngo_proj==1) %>% 
#   drop_na(info_source_agrovet ) 
# 
# skim(recipients_priv)
# 
# rec_perc_priv<-recipients_priv %>% 
#   mutate_at(vars(matches("helpful")), colMeans
#             colSums(na.rm = T) %>% 
#               as.data.frame() %>% 
#               rename(perc=".") %>% 
#               mutate(perc=perc/nrow(recipients_priv)) %>% 
#               mutate(perc=percent(perc)) %>% 
#               rename(rec_priv=perc)
#             
#             recipients_nonpriv<-info_source %>% 
#               select(c(1:11,26:42)) %>% 
#               filter(info_recieved_ext_off!=1&info_recieved_ngo_proj!=1) %>% 
#               drop_na(info_source_agrovet ) 
#             
#             
#             rec_perc_nonpriv<-recipients_nonpriv %>% 
#               colSums(na.rm = T) %>% 
#               as.data.frame() %>% 
#               rename(perc=".") %>% 
#               mutate(perc=perc/nrow(recipients_nonpriv)) %>% 
#               mutate(perc=percent(perc)) %>% 
#               rename(rec_nonpriv=perc)
#             
#             
#             source_priv<-info_source %>% 
#               select(c(1:11,26:42)) %>% 
#               filter(info_source_ext_off==1|info_source_ngo_proj==1) %>% 
#               drop_na(info_source_agrovet ) 
#             
#             
#             sou_perc_priv<-source_priv %>% 
#               colSums(na.rm = T) %>% 
#               as.data.frame() %>% 
#               rename(perc=".") %>% 
#               mutate(perc=perc/nrow(source_priv)) %>% 
#               mutate(perc=percent(perc)) %>% 
#               rename(sou_priv=perc)
#             
#             source_nonpriv<-info_source %>% 
#               select(c(1:11,26:42)) %>% 
#               filter(info_source_ext_off!=1&info_source_ngo_proj!=1) %>% 
#               drop_na(info_source_agrovet ) 
#             
#             
#             sou_perc_nonpriv<-source_nonpriv %>% 
#               colSums(na.rm = T) %>% 
#               as.data.frame() %>% 
#               rename(perc=".") %>% 
#               mutate(perc=perc/nrow(source_nonpriv)) %>% 
#               mutate(perc=percent(perc)) %>% 
#               rename(sou_nonpriv=perc)
#             
#             
#             cbind(rec_perc_priv, rec_perc_nonpriv, sou_perc_priv, sou_perc_nonpriv)
#             
#             info_source %>% 
#               select(c(1:11,26:42)) %>% 
#               filter(info_source_ext_off!=1|info_source_ngo_proj!=1) %>% 
#               drop_na(info_source_agrovet ) %>% 
#               colSums(na.rm = T)
#             
#             info_source %>% 
#               select(26:42) %>% 
#               filter(info_source_ext_off!=1|info_source_ngo_proj!=1) %>% 
#               drop_na(info_source_agrovet ) %>% 
#               colSums()
#             
#             info_source %>% 
#               select(1:11) %>% 
#               filter(info_recieved_ext_off==1|info_recieved_ngo_proj==1) %>% 
#               drop_na(info_recieved_coop ) %>% 
#               colSums()
#             
#             info_source %>% 
#               select(1:11) %>% 
#               filter(info_recieved_ext_off!=1|info_recieved_ngo_proj!=1) %>% 
#               drop_na(info_recieved_coop ) %>% 
#               colSums()
#             
#             select_if(is.numeric) %>% 
#               remove_empty(c("cols", "rows")) %>% 
#               #replace(is.na(.), 0) %>% 
#               colSums() %>% 
#               as.data.frame() %>%
#               rename("source"=".") %>%
#               mutate(source=source/260) %>% 
#               arrange(desc(source)) %>% 
#               mutate(source=percent(source)) 
#             
#             
#             
#             raw_field %>% 
#               select(farmer_group_repeat_1_group_name) %>% 
#               mutate_all(as.factor) %>% 
#               mutate(farmer_group_repeat_1_group_name=str_to_lower(farmer_group_repeat_1_group_name)) %>% 
#               drop_na(farmer_group_repeat_1_group_name) %>% 
#               group_by(farmer_group_repeat_1_group_name) %>%
#               mutate(farmer_group_repeat_1_group_name=ifelse(str_detect(farmer_group_repeat_1_group_name, "acre|arce"), "one acre fund", farmer_group_repeat_1_group_name)) %>% 
#               summarise(instances = length(farmer_group_repeat_1_group_name)) %>% 
#               arrange(desc(instances)) %>% 
#               view()
#             
#             edges<-raw_field %>% 
#               select(parti_name, person_1, person_2, person_3, person_4) %>% 
#               pivot_longer(cols = c(person_1:person_4), names_to = "x", values_to = "edge") %>% 
#               select(-x) %>% 
#               drop_na(edge)
#             
#             
#             nodes <- raw_field %>% select(parti_name) %>% unique()
#             