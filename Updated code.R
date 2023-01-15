#packages and key functions####
library(tidyverse)
library(janitor)
library(skimr)
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
field_data <- read_csv("field_data.csv") 

##minor clear####
field_data <- field_data %>% 
  mutate(price_coop=na_if(price_coop, 0)) %>% 
  filter(parti_name!="sp009") %>% 
  filter(parti_name!="rp094") %>% 
  mutate(home_location=ifelse(home_location=="chamakanga welfare group","chamakanga", home_location))%>% 
  mutate_if(is.character, as.factor)

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
##vaccine would be use####
expanded_data<-expanded_data %>% 
  mutate(vaccine_wouldbe_use=as.logical(case_when(vaccine_use==1~1,
                                       vaccine_use==0&vaccine_access==1~0,
                                       vaccine_use==0&vaccine_access==0&vaccine_noaccess==1~1,
                                       vaccine_use==0&vaccine_access==0&vaccine_noaccess==0~0)))
##specific feed use####
rep_str <- c("calliandra resmodium nappier","desmodium nappier","license callander","nappier bracharia desmodium","nappier desmodium bracharia","nappies,bracharia maize,rhodes","nappier rhodes, desmodium")

expanded_data<-expanded_data %>% 
  mutate(feed_used_specific=as.character(feed_used_specific)) %>% 
  mutate(feed_used_specific=ifelse(feed_used_specific%in%rep_str, str_replace_all(feed_used_specific, " ",";"), feed_used_specific)) %>% 
  mutate(feed_used_specific=str_remove_all(feed_used_specific,"grass")) %>% 
  mutate(feed_used_specific=na_if(feed_used_specific,"helps to increase milk production"),
         feed_used_specific=na_if(feed_used_specific,"name not known"),
         feed_used_specific=str_replace_all(feed_used_specific," and ", ";"),
         feed_used_specific=str_replace_all(feed_used_specific,",", ";")
         ) %>%
  separate(feed_used_specific, sep =  ";" ,c("feed_1","feed_2","feed_3","feed_4")) %>% 
  mutate(across(c(feed_1:feed_4),~case_when(str_detect(.,"kaka")~"kakamenga 1 (napier)",
                                            (str_detect(.,"nappi")|str_detect(.,"napi"))~"napier",
                                            (str_detect(.,"desmo")|str_detect(.,"modium"))~"desmodium",
                                            (str_detect(.,"brac")|str_detect(.,"aria"))~"bracchiaria",
                                            (str_detect(.,"rode")|str_detect(.,"rhode"))~"rhodes grass",
                                            (str_detect(.,"cali")|str_detect(.,"call"))~"calliandra",
                                            (str_detect(.,"license")|str_detect(.,"luce")|str_detect(.,"lucian"))~"tree lucerne",
                                            str_detect(.,"maize")~"maize",
                                            str_detect(.,"lato")~"mulato",#check what the hell this is - shows up twice?
                           TRUE~NA_character_))) %>% 
  unite("specific_feed_used", feed_1:feed_4, na.rm = T,sep = ";") %>% 
  mutate(specific_feed_used=na_if(specific_feed_used, "")) 

##household head gender####
expanded_data<-expanded_data %>% 
  mutate(hh_head_gender=case_when(decision_maker==1~parti_gender,
                                  decision_maker==0~decision_maker_gender))

#clearing variable names and reordering####
expanded_data<-expanded_data %>% 
  select(-age_check, -dairy_check) %>% 
  rename() %>% 
  select(#people
         parti_name,
         person_1:person_4,
         #tech use
         vaccine_use, vaccine_wouldbe_use,
         feed_used, specific_feed_used,
         breed_service_ai,
         #tech knowledge
         vaccine_aware,
         aware_feed_benefits, #issue w cooperative [2], g [247],
         improv_breed, #not awareness of ai per se but of crossbreeding
         #tech recomm
         vaccine_recomm, 
         feed_recommend,
         ai_recommend,
         #individual and hh traits
         hh_head_gender, parti_gender, age,  education, hh_depen, county, village_name, home_location,
         #economic traits
         farming_years, farm_size, num_cows, amount_milk,con_personal,
         sell_coop, sell_market, perc_dairy,price_coop, price_market, 
         transport_market, market_distance,
         other_livestock, other_livestock_type,other_livestock_number,
         second_income_none, second_income_employment:second_income_other_farming,
         #network measures
         degree:eigen, cluster_parti,
         contains("info_source"),
         contains("info_recieved"),
         contains("helpful"),
         everything()) %>% 
  rename(feed_use=feed_used,
         ai_use=breed_service_ai,
         feed_aware=aware_feed_benefits,
         crossbreed_aware=improv_breed,
         vaccine_recommend=vaccine_recomm)

#network mapping####
library(visNetwork)

test_nodes<-expanded_data %>% select(parti_name, person_1:person_4) %>% 
  pivot_longer(cols = c(1:5),
               names_to = "var",
               values_to = "label") %>% 
  select(2) %>%
  filter(str_detect(label,"o")) %>% 
  group_by(label) %>% 
  count() %>% 
  mutate(label=as.character(label)) %>% 
  #mutate(label=ifelse((str_detect(label,"x")&n=="1"),NA, label)) %>% 
  select(label) %>% 
  drop_na(label) %>% 
  arrange(label) %>%
  mutate(id=label) %>% 
  select(id,label) %>% 
  mutate(color.background=ifelse(str_detect(label,"p"),"tomato","slategray"))
  

test_edges<-expanded_data %>% select(parti_name, person_1:person_4) %>% 
  mutate_all(as.character) %>% 
  pivot_longer(cols = c(2:5),
               names_to = "var",
               values_to = "to") %>% 
  select(-var) %>% 
  rename("from"=1) %>% 
  filter(to%in%test_nodes$label)

visNetwork(test_nodes,test_edges)

network_map <- expanded_data %>% 
  select("parti_name", 
         "person_1",
         "person_2",
         "person_3",
         "person_4"
  )

edge_fun <- function(a){
  network_map %>% 
    select(parti_name, a) %>% 
    rename(nodes = parti_name, edges = a)
}

for (o in 1:ncol(network_map %>% select(-1))) {
  edge_cols <- as.data.frame(colnames(network_map %>% select(-1)))
  nums <- as.data.frame(1:ncol(network_map%>% select(-1)))
  nam <- paste("edges", as.character(nums$`1:ncol(network_map %>% select(-1))`[o]), sep = "")
  assign(nam, (edge_fun(edge_cols$`colnames(network_map %>% select(-1))`[o])))
}


edges_final <- mget(ls(pattern="edges")) %>%
  bind_rows()

edges_final[edges_final=="n/a"|edges_final=="999"|
              edges_final=="other farmer"]<-NA
edges_final <- na.omit(edges_final)


nodes_1 <- edges_final %>% 
  select(nodes) 
nodes_2<- edges_final %>% 
  select(edges) %>% 
  rename(nodes = edges)

nodes_final <- rbind(nodes_1, nodes_2)
nodes_final <- nodes_final %>%  distinct()
nodes_final[nodes_final=="n/a"|nodes_final=="999"|nodes_final=="9999"]<-NA
nodes_final <- na.omit(nodes_final)

surveyed <- expanded_data %>% select(1)
nodes_final <- nodes_final %>% 
  mutate(type = ifelse
         (as.character(nodes_final$nodes)%in%as.character(surveyed$parti_name),
           "surveyed", 
           ifelse(as.character(nodes_final$nodes)%in%info$groups,
                  "info_source",
                  "unsurveyed")))


nodes<- nodes_final %>% rowid_to_column("id") %>% 
  mutate(id = id-1)

edges<- edges_final %>% 
  left_join(nodes, by=c("nodes"="nodes")) %>% 
  rename(from=id)

edges <- edges %>% 
  left_join(nodes, by=c("edges"="nodes")) %>% 
  rename(to=id) %>% 
  select(from, to)

nodes <- nodes %>% 
  rename(group=type)

mentions <-(edges %>%group_by(to) %>% count())

for (p in 1:nrow(nodes)) {
  nodes$size[p] <- (ifelse((nodes$id[p]%in%mentions$to), 
                           (as.numeric(mentions$n[which(mentions$to==nodes$id[p])])), 
                           0.5))
}


nodes <- nodes %>% 
  mutate(size=size*10) %>% 
  rename(label=nodes)

nodes$shape <- ifelse(nodes$group=="surveyed", 
                      "square", 
                      ifelse(nodes$group=="unsurveyed", 
                             "triangle", 
                             "pentagon"))

oyugis_net <- visNetwork(nodes=nodes, edges = edges)
visSave(oyugis_net, file="oyugis_net.html", background = "white")

field$vaccine_use[is.na(field$vaccine_use)]<-0
field$vaccine_aware[is.na(field$vaccine_aware)]<-0
# 
for (p in 1:nrow(nodes)) {
  nodes$group[p]<-ifelse(nodes$shape[p]=="pentagon", 
                         "not applicable",
                         ifelse(!(nodes$label[p]%in%field$parti_name),
                                "unknown",
                                ifelse(as.numeric(field$vaccine_use[which(field$parti_name==nodes$label[p])])==1,
                                       "vaccinated",
                                       ifelse(as.numeric(field$vaccine_aware[which(field$parti_name==nodes$label[p])])==1,
                                              "aware",
                                              "unaware"))))
}

# for (p in 1:nrow(nodes)) {
#   nodes$group[p]<-ifelse(nodes$shape[p]=="pentagon", 
#                                          "not applicable",
#     ifelse(!(nodes$label[p]%in%field$parti_name),
#                          "unknown",
#                          ifelse(as.numeric(field$feed_used[which(field$parti_name==nodes$label[p])])==1,
#                                 "improved fodder",
#                                 ifelse(as.numeric(field$aware_feed_benefits[which(field$parti_name==nodes$label[p])])==1,
#                                        "aware",
#                                        "unaware"))))
# }
# 
# for (p in 1:nrow(nodes)) {
#   nodes$group[p]<-ifelse(nodes$shape[p]=="pentagon", 
#                                          "not applicable",
#     ifelse(!(nodes$label[p]%in%field$parti_name),
#                          "unknown",
#                          ifelse(as.numeric(field$perc_crossbreed[which(field$parti_name==nodes$label[p])])==1,
#                                 "improved breed",
#                                 ifelse(as.numeric(field$improv_breed[which(field$parti_name==nodes$label[p])])==1,
#                                        "aware",
#                                        "unaware"))))
# }


visNetwork(nodes=nodes, edges = edges) %>% 
  visLegend()

nodes_bra <- nodes 
for (p in 1:nrow(nodes_bra)) {
  nodes_bra$group[p]<-ifelse(nodes$shape[p]=="pentagon", 
                             "not applicable",
                             ifelse(!(nodes_bra$label[p]%in%field$parti_name),
                                    "unknown",
                                    ifelse((field$improved_feed_bracchiaria[which(field$parti_name==nodes_bra$label[p])])==1,
                                           "braccharia",
                                           "NO bracchiara")))
}
visNetwork(nodes=nodes_bra, edges = edges) %>% 
  visLegend()

nodes_des <- nodes 
for (p in 1:nrow(nodes_des)) {
  nodes_des$group[p]<-ifelse(nodes$shape[p]=="pentagon", 
                             "not applicable",
                             ifelse(!(nodes_des$label[p]%in%field$parti_name),
                                    "unknown",
                                    ifelse((field$improved_feed_desmodium[which(field$parti_name==nodes_des$label[p])])==1,
                                           "desmodium",
                                           "NO desmodium")))
}
visNetwork(nodes=nodes_des, edges = edges) %>% 
  visLegend()


nodes_rho <- nodes 
for (p in 1:nrow(nodes_rho)) {
  nodes_rho$group[p]<-ifelse(nodes$shape[p]=="pentagon", 
                             "not applicable",
                             ifelse(!(nodes_rho$label[p]%in%field$parti_name),
                                    "unknown",
                                    ifelse((field$improved_feed_rhodes_grass[which(field$parti_name==nodes_rho$label[p])])==1,
                                           "rhodes grass",
                                           "NO rhodes grass")))
}
visNetwork(nodes=nodes_rho, edges = edges) %>% 
  visLegend()

nodes_mai <- nodes 
for (p in 1:nrow(nodes_mai)) {
  nodes_mai$group[p]<-ifelse(nodes$shape[p]=="pentagon", 
                             "not applicable",
                             ifelse(!(nodes_mai$label[p]%in%field$parti_name),
                                    "unknown",
                                    ifelse((field$improved_maize[which(field$parti_name==nodes_mai$label[p])])==1,
                                           "maize",
                                           "NO maize")))
}
visNetwork(nodes=nodes_mai, edges = edges) %>% 
  visLegend()



library(igraph)
g0_nodes<-nodes %>% filter(shape!="pentagon") %>% 
  select(label)
g0 <- graph_from_data_frame(d=edges_final %>% filter(edges_final$edges%in%g0_nodes$label), vertices = (g0_nodes))
plot.igraph(g0)

centrality <- function(a){
  igdegree<-cbind(as.data.frame((degree(a))),
                  as.data.frame((degree(a, mode = "in"))),
                  as.data.frame((degree(a, mode = "out"))))
  colnames(igdegree)<-c("total", "in", "out")
  
  igstrength <- as.data.frame(strength(a))
  names(igstrength)<-c("strength")
  
  igcloseness <- as.data.frame(closeness(a, normalized=TRUE))
  names(igcloseness)<-c("closeness")
  
  igbetweenness <- as.data.frame(betweenness(a))
  names(igbetweenness)<-c("betweenness")
  
  igeigenvector <- as.data.frame(eigen_centrality(a)$vector)
  names(igeigenvector) <- c("Eigenvector centrality")
  
  igpagerank <- as.data.frame(page_rank(a)$vector)
  names(igpagerank)<-c("page rank")
  
  igauthority <- as.data.frame(authority_score(a)$vector)
  names(igauthority)<-c("authority")
  
  print(cbind(g0_nodes$label, igdegree,igstrength, igcloseness,
              igbetweenness, igeigenvector, igpagerank, igauthority))
  
}
g0_centrality <- centrality(g0)

write_xlsx(g0_centrality,"oyugis centrality.xlsx")


net_properties <- function(g){
  
  diameter_ <- diameter(g, directed = F, weights = NA)
  
  mean_distance_ <- mean_distance(g, directed = F)
  
  edge_density_ <- edge_density(g)
  
  transitivity_ <- transitivity(g)
  
  print(as.data.frame(cbind(diameter_, mean_distance_, edge_density_, transitivity_)))
  
}
g0_properties<- as.data.frame(net_properties(g0))

write_xlsx(g0_properties, "oyugis network properties.xlsx")

ig_nodes <- nodes %>% filter(shape!="pentagon")
ig_nodes$label <- ifelse(ig_nodes$shape=="square",
                         ig_nodes$label,
                         ifelse(ig_nodes$size<11,
                                NA,
                                ig_nodes$label))

ig_nodes <- ig_nodes %>% drop_na(label) %>% select(label)

ig_edges<- edges_final %>% filter(edges_final$edges%in%ig_nodes$label)

g1 <- graph_from_data_frame(d=ig_edges, vertices = ig_nodes)

modularity_max <- as.data.frame(matrix(ncol=1, nrow=50))
for (a in 1:50) {
  modularity_max$V1[a]<-modularity(cluster_walktrap(g1, steps = a))
  
}

c1 = cluster_walktrap(g1, steps = which.max(modularity_max$V1))
modularity(c1)
length(c1)
sizes(c1)
crossing(c1,g1)
plot(c1,g1)
plot_dendrogram(c1)
group_membership <- as.data.frame(c1$membership) %>% 
  rename(group='c1$membership')

group_nodes <- nodes %>% filter(nodes$label%in%ig_nodes$label)
group_nodes$group <- group_membership$group
group_nodes <- group_nodes[order(group_nodes$group),]

group_edges <- edges %>% filter(edges$to%in%group_nodes$id)

visNetwork(nodes=group_nodes, edges = group_edges) %>% 
  visLegend()

#qualitative variable cleaning taken from paper 2 file####
qual <- c("no_feed_use",
          "feed_no_recomm",
          "hay_benefits_yes",
          "hay_making_no_recomm",
          "vaccine_no_reason",
          "vaccine_no_recomm",
          "ai_recommend_no",
          "hired_recommend_no")
raw_field[qual] <- lapply(raw_field[qual], gsub, pattern=",", replacement=";")
raw_field[qual] <- lapply(raw_field[qual], gsub, pattern="  ", replacement=" ")
raw_field[qual] <- lapply(raw_field[qual], gsub, pattern=" and ", replacement=";")
raw_field[qual] <- lapply(raw_field[qual], gsub, pattern=" ;", replacement=";")
raw_field[qual] <- lapply(raw_field[qual], gsub, pattern="; ", replacement=";")

raw_field <- raw_field %>% separate(no_feed_use, c("no_feed_use_1",
                                                   "no_feed_use_2"), ";")
raw_field <- raw_field %>% separate(hay_benefits_yes, c("hay_benefits_yes_1",
                                                        "hay_benefits_yes_2"), ";")

raw_field$no_feed_use_1 <- ifelse(str_detect(raw_field$no_feed_use_1, ("expens|cost|finance")),
                                  "expensive", 
                                  ifelse(str_detect(raw_field$no_feed_use_1, ("cow|free")),
                                         "unimproved or free range cow/s",
                                         ifelse(str_detect(raw_field$no_feed_use_1, ("idea|know")),
                                                "lack of knowledge",
                                                ifelse(str_detect(raw_field$no_feed_use_1, ("seed|available|do not have|don't have|doesn't have|don't own|don't produce|doesn't grow")),
                                                       "no access to seed",
                                                       ifelse(str_detect(raw_field$no_feed_use_1, ("land|size|space|small|limited|lznd")),
                                                              "limited space", 
                                                              ifelse(str_detect(raw_field$no_feed_use_1, "time"),
                                                                     "not enough time",
                                                                     "other"))))))


raw_field$no_feed_use_2 <- ifelse(str_detect(raw_field$no_feed_use_2, ("expens|cost|finance")),
                                  "expensive", 
                                  ifelse(str_detect(raw_field$no_feed_use_2, ("cow|free")),
                                         "unimproved or free range cow/s",
                                         ifelse(str_detect(raw_field$no_feed_use_2, ("idea|know")),
                                                "lack of knowledge",
                                                ifelse(str_detect(raw_field$no_feed_use_2, ("seed|available")),
                                                       "no access to seed",
                                                       ifelse(str_detect(raw_field$no_feed_use_2, ("land|size|space|small|limited")),
                                                              "limited space", 
                                                              ifelse(str_detect(raw_field$no_feed_use_2, "time"),
                                                                     "not enough time",
                                                                     "other"))))))

raw_field$feed_no_recomm <- ifelse(str_detect(raw_field$feed_no_recomm, ("expens|cost|finance")),
                                   "expensive", 
                                   ifelse(str_detect(raw_field$feed_no_recomm, ("cow|free")),
                                          "unimproved or free range cow/s",
                                          ifelse(str_detect(raw_field$feed_no_recomm, ("idea|know")),
                                                 "lack of knowledge",
                                                 ifelse(str_detect(raw_field$feed_no_recomm, ("seed|available")),
                                                        "no access to seed",
                                                        ifelse(str_detect(raw_field$feed_no_recomm, ("land|size|space|small|limited")),
                                                               "limited space", 
                                                               ifelse(str_detect(raw_field$feed_no_recomm, "time"),
                                                                      "not enough time",
                                                                      raw_field$feed_no_recomm))))))

raw_field$hay_benefits_yes_1 <- ifelse(str_detect(raw_field$hay_benefits_yes_1, ("expens|cost|finance|capital|funds|sive")),
                                       "expensive", 
                                       ifelse(str_detect(raw_field$hay_benefits_yes_1, ("no enough|not enough|doesn't have enough|lack of enough|doesn't grow|do not have enough|no fodder|no feeds|resource|material")),
                                              "no/too little fodder",
                                              ifelse(str_detect(raw_field$hay_benefits_yes_1, ("know|skill|practice|aware|capacity|no idea|knwo")),
                                                     "lack of knowledge",
                                                     ifelse(str_detect(raw_field$hay_benefits_yes_1, "difficult|time|tedious|ectic"),
                                                            "not enough time/too difficult",
                                                            ifelse(str_detect(raw_field$hay_benefits_yes_1, ("small|room|space")),
                                                                   "limited space", 
                                                                   "other")))))

raw_field$hay_benefits_yes_2 <- ifelse(str_detect(raw_field$hay_benefits_yes_2, ("financ|capital")),
                                       "expensive",
                                       ifelse(str_detect(raw_field$hay_benefits_yes_2, "difficult|time|tedious|ectic"),
                                              "not enough time/too difficult",
                                              "other "))


raw_field$vaccine_no_reason<- ifelse(str_detect(raw_field$no_feed_use_2, ("idea")),
                                     "lack of knowledge",
                                     raw_field$vaccine_no_reason)
#s####
expanded_data %>% 
  select(county,
    feed_use, feed_aware,
         vaccine_use, vaccine_wouldbe_use, vaccine_aware,
         ai_use, crossbreed_aware) %>%
  group_by(county) %>% 
  skim()

summary(glm(family = binomial("logit"),
            data = expanded_data,
            crossbreed_aware~hh_head_gender+age+education+hh_depen+
              farming_years+num_cows+con_personal+degree+village_relationship
            ))

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

