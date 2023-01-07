library(dplyr)
library(tidyverse)
library(readxl)
library(tidyr)
library(janitor)
library(visNetwork)

raw_field <- read.csv("all_data.csv") %>% clean_names() %>% drop_na(basic_info_enum_name)

excess_title <- as.data.frame(as.matrix(c("basic_info_", "criteria_check_", "personal_info_",
                                          "milk_sell_milk_","info_groups_","formal_networks_info_","informal_networks_",
                                          "feed_fodder_", "animal_health_", "breeding_", "animal_health_",
                                          "info_transmission_")))

for (m in 1:nrow(excess_title)) {
  colnames(raw_field)<-ifelse(str_detect(colnames(raw_field), excess_title$V1[m]),
                          gsub(excess_title$V1[m], "", colnames(raw_field)),
                          colnames(raw_field))
}



raw_field$date <-  as.Date(raw_field$date, format = "%d/%m/%Y")
raw_field <- raw_field %>% filter(date>"2022-03-06") %>% drop_na(enum_name)
raw_field$date <- as.character(raw_field$date)

raw_field[raw_field=="TRUE"|raw_field=="yes"]<-1
raw_field[raw_field=="FALSE"|raw_field=="no"]<-0
raw_field[raw_field=="n/a"|raw_field=="999"|raw_field=="700000000"|raw_field=="9999"]<-NA
raw_field$perc_crossbreed[is.na(raw_field$perc_crossbreed)]<-0
raw_field$perc_crossbreed <- ifelse(raw_field$perc_crossbreed>0, 1, 0)

raw_field$improved_feed_napier <- ifelse(as.numeric(raw_field$improved_feed_nap_kak_2)+as.numeric(raw_field$improved_feed_napier_other_improv)>0,1,0)
raw_field <- raw_field %>% select(-improved_feed_nap_kak_2, -improved_feed_napier_other_improv)

raw_field[colnames(raw_field)] <- lapply(raw_field[colnames(raw_field)],str_to_lower)
raw_field[colnames(raw_field)]<-lapply(raw_field[colnames(raw_field)],  gsub, pattern="  ", 
                               replacement=" ")

fodders <- raw_field %>% select(improved_feed_desho_grass:improved_feed_other, improved_feed_napier)
fodders[colnames(fodders)] <- lapply(fodders[colnames(fodders)],as.numeric)
fodders <- as.data.frame(colSums(fodders))


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


raw_field[raw_field=="frdrick zelovi"]<-"fredrick zelovi"
raw_field[raw_field=="josina lugalishi"]<-"josina lugarishi"
raw_field[raw_field=="rebecca swahili"|
        raw_field=="rebeka kasudi"]<-"rebecca kasudi swahili"
raw_field[raw_field=="margrate hambwe"]<-"margaret hambwe"
raw_field[raw_field=="homesley vulimu"]<-"wormsley vulimu"
raw_field[raw_field=="mike misalia"]<-"michael musalia"
raw_field[raw_field=="henry mengesa"]<-"margaret mwambasani"
raw_field[raw_field=="harun ogadi/ruth kasoa"|
        raw_field=="harun ongadi"]<-"ruth kasoa"
raw_field[raw_field=="frederick odiege"]<-"fredrick odiege"
raw_field[raw_field=="cyprus majanga"]<-"cyrus aluvisia"
raw_field[raw_field=="warmsley vulimu"|raw_field=="wordsley vulimu"]<-"wormsley vulimu"
raw_field[raw_field=="raphael makamu"]<-"raphael magamu"
raw_field[raw_field=="edga lohondo"]<-"edgar lihondo"
raw_field[raw_field=="nixom kidim"]<-"nickson kidmu"
raw_field[raw_field=="azibeta chazima"]<-"azibeta agiza"
raw_field[raw_field=="beatrice mahuku"]<-"beatrice maugo"
raw_field[raw_field=="dason kaverenge"]<-"danson kiverenge"
raw_field[raw_field=="defina mugora"]<-"difina mbone mugoha"
raw_field[raw_field=="esther dambiza"]<-"esther naomo dambiza"
raw_field[raw_field=="evans seva"]<-"evans luvinzu"
raw_field[raw_field=="hesbon luma"|
        raw_field=="hesbon avedi"|
        raw_field=="hesbon lluma"|
        raw_field=="hesbon lluma"]<- "hesborn lluma"
raw_field[raw_field=="margret kaluli"|
        raw_field=="margret karudi"]<-"margaret kaluli"
raw_field[raw_field=="nancy dembede"|
        raw_field=="nancy debende"|raw_field=="nancy dembele"]<-"nancy debede"
raw_field[raw_field=="mary rugeywa"|
        raw_field=="mary lugeywa"]<-"mary logeywa"
raw_field[raw_field=="peter muenge"]<-"peter muhenge"
raw_field[raw_field=="safan arigora"]<-"safania arigora"
raw_field[raw_field=="zainabu mbona"]<-"zainabu mbone"
raw_field[raw_field=="gloria kirai/benedict kiwanuka"]<-"benedict kiwanuka/gloria kirai"
raw_field[raw_field=="margret mengesa"]<-"margaret mwambasani"
raw_field[raw_field=="micheal musalia/maxmilla"|
        raw_field=="maxmilla kavai"]<-"micheal musalia"
raw_field[raw_field=="rose isavo"]<-"rose isabwa"
raw_field[raw_field=="ruth ongati"]<-"ruth kasoa"
raw_field[raw_field=="micheal musalia"]<-"michael musalia"
raw_field[raw_field=="stanley rumwaji"]<-"muhonja christabel"
raw_field[raw_field=="wiliam malesi"]<-"william malesi"
raw_field[raw_field=="danson kiverenge"]<-"dason kiverenge"
raw_field[raw_field=="mary khadambi"|
        raw_field=="mary khatambi"]<-"consolata mary khatambi"
raw_field[raw_field=="lugai raphael"]<-"raphael lugai"
raw_field[raw_field=="zephaniah aligora"]<-"safania arigora"
raw_field[raw_field=="difina mugoa"]<-"difina mbone mugoha"
raw_field[raw_field=="fredrick odiege"]<-"frederick ondiege"
raw_field[raw_field=="maheri"]<-"alfred maheli"
raw_field[raw_field=="azibeta"]<-"azibeta agiza"
raw_field[raw_field=="beatrice kaungusia"]<-"beatrice maugo"








raw_field$parti_phone <- ifelse(raw_field$parti_name=="mary logeywa", 727552712, raw_field$parti_phone)

raw_field[raw_field=="milulu"]<-"mululu"
raw_field[raw_field=="kivaze"| raw_field=="kivaze a"| raw_field=="kivaze b"]<-"kivazi"
raw_field[raw_field=="kaimedi"|
        raw_field=="kaimendi"|
        raw_field=="gaimendi"|raw_field=="ngaimedi"|raw_field=="ngaimendi"]<-"gaimedi"


for (k in 14:17) {
  for (j in 1:nrow(raw_field)){
    raw_field[j,k] <-   ifelse(
      raw_field[j,k-12]%in%raw_field$parti_name,
      as.character(raw_field$parti_phone[which(raw_field$parti_name==raw_field[j,k-12])]),
      as.character(raw_field[j,k])
    )
  }
}


for(i in 9:12){
  raw_field[i]<- lapply(raw_field[i], as.character)
  for(j in 1:nrow(raw_field)){
    
    raw_field[j,i] <-   ifelse( raw_field[j,i-7]%in%raw_field$parti_name, as.character(raw_field$village_name[which(raw_field$parti_name==raw_field[j,i-7])]),
                            as.character(raw_field[j,i]))
  }
}



#qualitative analysis####

raw_field$neg_relationship_village <- ifelse(str_detect(raw_field$neg_relationship_village, "dispute"),
                                             "disputes",
                                             ifelse(str_detect(raw_field$neg_relationship_village, "on their own"),
                                                    "lack of cummunity spirit", 
                                                    "other"))


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
                                              "other"))


raw_field$vaccine_no_reason<- ifelse(str_detect(raw_field$no_feed_use_2, ("idea")),
                                     "lack of knowledge",
                                     raw_field$vaccine_no_reason)


view(raw_field %>% select(no_feed_use_1,
                          no_feed_use_2,
                          feed_no_recomm,
                          hay_benefits_yes_1,
                          hay_benefits_yes_2,
                          hay_making_no_recomm,
                          vaccine_no_reason,
                          vaccine_no_recomm,
                          ai_recommend_no,
                          hired_recommend_no, 
                          neg_relationship_village,
                          farmer_group_repeat_1_neg_relationship_coop
))


raw_field$farmer_group_repeat_1_group_mem_relationship <- as.numeric(raw_field$farmer_group_repeat_1_group_mem_relationship)-3
raw_field$farmer_group_repeat_2_group_mem_relationship <- as.numeric(raw_field$farmer_group_repeat_2_group_mem_relationship)-3
raw_field$farmer_group_repeat_1_group_mem_relationship[is.na(raw_field$farmer_group_repeat_1_group_mem_relationship)]<-0
raw_field$farmer_group_repeat_2_group_mem_relationship[is.na(raw_field$farmer_group_repeat_2_group_mem_relationship)]<-0

raw_field$social_cap <- (as.numeric(raw_field$farmer_group_repeat_1_group_mem_relationship))+(as.numeric(raw_field$farmer_group_repeat_2_group_mem_relationship))



field<- raw_field %>%   select("parti_name", 
                               "person_1",
                               "person_2",
                               "person_3",
                               "person_4", 
                               "enum_name", 
                               "date",
                               "village_name",
                               "person_1_location",
                               "person_2_location",
                               "person_3_location",
                               "person_4_location",
                               "parti_phone",
                               "person_1_number",
                               "person_2_number",
                               "person_3_number",
                               "person_4_number",
                               "farmer_group_repeat_1_group_name",
                               "farmer_group_repeat_2_group_name",
                               "religious_comm_repeat_1_which_religious_comm",
                               "finance_group_repeat_1_finance_group_name",
                               "finance_group_repeat_2_finance_group_name",
                               "finance_group_repeat_3_finance_group_name",
                               "social_group_repeat_1_social_groups_info",
                               "social_group_repeat_2_social_groups_info",
                               "feed_training_source",
                               "vaccine_benefits_source",
                               "vaccine_use",
                               #"breed_service_ai",
                               "vaccine_aware", 
                               "feed_used",
                               "aware_feed_benefits",
                               "perc_crossbreed",
                               "improv_breed",
                               "improved_feed_napier",
                               "improved_feed_bracchiaria",
                               "improved_feed_desmodium",
                               "social_cap"
)

people <- field %>% select(1:5) %>% pivot_longer(1:5, names_to = "cols", values_to = "people")
village <- field %>% select(8:12) %>% pivot_longer(1:5, names_to = "cols", values_to = "village") %>% select(2)
# field[13:17] <- lapply(field[13:17], as.character)
number <- field %>% select(13:17) %>% pivot_longer(1:5, names_to = "cols", values_to = "number") %>% select(2)
# 
 people_village <- cbind(people, village, number, 
                       do.call("rbind", replicate(5, as.data.frame(field$parti_name), simplify = FALSE)),
                       do.call("rbind", replicate(5, as.data.frame(field$vaccine_use), simplify = FALSE)), 
                       do.call("rbind", replicate(5, as.data.frame(field$breed_service_ai), simplify = FALSE)),
                       do.call("rbind", replicate(5, as.data.frame(field$feed_used), simplify = FALSE))) %>% 
 drop_na(people)%>% unique()%>% distinct(people, .keep_all = T) %>% 
 rename(vacc=`field$vaccine_use`,
        ai=`field$breed_service_ai`,
        fodder=`field$feed_used`)
# 
# people_village$people<-ifelse(people_village$people%in%field$parti_name,NA, as.character(people_village$people))
# people_village <- people_village %>% drop_na(people) %>%
#   #select(-1) %>%
#   distinct(people, .keep_all = T)


groups_cols <- c("farmer_group_repeat_1_group_name",
                 "farmer_group_repeat_2_group_name",
                 "religious_comm_repeat_1_which_religious_comm",
                 "finance_group_repeat_1_finance_group_name",
                 "finance_group_repeat_2_finance_group_name",
                 "social_group_repeat_1_social_groups_info",
                 "feed_training_source",
                 "vaccine_benefits_source")


field[groups_cols]<- lapply(field[groups_cols], gsub, pattern="from ", replacement="")
field[groups_cols]<- lapply(field[groups_cols], gsub, pattern=" and ", replacement=";")
field[groups_cols]<- lapply(field[groups_cols], gsub, pattern=",", replacement=";")
field[groups_cols]<- lapply(field[groups_cols], gsub, pattern="; ", replacement=";")


field[field=="busare"]<-"busare dairy group"
field[field=="caritus"]<-"caritas"
field[field=="chamakanga"|
        field=="chemakanga sublocation welfare group"|
        field=="chamakanga team welfare"|
        field=="chamakanga welfare"|
        field=="chemakanga welfare group"]<-"chamakanga welfare group"
field[field=="christians"|
        field=="church"]<-"christian"
field[field=="community welfare"]<-"community welfare group"
field[field=="jimodi welfare"]<-"jimodi welfare group"
field[field=="kitieto"]<-"kitiezo"
field[field=="trained by narigp project"]<-"narigp project"
field[field=="okoa group"]<-"okoa local farmer group"
field[field=="tea farming group"]<-"tea farmers association"
field[field=="village elders"]<-"village leaders"
field[field=="vumilia dairy"|
        field=="vumilia"|
        field=="vumilia group"|
        field=="vumulia"]<-"vumilia dairy group"
field[field=="worth group"]<-"worth"
field[field=="a vet"]<-"vet"
field[field=="caritas"|field=="caritus group"]<-"caritas group"
field[field=="farmers groups;lead farmers;ngos"]<-"farmers group;lead farmers;ngo"
field[field=="fips farm inputs promotion africa"|
        field=="fips, farm inputs promotion africa"]<-"fips;farm inputs promotion africa"
field[field=="got trainingfips in oneof tge farmers farm"]<-"fips"
field[field=="government extensionist"]<-"government extensionists"
field[field=="agroecology"]<-"agroecology group"
field[field=="agroecology"]<-"agroecology group"
field$finance_group_repeat_1_finance_group_name[field$finance_group_repeat_1_finance_group_name=="dairy group"]<-"vumilia dairy group"#same village as others, so change not made for other person from "dairy group" from diff village
field[field=="muga self help group."]<-"muga self help group"
field[field=="n.g.o"]<-"ngo"
field[groups_cols]<- lapply(field[groups_cols], gsub, pattern="ngos", replacement="ngo")
field[field=="trained by the narigp project"]<-"narigp project"
field[field=="ngo project"]<-"ngo"
field[field=="has no name"]<-"no name"
field[field=="okoa local farmer group"|
        field=="okoa chicken farmers"|
        field=="okoa"]<-"okoa chicken farmers group"
field[groups_cols]<- lapply(field[groups_cols], gsub, pattern="other farmers", replacement="other farmer")
field[field=="prevent diseases"]<-NA
field[field=="vet;my neighbour"]<-"vet;other farmer"
field[field=="vets"]<-"vet"
field[field=="vumilia dairy farmers"|
        field=="vumilia farmers"|
        field=="vumilia farmers group"]<-"vumilia dairy group"
field[field=="worth"|
        field=="worth program"]<-"worth women group"
field[field=="baraka"]<-"baraza"
field[field=="chama ya huruma"]<-"chama cha wazi"
field[field=="mululu welfare"]<-"mululu women group"
field$finance_group_repeat_1_finance_group_name[field$finance_group_repeat_1_finance_group_name=="mululu"]<-"mululu women group"
field[field=="tuinuane"]<-"two in one"

field <- field %>% separate(vaccine_benefits_source, c("vaccine_benefits_source_1",
                                                       "vaccine_benefits_source_2"), ";")
field <- field %>% separate(feed_training_source, c("feed_training_source_1",
                                                    "feed_training_source_2",
                                                    "feed_training_source_3"
), ";")

field[field=="community vet"]<-"vet"

field[field=="country government"|
        field=="government extensionists"|
        field=="government vets"|
        field=="narig"|
        field=="narigp project"]<-"government extensionists"#putting gov vets under gov not vets

field[field=="extension officer"]<-"other extension officers"

field[field=="lead farmer"|
        field=="volunteers farmers"]<-"lead farmers"

field[field=="ngo"]<-"other ngo"

field[field=="neighbour"]<-"other farmer"

field[field=="school"|
        field=="volunteer farmer trainer"]<-"training"

field[field=="farm inputs promotion africa"]<-"fips"

field[field=="area chief"|
        field=="village leaders"]<-"village chief/s"


info <- groups <- field %>% select("vaccine_benefits_source_1",
                                   "vaccine_benefits_source_2",
                                   "feed_training_source_1",
                                   "feed_training_source_2",
                                   "feed_training_source_3") %>% 
  pivot_longer(1:5, names_to = "cols", values_to = "groups") %>% 
  select(2) %>% 
  unique() %>% na.omit()

groups <- field %>% select("farmer_group_repeat_1_group_name",
                           "farmer_group_repeat_2_group_name",
                           "religious_comm_repeat_1_which_religious_comm",
                           "finance_group_repeat_1_finance_group_name",
                           "finance_group_repeat_2_finance_group_name",
                           "social_group_repeat_1_social_groups_info",
                           "vaccine_benefits_source_1",
                           "vaccine_benefits_source_2",
                           "feed_training_source_1",
                           "feed_training_source_2",
                           "feed_training_source_3") %>% 
  pivot_longer(1:8, names_to = "cols", values_to = "groups") %>% 
  select(2) %>% 
  unique() %>% na.omit()

library("writexl")
write_xlsx(groups,"vihiga groups.xlsx")

###network map

network_map <- field %>% 
  select("parti_name", 
         "person_1",
         "person_2",
         "person_3",
         "person_4",
         "vaccine_benefits_source_1",
                "vaccine_benefits_source_2",
                "feed_training_source_1",
                "feed_training_source_2",
                "feed_training_source_3")


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

surveyed <- field %>% select(1)
nodes_final <- nodes_final <- nodes_final %>% 
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

# test <- (rep(1:7, times=41))
# 
# # edges$value <- test
# edges$length <- test

visNetwork(nodes=nodes, edges = edges)

vihiga_net <- visNetwork(nodes=nodes, edges = edges)
visSave(vihiga_net, file="vihiga_net.html", background = "white")

field$vaccine_use[is.na(field$vaccine_use)]<-0
field$vaccine_aware[is.na(field$vaccine_aware)]<-0


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

field$feed_used[is.na(field$feed_used)]<-0

for (p in 1:nrow(nodes)) {
  nodes$group[p]<-ifelse(nodes$shape[p]=="pentagon", 
                         "not applicable",
                         ifelse(!(nodes$label[p]%in%field$parti_name),
                         "unknown",
                         ifelse(as.numeric(field$feed_used[which(field$parti_name==nodes$label[p])])==1,
                                "improved fodder",
                                ifelse(as.numeric(field$aware_feed_benefits[which(field$parti_name==nodes$label[p])])==1,
                                       "aware",
                                       "unaware"))))
}


for (p in 1:nrow(nodes)) {
  nodes$group[p]<-ifelse(nodes$shape[p]=="pentagon", 
                         "not applicable",
                         ifelse(!(nodes$label[p]%in%field$parti_name),
                         "unknown",
                         ifelse(as.numeric(field$perc_crossbreed[which(field$parti_name==nodes$label[p])])==1,
                                "improved breed",
                                ifelse(as.numeric(field$improv_breed[which(field$parti_name==nodes$label[p])])==1,
                                       "aware",
                                       "unaware"))))
}
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
  
nodes_nap <- nodes 
for (p in 1:nrow(nodes_nap)) {
  nodes_nap$group[p]<-ifelse(nodes$shape[p]=="pentagon", 
                             "not applicable",
                             ifelse(!(nodes_nap$label[p]%in%field$parti_name),
                             "unknown",
                             ifelse((field$improved_feed_napier[which(field$parti_name==nodes_nap$label[p])])==1,
                                    "napier",
                                    "NO napier")))
}
visNetwork(nodes=nodes_nap, edges = edges) %>% 
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

write_xlsx(g0_centrality,"vihiga centrality.xlsx")


net_properties <- function(g){
  
  diameter_ <- diameter(g, directed = F, weights = NA)
  
  mean_distance_ <- mean_distance(g, directed = F)
  
  edge_density_ <- edge_density(g)
  
  transitivity_ <- transitivity(g)
  
  print(as.data.frame(cbind(diameter_, mean_distance_, edge_density_, transitivity_)))
  
}
g0_properties<- as.data.frame(net_properties(g0))

write_xlsx(g0_properties, "vihiga network properties.xlsx")

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

#regression analysis####

ra_data <- raw_field %>% 
  select("parti_name",
    "vaccine_use",
    "feed_used",
    "perc_crossbreed",
         "parti_gender",
         "age",
         "farming_years",
         "education",
         "hh_depen",
         "perc_dairy",
         "num_cows",
         "farm_size",
         "price_market",
         "sell_market",
    "sell_coop")

group_numb <- group_nodes %>% select(label, group)

ra_data$parti_gender <- ifelse(ra_data$parti_gender=="female",0,1)

ra_data$education <- ifelse(ra_data$education=="none", 0,
                            ifelse(ra_data$education=="none_read", 1,
                                   ifelse(ra_data$education=="primary", 2,
                                          ifelse(ra_data$education=="above_primary",3,
                                                 NA))))
ra_data$farm_size <- ifelse(ra_data$farm_size=="0_0.5",0, 
                            ifelse(ra_data$farm_size=="0.6_2.5", 1, 
                                   ifelse(ra_data$farm_size=="greater_2.6",2,
                                          NA)))

ra_data<- ra_data %>% 
  left_join(group_numb, by=c("parti_name"="label"))
ra_data <- ra_data[order(ra_data$group),]


ra_data<-(pivot_wider(ra_data, 
                      names_from = group, 
                      values_from = group, 
                      values_fn = list(group = ~1), 
                      values_fill = list(group = 0)))

in_out_ra <- g0_centrality %>% filter(`g0_nodes$label`%in%ra_data$parti_name) %>% select(1,3:4,7)
ra_data <- (ra_data %>% 
              left_join(in_out_ra, by=c("parti_name"="g0_nodes$label")))

ra_data$connections <- (ra_data$`in`)+ra_data$out*0.5

ra_data[2:ncol(ra_data)] <- lapply(ra_data[2:ncol(ra_data)],as.numeric)

sizes(c1)

summary(lm(data=ra_data, vaccine_use ~ parti_gender+
           age+
           farming_years+
           education+
           hh_depen+
           perc_dairy+
           num_cows+
           farm_size+
           #price_market+
           sell_market+
            betweenness+
            `1`+`2`+`3`+`4`+`5`+`6`+`7`+`8`+`9`))

summary(lm(data=ra_data, vaccine_use ~ parti_gender+
             age+
             farming_years+
             education+
             hh_depen+
             perc_dairy+
             num_cows+
             farm_size+
             #price_market+
             sell_market+
             connections+
             `1`+`2`+`3`+`4`+`5`+`6`+`7`+`8`+`9`))

summary(lm(data=ra_data, feed_used ~ parti_gender+
             age+
             farming_years+
             education+
             hh_depen+
             perc_dairy+
             num_cows+
             farm_size+
            #price_market+
             sell_market+
             betweenness+
             `1`+`2`+`3`+`4`+`5`+`6`+`7`+`8`+`9`))

summary(lm(data=ra_data, feed_used ~ parti_gender+
             age+
             farming_years+
             education+
             hh_depen+
             perc_dairy+
             num_cows+
             farm_size+
             #price_market+
             sell_market+
             connections+
             `1`+`2`+`3`+`4`+`5`+`6`+`7`+`8`+`9`))

summary(lm(data=ra_data, perc_crossbreed ~ parti_gender+
             age+
             farming_years+
             education+
             hh_depen+
             perc_dairy+
             num_cows+
             farm_size+
             #price_market+
             sell_market+
             betweenness+
             `1`+`2`+`3`+`4`+`5`+`6`+`7`+`8`+`9`))

summary(lm(data=ra_data, perc_crossbreed ~ parti_gender+
             age+
             farming_years+
             education+
             hh_depen+
             perc_dairy+
             num_cows+
             farm_size+
             #price_market+
             sell_market+
             connections+
             `1`+`2`+`3`+`4`+`5`+`6`+`7`+`8`+`9`))

summary(lm(data=ra_data, betweenness ~ parti_gender+
             age+
             farming_years+
             education+
             hh_depen+
             perc_dairy+
             num_cows+
             farm_size+
             #price_market+
             sell_market+
             #sell_coop+
             vaccine_use+
             feed_used+
             perc_crossbreed))

summary(lm(data=ra_data, connections ~ parti_gender+
             age+
             farming_years+
             education+
             hh_depen+
             perc_dairy+
             num_cows+
             farm_size+
            #price_market+
             sell_market+
             #sell_coop+
             vaccine_use+
             feed_used+
             perc_crossbreed))

write.csv(ra_data, "vihiga_regression.csv")
