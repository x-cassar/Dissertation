library(dplyr)
library(tidyverse)
library(readxl) 
library(tidyr) 
library(janitor)
library(visNetwork)

#getting data in ####
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
raw_field <- raw_field %>% filter(date<"2022-02-28")
raw_field$date <- as.character(raw_field$date)

# raw_field$coordinates <- unite(raw_field,"coordinates", 
#                            "geo_location_latitude":"geo_location_longitude", 
#                            sep=",") %>% 
#   select(coordinates)

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
fodders[is.na(fodders)]<-0
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
raw_field <- raw_field %>% drop_na(enum_name)


raw_field[raw_field=="martin okwach"]<-"martin adada"
raw_field[raw_field=="martin okwach"]<-"martin adada"
raw_field[raw_field=="samuel ongou ochieng"]<-"samuel ongou"
raw_field[raw_field=="70486043"]<-"704869043"
raw_field[raw_field=="agnes akinyi"]<-"agnes okeyo"
raw_field[raw_field=="edward mbori/philistine ogenga"]<-"philistine ogenga"
raw_field[raw_field=="706396802"]<-"706398802"
raw_field[raw_field=="philemon"] <-"philemon ondiek"
raw_field[raw_field=="caren adhiambo okello"|raw_field=="caren adhiambo"] <-"caren okello"
raw_field[raw_field=="emily"]<-"emily nyamburi"
raw_field[raw_field=="mboya"]<-"joshua mboya"
raw_field[raw_field=="eunice adhiambo"]<-"eunice otieno"
raw_field[raw_field=="margaret ojuando"|raw_field=="mrs margaret ojuando"]<-"margaret ojwando"
raw_field[raw_field=="daktari kwach"]<-"george okwach"
raw_field[raw_field=="emily awor omboto"]<-"emily awor"
raw_field[raw_field=="okeko okal"]<-"okoko abel"
raw_field[raw_field=="727209267"]<-"727209262"
raw_field[raw_field=="joshua ouma auko"]<-"joshua auko"
raw_field[raw_field=="moriss otieno"]<-"moris otieno"
raw_field[raw_field=="ajos"]<-"josephine auma"
raw_field$person_2_number <- ifelse(raw_field$person_2=="bonface okinda", 711856542, raw_field$person_2_number)
raw_field[raw_field=="714570782"]<-"714590782"
raw_field[raw_field=="722840760"]<-"722480760"
raw_field[raw_field=="714642870"]<-"722835898"
raw_field[raw_field=="714642870"]<-"722835898"
raw_field[raw_field=="philistine oganga"]<-"philistine ogenga"
raw_field[raw_field=="joseph apollo bwana"]<-"joseph apollo"
raw_field[raw_field=="eric ouma oyugi"]<-"eric ouma"
raw_field[raw_field=="grace odongo"]<-"grace oyugi"
raw_field$person_1[raw_field$person_1=="jane"]<-"jane anyango"
raw_field[raw_field=="john okoth"]<-"john okoth ndege"
raw_field[raw_field=="marin awoko odongo"]<-"marin odogo"
raw_field[raw_field=="726687880"]<-"7266878880"
raw_field[raw_field=="veronica odera"]<-"veronicah odera"
raw_field[raw_field=="lillian ogada"]<-"lillian ogado"
raw_field[raw_field=="sila ouko"]<-"erick ochieng ouko"
raw_field[raw_field=="moseremo"]<-"joram mosaremo"
raw_field[raw_field=="grace alade"]<-"grace atieno"
raw_field[raw_field=="jemima olunga"]<-"jemima onunga"
raw_field[raw_field=="eliakim auko"]<-"eliakim okeyo"
raw_field[raw_field=="okwach george"]<-"george okwach"
raw_field[raw_field=="emily omboto"]<-"emily awor"
raw_field[raw_field=="marin odogo"]<-"marian odongo"
raw_field[raw_field=="kefa ogada"]<-"newton kefa ogada"
raw_field[raw_field=="alcaza ouma"]<-"joseph apollo"#steve wife
raw_field[raw_field=="everyone okoko"]<-"okoko abel"#steve, everyone meant to be evelyn, wife of abel
raw_field[raw_field=="odhiambo"]<-"gilbert odhiambo"#steve
raw_field[raw_field=="tom"]<-"tom sithe/judy sithe"#steve

raw_field$hh_depen[raw_field$hh_depen=="133"]<-13
raw_field[raw_field=="nyatindo."]<-"nyatindo"
raw_field[raw_field=="kisacho"]<-"kisachu"
raw_field[raw_field=="ravinji"]<-"rawinji"
raw_field[raw_field=="cupana"]<-"copana"
raw_field[raw_field=="kamunga"]<-"kamuma"
raw_field[raw_field=="mathende"|
            raw_field=="mathende"]<-"madhenge"
raw_field$person_1_location[raw_field$person_1_location==30]<-NA
raw_field[raw_field=="700000000"]<-NA
raw_field$person_1_number <- ifelse(raw_field$person_1=="grace oyugi", 722279812, raw_field$person_1_number)

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
                                              "other "))


raw_field$vaccine_no_reason<- ifelse(str_detect(raw_field$no_feed_use_2, ("idea")),
                                     "lack of knowledge",
                                     raw_field$vaccine_no_reason)

# 
# view(raw_field %>% select(no_feed_use_1,
#                           no_feed_use_2,
#                           feed_no_recomm,
#                           hay_benefits_yes_1,
#                           hay_benefits_yes_2,
#                           hay_making_no_recomm,
#                           vaccine_no_reason,
#                           vaccine_no_recomm,
#                           ai_recommend_no,
#                           hired_recommend_no, 
#                           neg_relationship_village,
#                           farmer_group_repeat_1_neg_relationship_coop
# ))


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
                               "improved_feed_bracchiaria",
                               "improved_feed_desmodium",
                               "improved_feed_rhodes_grass",
                               "improved_maize",
                               "social_cap"
)

people <- field %>% select(1:5) %>% pivot_longer(1:5, names_to = "cols", values_to = "people")
village <- field %>% select(8:12) %>% pivot_longer(1:5, names_to = "cols", values_to = "village") %>% select(2)
field[13:17] <- lapply(field[13:17], as.character)
number <- field %>% select(13:17) %>% pivot_longer(1:5, names_to = "cols", values_to = "number") %>% select(2)

groups <- c("farmer_group_repeat_1_group_name",
            "religious_comm_repeat_1_which_religious_comm",
            "finance_group_repeat_1_finance_group_name",
            "finance_group_repeat_2_finance_group_name",
            "finance_group_repeat_3_finance_group_name",
            "social_group_repeat_1_social_groups_info",
            "social_group_repeat_2_social_groups_info",
            "feed_training_source",
            "vaccine_benefits_source")
field[groups]<-lapply(field[groups], gsub, pattern="from  ", replacement="")



field$farmer_group_repeat_1_group_name <- (ifelse(str_detect(field$farmer_group_repeat_1_group_name,"ayoro"), "ayoro coffee group", 
                                                  ifelse(str_detect(field$farmer_group_repeat_1_group_name,"kasibondo"), "kasbondo dairy cooperative",
                                                         ifelse(str_detect(field$farmer_group_repeat_1_group_name,"kasbondo"), "kasbondo dairy cooperative",
                                                                as.character(field$farmer_group_repeat_1_group_name)))))

field$religious_comm_repeat_1_which_religious_comm <- (
  ifelse(str_detect(field$religious_comm_repeat_1_which_religious_comm,"advent|sda|seven"), "seventh day adventist",
         ifelse(str_detect(field$religious_comm_repeat_1_which_religious_comm,"cath"), "catholic",
                ifelse(str_detect(field$religious_comm_repeat_1_which_religious_comm,"bap"), "baptist",
                       ifelse(str_detect(field$religious_comm_repeat_1_which_religious_comm,"church|christ"), "christian",
                              as.character(field$religious_comm_repeat_1_which_religious_comm))))))

field$finance_group_repeat_1_finance_group_name <- 
  (ifelse(str_detect(field$finance_group_repeat_1_finance_group_name,"upendo"), 
          "ushirika na upendo",
          ifelse(str_detect(field$finance_group_repeat_1_finance_group_name, "jirani"),
                 "jirani self help group",
                 ifelse(str_detect(field$finance_group_repeat_1_finance_group_name, "losiek"),
                        "kodiek opong'obur self-help group",
                        ifelse(str_detect(field$finance_group_repeat_1_finance_group_name, "kisach"), 
                               "kisachu", 
                               ifelse(str_detect(field$finance_group_repeat_1_finance_group_name, "kosalo"),
                                      "kosalo",
                                      ifelse(str_detect(field$finance_group_repeat_1_finance_group_name, "name"),
                                             "no name",
                                             as.character(field$finance_group_repeat_1_finance_group_name))))))))

field$finance_group_repeat_2_finance_group_name <- 
  (ifelse(str_detect(field$finance_group_repeat_2_finance_group_name,"upendo"), 
          "ushirika na upendo",
          ifelse(str_detect(field$finance_group_repeat_2_finance_group_name, "jirani"),
                 "jirani self help group",
                 ifelse(str_detect(field$finance_group_repeat_2_finance_group_name, "losiek"),
                        "kodiek opong'obur self-help group",
                        ifelse(str_detect(field$finance_group_repeat_2_finance_group_name, "kisach"), 
                               "kisachu", 
                               ifelse(str_detect(field$finance_group_repeat_2_finance_group_name, "kosalo"),
                                      "kosalo",
                                      ifelse(str_detect(field$finance_group_repeat_2_finance_group_name, "name"),
                                             "no name",
                                             as.character(field$finance_group_repeat_2_finance_group_name))))))))

field$finance_group_repeat_3_finance_group_name <- 
  (ifelse(str_detect(field$finance_group_repeat_3_finance_group_name,"upendo"), 
          "ushirika na upendo",
          ifelse(str_detect(field$finance_group_repeat_3_finance_group_name, "jirani"),
                 "jirani self help group",
                 ifelse(str_detect(field$finance_group_repeat_3_finance_group_name, "losiek"),
                        "kodiek opong'obur self-help group",
                        ifelse(str_detect(field$finance_group_repeat_3_finance_group_name, "kisach"), 
                               "kisachu", 
                               ifelse(str_detect(field$finance_group_repeat_3_finance_group_name, "kosalo"),
                                      "kosalo",
                                      ifelse(str_detect(field$finance_group_repeat_3_finance_group_name, "name"),
                                             "no name",
                                             as.character(field$finance_group_repeat_3_finance_group_name))))))))

field$social_group_repeat_1_social_groups_info <- 
  (ifelse(str_detect(field$social_group_repeat_1_social_groups_info, "jirani"),
          "jirani self help group",
          ifelse(str_detect(field$social_group_repeat_1_social_group_info, "basi"),
                 "basi village",
                 ifelse(str_detect(field$social_group_repeat_1_social_groups_info, "upendo"),
                        "ushirika na upendo",
                        ifelse(str_detect(field$social_group_repeat_1_social_groups_info, "kanyamwanda"),
                               "kayamo self help group", 
                               ifelse(str_detect(field$social_group_repeat_1_social_groups_info,"rewinding"), 
                                      "rawinji self help group",
                                      ifelse(str_detect(day_1$social_group_repeat_1_social_groups_info,"ngwono"), 
                                             "nguono",
                                             ifelse(str_detect(field$social_group_repeat_1_social_groups_info, "kamuma"),
                                                    "kamuma self-help group",
                                                    ifelse(str_detect(field$social_group_repeat_1_social_groups_info, "minasi"),
                                                           "minasi",
                                                           ifelse(str_detect(field$social_group_repeat_1_social_groups_info, "ringa"),
                                                                  "ringa self-help group",
                                                                  as.character(day_1$social_group_repeat_1_social_groups_info)))))))))))


field$social_group_repeat_2_social_groups_info <- 
  (ifelse(str_detect(field$social_group_repeat_2_social_groups_info, "jirani"),
          "jirani self help group",
          ifelse(str_detect(field$social_group_repeat_2_social_group_info, "basi"),
                 "basi village",
                 ifelse(str_detect(field$social_group_repeat_2_social_groups_info, "upendo"),
                        "ushirika na upendo",
                        ifelse(str_detect(field$social_group_repeat_2_social_groups_info, "kanyamwanda"),
                               "kayamo self help group", 
                               ifelse(str_detect(field$social_group_repeat_2_social_groups_info,"rewinding"), 
                                      "rawinji self help group",
                                      ifelse(str_detect(day_1$social_group_repeat_2_social_groups_info,"ngwono"), 
                                             "nguono",
                                             ifelse(str_detect(field$social_group_repeat_2_social_groups_info, "kamuma"),
                                                    "kamuma self-help group",
                                                    ifelse(str_detect(field$social_group_repeat_2_social_groups_info, "minasi"),
                                                           "minasi",
                                                           ifelse(str_detect(field$social_group_repeat_2_social_groups_info, "ringa"),
                                                                  "ringa self-help group",
                                                                  as.character(day_1$social_group_repeat_2_social_groups_info)))))))))))


field$feed_training_source <- gsub("from ", field$feed_training_source, replacement = "")

field$feed_training_source <- as.factor(ifelse(str_detect(field$feed_training_source,"coop|cooperative|kasbon"), 
                                               "kasbondo dairy cooperative", 
                                               ifelse(str_detect(field$feed_training_source,"non"), as.character(field$feed_training_source),
                                                      ifelse(str_detect(field$feed_training_source,"gov"), "government",
                                                             as.character(field$feed_training_source)))))

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
field[groups_cols]<- lapply(field[groups_cols], gsub, pattern="ngos", replacement="ngo")
field[groups_cols]<- lapply(field[groups_cols], gsub, pattern="other farmers", replacement="other farmer")


field[field=="a vet"]<-"vet"
field[field=="an extension officer"|
        field=="an extensionists"|
        field=="extension officer"]<-"extensionist"
field[field=="coffee sacco"]<-"ayoro coffee group"
field[field=="cooperative ;kalro"]<-"cooperative;kalro"
field[groups_cols]<- lapply(field[groups_cols], gsub, pattern="dairy cooperative|cooperative", replacement="kasbondo dairy cooperative")
field[field=="kasbondo kasbondo dairy cooperative"]<-"kasbondo dairy cooperative"
field[field=="gelo gi yie"]<-"geno gi yie"
field[field=="nguono"]<-"nguono youth group"
field[field=="radio vet"]<-"radio;vet"
field[field=="tv"|
        field=="televisions"]<-"television"
field[field=="vet doctor"]<-"vet"
field[field=="vet;agrovet;reading books;television"]<-"vet;reading books;television"
field[field=="trainingsdairy extension officers"]<-"training;dairy extension officers"
field[field=="media"]<-"radio;television"

field <- field %>% separate(vaccine_benefits_source, c("vaccine_benefits_source_1",
                                                       "vaccine_benefits_source_2",
                                                       "vaccine_benefits_source_3"), ";")

#str_split_fixed(field$vaccine_benefits_source, ";", 3)
field[field=="learned school"]<-"school"
field[field=="vets"]<-"vet"

field[field=="county agricultural officer"|
        field=="government semina"|
        field=="government extensionists"|
        field=="ministry of agriculture"|
        field=="government"|
        field=="government extensionist"|
        field=="county agricultural staff"|
        field=="kalro"] <- "government extensionist"

field[field=="extension officers"|
        field=="dairy extension officers"|
        field=="extensionist"|
        field=="non government extensionists"]<-"other extension officers"

field[field=="friends"]<-"other farmer"
field[field=="coop society"]<-"kasbondo dairy cooperative"
field[field=="farm training"|
        field=="school"]<-"training"

field[field=="ngo"]<-"other ngo"

groups <- field %>% select("farmer_group_repeat_1_group_name",
                           "farmer_group_repeat_2_group_name",
                           "religious_comm_repeat_1_which_religious_comm",
                           "finance_group_repeat_1_finance_group_name",
                           "finance_group_repeat_2_finance_group_name",
                           "social_group_repeat_1_social_groups_info",
                           "vaccine_benefits_source_1",
                           "vaccine_benefits_source_2",
                           "vaccine_benefits_source_3", 
                           "feed_training_source"
) %>% 
  pivot_longer(1:6, names_to = "cols", values_to = "groups") %>% 
  select(2) %>% 
  unique() %>% na.omit()

info <- field %>% select("vaccine_benefits_source_1",
                         "vaccine_benefits_source_2",
                         "vaccine_benefits_source_3", 
                         "feed_training_source") %>% 
  pivot_longer(1:4, names_to = "cols", values_to = "groups") %>% 
  select(2) %>% 
  unique() %>% na.omit()

library("writexl")
write_xlsx(groups,"oyugis groups.xlsx")
###network map

network_map <- field %>% 
  select("parti_name", 
         "person_1",
         "person_2",
         "person_3",
         "person_4",
         "feed_training_source",
         "vaccine_benefits_source_1",
         "vaccine_benefits_source_2"
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

surveyed <- field %>% select(1)
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

ra_data$sell_market[is.na(ra_data$sell_market)]<-0


#clustering optimisation####


ra_data[2:ncol(ra_data)] <- lapply(ra_data[2:ncol(ra_data)],as.numeric)
test<- ra_data %>% 
  left_join(group_nodes %>% filter(label%in%ra_data$parti_name) %>% select(label, group),
            by=c("parti_name"="label")) %>% 
 # filter(group<10, group!=4, group!=7) %>%
 #  select(1:15,`1`,`2`,`3`,`5`,`6`,`8`,`9`, group) %>%
  mutate(group=as.factor(group)) 



library(broom)
for(b in 2:15){
  
  column <- names(test[b])
  #tidy will summarise and return neat format
  avz <- tidy(aov(unlist(test[,b]) ~ group, data = test))
  
  # Add this condition if you only want aov with P < 0.05 printed
  if(avz$p.value[1] < 0.05) {
   
    print(column)
     print(avz)
   }
  
}

# ra_data[16:22] <- lapply(ra_data[16:22],as.factor)
# 
# for(b in 2:15){
#   for(d in 16:22){
#     column <- names(test[b])
#     column2 <- names(test[d])
#   #tidy will summarise and return neat format
#   avz <- tidy(aov(unlist(test[,b]) ~ unlist(test[,d]), data = test))
# 
#   # Add this condition if you only want aov with P < 0.05 printed
#   if(avz$p.value[1] < 0.05) {
# 
#     print(column)
#     print(column2)
#     print(avz)
#   }
#   }
# }
# 
# test2 <- as.data.frame(matrix(ncol=14, nrow = 14))
# test2$V1 <- colnames(test[2:15])
# for (c in 1:13) {
#   for (b in 2:15) {
#    test2[b-1,c+1]<-(summary(test%>% filter(group==c) %>% select(b))[4])
#     
#   }
#   
# }

