#packages####
library(dplyr)
library(tidyverse)
library(readxl) 
library(tidyr) 
library(janitor)
library(visNetwork)

#data import####
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

# raw_field$coordinates <- unite(raw_field,"coordinates", 
#                             "geo_location_latitude":"geo_location_longitude", 
#                             sep=",") %>% 
#   select(coordinates)




raw_field$date <-  as.Date(raw_field$date, format = "%d/%m/%Y")
raw_field <- raw_field %>% filter(date>"2022-02-27") %>% filter(date<"2022-03-03")
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

raw_field[raw_field=="alphonse okuku"]<-"alphonce okuku"
raw_field[raw_field=="ann akinyi otieno"]<-"ann akinyi"
raw_field[raw_field=="atanasia odhiambo"]<-"attanasio odhiambo"
raw_field[raw_field=="barabas ayugi"]<-"barnabas otieno"
raw_field[raw_field=="cyprian rachuonyo"|
        raw_field=="cyprin achieng"]<-"cyprine rachuonyo"
raw_field[raw_field=="david oure"]<-"david ouma oure"
raw_field[raw_field=="eric ochieng/jocinta akinyi"]<-"eric ochieng"
raw_field[raw_field=="frederick otieno aditi"|
        raw_field=="fred otieno anditi"|
        raw_field=="fredrick anditi" ] <-"fredrick otieno anditi"
raw_field[raw_field=="george mwangu"]<-"george mwango"
raw_field[raw_field=="greson otieno"]<-"gregson otieno"
raw_field[raw_field=="helen onyanch"]<-"helen onyach"
raw_field[raw_field=="john akiri okombo"]<-"john akiri"
raw_field[raw_field=="josenyadawa"]<-"joseph nyadawa"
raw_field[raw_field=="joseph ojowang"]<-"joseph ojwang ojwang"
raw_field[raw_field=="macolata orwa"]<-"macolata adhiambo arwo"
raw_field[raw_field=="maurice ario"|
        raw_field=="maurice ariyo"|
        raw_field=="morris ario"|
        raw_field=="moris aroio anyango"|
        raw_field=="morris ariyo"]<-"maurice ariyo anyango"
raw_field[raw_field=="milka ochieng"]<-"milkah ochieng"
raw_field[raw_field=="phillip oluoch"]<-"philip olouch ojowang"
raw_field[raw_field=="phyllis adoyo"]<-"phyllis adoyo ngere"
raw_field[raw_field=="plus opiyo mwango"]<-"pius opiyo mwango"
raw_field[raw_field=="rosemary akinyi"]<-"rosemary akinyi anditi"
raw_field[raw_field=="susan migunde"|
        raw_field=="susan mugune"]<-"susan migune"
raw_field[raw_field=="wilikister oyugi othoo"]<-"wilikister oyugi"
raw_field[raw_field=="cyprin rachuonyo"]<-"cyprine rachuonyo"
raw_field[raw_field=="device kagunga"]<-"devince agunga"
raw_field[raw_field=="john orwa"]<-"john fredrick hurmphery orwa"
raw_field[raw_field=="joseph opany"]<-"joseph mwango opany"
raw_field[raw_field=="samsung onyango"]<-"samson onyango ngere"
raw_field[raw_field=="walter  ojuka"]<-"walter ojuka"
raw_field[raw_field=="wambi 0pany"]<-"wambi opany"
raw_field[raw_field=="jackline adede"|
        raw_field=="jackline onyango"]<-"jackline onyango adede"#steve
raw_field[raw_field=="jane akeyo"|
        raw_field=="jane ojijo"]<-"jane akeyo ojijo"#steve
raw_field[raw_field=="margaret opiyo"|
        raw_field=="magret onyach"]<-"margaret opiyo onyach"#steve
raw_field[raw_field=="mevin otieno"]<-"mevin awino otieno"#steve
raw_field[raw_field=="michael mwango"|
        raw_field=="michael odhiambo wanyango"]<-"michael odhiambo onyango"#steve
raw_field[raw_field=="pamela nasa"|
        raw_field=="pamela orwa"|
        raw_field=="pamela atieno rashid"]<-"pamela otiena orwa"#steve nasa and rashid nicknames 
raw_field[raw_field=="patrick ochieng"|
        raw_field=="patrick odhiambo"]<-"patrick ochieng odhiambo"#steve
raw_field[raw_field=="ruth mwango"|
        raw_field=="ruth obiero"]<-"ruth mwango obiero"#steve
raw_field[raw_field=="walter"]<-"walter ojuka"#steve



raw_field[raw_field=="kokong'o"]<-"kokongo"
raw_field[raw_field=="ktieno"]<-"kotieno"
raw_field[raw_field=="migorii"]<-"migori"
raw_field[raw_field=="njume"]<-"njuma"

raw_field[raw_field=="700000000"]<-NA
raw_field$person_1_number <- ifelse(raw_field$person_1=="alphonce okuku", 704695396, raw_field$person_1_number)
raw_field[raw_field=="george"]<-"daktari george"
raw_field[raw_field=="millicent akinyi mbayo"]<-"millicent awour"
raw_field[raw_field=="molly achieng"|
        raw_field=="moline anyango"|
        raw_field=="molly"]<-"molly mbogo"
raw_field$person_4_number <- ifelse(raw_field$person_4=="philip olouch ojowang", 726573204, raw_field$person_4_number)
raw_field$person_2_number <- ifelse(raw_field$person_2=="susan migunde", 724099516, raw_field$person_2_number)
raw_field[raw_field=="jecinta akinyi"]<-"eric ochieng"
raw_field[raw_field=="joseph mwango opany"]<-"joseph opany"
raw_field[raw_field=="wambi rock"]<-"hezron wambirock"
raw_field[raw_field=="mourine matabero dihiambo"]<-"matabell"
raw_field[raw_field=="samuel dede, matabell adhiambo"|raw_field=="matabell"]<-"matabell adhiambo"
raw_field[raw_field=="millicent akinyi mbayo"]<-"millicent awour"
raw_field[raw_field=="plus ooko"]<-"pius opiyo mwango"
raw_field[raw_field=="rosella achieng"]<-"roslyne achieng ochiyo"
raw_field[raw_field=="millicent awour"]<-"millicent bayo"
raw_field[raw_field=="710804853"]<-"710804854"
raw_field[raw_field=="ruth otieno"|raw_field=="thomas mwango"]<-"ruth obiero"
raw_field[raw_field=="mary weke"]<-"mary achieng"
raw_field[raw_field=="okongo"]<-"zechariah okongo ochieng"

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
                           "improved_feed_desmodium",
                           "improved_feed_bracchiaria",
                           "improved_feed_calliandra", 
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
            "social_group_repeat_1_social_groups_info",
            "social_group_repeat_2_social_groups_info",
            "feed_training_source",
            "vaccine_benefits_source")

field$farmer_group_repeat_1_group_name <- (ifelse(str_detect(field$farmer_group_repeat_1_group_name,"acre"), 
                                                           "one acre fund",
                                                           ifelse(str_detect(field$farmer_group_repeat_1_group_name, "field d"),
                                                                  "field dairy farmers cooperative",
                                                            as.character(field$farmer_group_repeat_1_group_name))))

field$religious_comm_repeat_1_which_religious_comm <- (
         ifelse(str_detect(field$religious_comm_repeat_1_which_religious_comm,"cath"), "catholic",
                ifelse(str_detect(field$religious_comm_repeat_1_which_religious_comm,"bap"), "baptist",
                       ifelse(str_detect(field$religious_comm_repeat_1_which_religious_comm,"church|christ"), "christian",
                              as.character(field$religious_comm_repeat_1_which_religious_comm)))))


field$finance_group_repeat_1_finance_group_name <- 
  (ifelse(str_detect(field$finance_group_repeat_1_finance_group_name,"alpha"), 
                   "alpha women group",
                   ifelse(str_detect(field$finance_group_repeat_1_finance_group_name, "field dairy farmers"),
                          "field dairy sacco",
                          ifelse(str_detect(field$finance_group_repeat_1_finance_group_name, "silk"),
                                 "sirk",
                                 ifelse(str_detect(field$finance_group_repeat_1_finance_group_name, "skirt red|redskirt"), 
                                        "redskirt women group",
                                                      as.character(field$finance_group_repeat_1_finance_group_name))))))

field$finance_group_repeat_2_finance_group_name <- 
  (ifelse(str_detect(field$finance_group_repeat_2_finance_group_name,"alpha"), 
                   "alpha women group",
                   ifelse(str_detect(field$finance_group_repeat_2_finance_group_name, "field dairy farmers"),
                          "field dairy sacco",
                          ifelse(str_detect(field$finance_group_repeat_2_finance_group_name, "silk"),
                                 "sirk",
                                 ifelse(str_detect(field$finance_group_repeat_2_finance_group_name, "skirt red|redskirt"), 
                                        "redskirt women group",
                                        as.character(field$finance_group_repeat_2_finance_group_name))))))


field$social_group_repeat_1_social_groups_info <- 
  (ifelse(str_detect(field$social_group_repeat_1_social_groups_info, "community welfare"),
                   "community group",
                   ifelse(str_detect(field$social_group_repeat_1_social_groups_info, "jouinor|junior"),
                          "junior youth group",
                          ifelse(str_detect(field$social_group_repeat_1_social_groups_info, "kagwana"),
                                 "kagwana",
                                 ifelse(str_detect(field$social_group_repeat_1_social_groups_info, "karende"),
                                        "karende self-help group", 
                                        ifelse(str_detect(field$social_group_repeat_1_social_groups_info,"sirk"), 
                                               "sirk",
                                              as.character(field$social_group_repeat_1_social_groups_info)))))))


field$social_group_repeat_2_social_groups_info <- 
  (ifelse(str_detect(field$social_group_repeat_2_social_groups_info, "community welfare"),
                   "community group",
                   ifelse(str_detect(field$social_group_repeat_2_social_groups_info, "jouinor|junior"),
                          "junior youth group",
                          ifelse(str_detect(field$social_group_repeat_2_social_groups_info, "kagwana"),
                                 "kagwana",
                                 ifelse(str_detect(field$social_group_repeat_2_social_groups_info, "karende"),
                                        "karende self-help group", 
                                        ifelse(str_detect(field$social_group_repeat_2_social_groups_info,"sirk"), 
                                               "sirk",
                                               as.character(field$social_group_repeat_2_social_groups_info)))))))

field$feed_training_source <- gsub("from ", field$feed_training_source, replacement = "")
levels(as.factor(field$feed_training_source))
field$feed_training_source <- gsub(" and", field$feed_training_source, replacement = ";")

field$feed_training_source[field$feed_training_source=="cooperative;heifer international"]<-
  "rongo dairy farmers cooperative; heifer international"
field$feed_training_source[field$feed_training_source=="the dairy cooperative; other farmers" ]<-
  "rongo dairy farmers cooperative; other farmers"

field$feed_training_source[field$feed_training_source=="dairy cooperative"]<-
  "rongo dairy farmers cooperative"

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
field[field=="a private vet"]<-"private vet"
field[field=="government extension officers"]<-"government extensionists"
field[field=="kageso"|
        field=="kageso welfare group"]<-"kangeso women group"
field[field=="lwala"]<-"luala group"
field[field=="neigbour"]<-"other farmer"
field[field=="one arce fund"]<-"one acre fund"
field[field=="rongo dairies"|
        field=="rongo dairy"|
        field=="rongo dairy cooperative"]<-"rongo dairy farmers cooperative"
field[field=="rongo dairy;other farmer"]<-"rongo dairy farmers cooperative;other farmer"
field[field=="rongo dairy farmers sacco"]<-"rongo dairy sacco"
field[field=="the cooperative."]<-"rongo dairy farmers cooperative"
field[groups_cols]<- lapply(field[groups_cols], gsub, pattern="the cooperative", replacement="rongo dairy farmers cooperative")
field[field=="the government vets;other farmer"]<-"government vet;other farmer"
field[groups_cols]<- lapply(field[groups_cols], gsub, pattern="the radio", replacement="radio")
field[field=="veterinary officer"]<-"vet"
field[groups_cols]<- lapply(field[groups_cols], gsub, pattern="vets", replacement="vet")

field <- field %>% separate(vaccine_benefits_source, c("vaccine_benefits_source_1",
                                                       "vaccine_benefits_source_2",
                                                       "vaccine_benefits_source_3"), ";")
field <- field %>% separate(feed_training_source, c("feed_training_source_1",
                                                       "feed_training_source_2"
                                                       ), ";")

#str_split_fixed(field$vaccine_benefits_source, ";", 3)
field[field=="dairy cooperative"|
        field=="cooperative"|
        field=="rongo ccf"]<-"rongo dairy farmers cooperative"

field[field=="extension officers"|
        field=="extensionists"]<-"other extension officers"

field[field=="government"|
        field=="government extensionists."|
        field=="government livestock department"|
        field=="government vet"|
        field=="kenya livestock officers"|
        field=="narigp"]<-"government extensionists"

field[field=="in the training i attended"|
        field=="school"|
        field=="the benchmarking"|
        field=="volunteer farmer trainers"]<-"training"

field[field=="area chief"|
        field=="chief"|
        field=="village elders"]<-"chief"

field[field=="journals"]<-"newspapers"

field[field=="ngo"]<-"other ngo"

field[field=="private vet"]<-"vet"

info <- groups <- field %>% select("vaccine_benefits_source_1",
                                   "vaccine_benefits_source_2",
                                   "vaccine_benefits_source_3", 
                                   "feed_training_source_1",
                                   "feed_training_source_2") %>% 
  pivot_longer(1:5, names_to = "cols", values_to = "groups") %>% 
  select(2) %>% 
  unique() %>% na.omit()


groups <- field %>% select("farmer_group_repeat_1_group_name",
                           "farmer_group_repeat_2_group_name",
                           "religious_comm_repeat_1_which_religious_comm",
                           "finance_group_repeat_1_finance_group_name",
                           "finance_group_repeat_2_finance_group_name",
                           "social_group_repeat_1_social_groups_info",
                           "feed_training_source",
                           "vaccine_benefits_source") %>% 
  pivot_longer(1:8, names_to = "cols", values_to = "groups") %>% 
  select(2) %>% 
  unique() %>% na.omit()

library("writexl")
write_xlsx(groups,"rongo groups.xlsx")
###network map

network_map <- field %>% 
  select("parti_name", 
         "person_1",
         "person_2",
         "person_3",
         "person_4",
         "feed_training_source_1",
         "feed_training_source_2",
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

rongo_net <- visNetwork(nodes=nodes, edges = edges)
visSave(rongo_net, file="rongo_net.html", background = "white")


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

nodes_cal <- nodes 
for (p in 1:nrow(nodes_cal)) {
  nodes_cal$group[p]<-ifelse(nodes$shape[p]=="pentagon", 
                             "not applicable",
                             ifelse(!(nodes_cal$label[p]%in%field$parti_name),
                             "unknown",
                             ifelse((field$improved_feed_calliandra[which(field$parti_name==nodes_cal$label[p])])==1,
                                    "calliandra",
                                    "NO calliandra")))
}
visNetwork(nodes=nodes_cal, edges = edges) %>% 
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

write_xlsx(g0_centrality,"rongo centrality.xlsx")


net_properties <- function(g){
  
  diameter_ <- diameter(g, directed = F, weights = NA)
  
  mean_distance_ <- mean_distance(g, directed = F)
  
  edge_density_ <- edge_density(g)
  
  transitivity_ <- transitivity(g)
  
  print(as.data.frame(cbind(diameter_, mean_distance_, edge_density_, transitivity_)))
  
}
g0_properties<- as.data.frame(net_properties(g0))

write_xlsx(g0_properties, "rongo network properties.xlsx")

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
#group 12 is sofia atieno, who is own group and not displayed becuase she was interviewed, referred to people that were not interviewed, and wasnt referred to herself
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

ra_data[2:ncol(ra_data)] <- lapply(ra_data[2:ncol(ra_data)],as.numeric)

ra_data$parti_gender[is.na(ra_data$parti_gender)]<-0
ra_data$farm_size[is.na(ra_data$farm_size)]<-0 #person w no data for farm size has 0 cows, so going to assume no ag land or smallest area
ra_data$price_market[ra_data$price_market==6]<-60

in_out_ra <- g0_centrality %>% filter(`g0_nodes$label`%in%ra_data$parti_name) %>% select(1,3:4,7)
ra_data <- (ra_data %>% 
              left_join(in_out_ra, by=c("parti_name"="g0_nodes$label")))

ra_data$connections <- (ra_data$`in`)+ra_data$out*0.5

ra_data[2:ncol(ra_data)] <- lapply(ra_data[2:ncol(ra_data)],as.numeric)

summary(lm(data=ra_data, vaccine_use ~ parti_gender+
             age+
             farming_years+
             education+
             hh_depen+
             perc_dairy+
             num_cows+
             farm_size+
             price_market+
             sell_market+
             sell_coop+
             betweenness+
             `1`+`2`+`3`+`4`+`5`+`6`+`7`+`8`+`9`+`10`+`11`+`12`))

summary(lm(data=ra_data, vaccine_use ~ parti_gender+
             age+
             farming_years+
             education+
             hh_depen+
             perc_dairy+
             num_cows+
             farm_size+
             price_market+
             sell_market+
             sell_coop+
             connections+
             `1`+`2`+`3`+`4`+`5`+`6`+`7`+`8`+`9`+`10`+`11`+`12`))

summary(lm(data=ra_data, feed_used ~ parti_gender+
             age+
             farming_years+
             education+
             hh_depen+
             perc_dairy+
             num_cows+
             farm_size+
             price_market+
             sell_market+
             sell_coop+
             betweenness+
             `1`+`2`+`3`+`4`+`5`+`6`+`7`+`8`+`9`+`10`+`11`+`12`))

summary(lm(data=ra_data, feed_used ~ parti_gender+
             age+
             farming_years+
             education+
             hh_depen+
             perc_dairy+
             num_cows+
             farm_size+
             price_market+
             sell_market+
             sell_coop+
             connections+
             `1`+`2`+`3`+`4`+`5`+`6`+`7`+`8`+`9`+`10`+`11`+`12`))

summary(lm(data=ra_data, perc_crossbreed ~ parti_gender+
             age+
             farming_years+
             education+
             hh_depen+
             perc_dairy+
             num_cows+
             farm_size+
             price_market+
             sell_market+
             sell_coop+
             betweenness+
             `1`+`2`+`3`+`4`+`5`+`6`+`7`+`8`+`9`+`10`+`11`+`12`))

summary(lm(data=ra_data, perc_crossbreed ~ parti_gender+
             age+
             farming_years+
             education+
             hh_depen+
             perc_dairy+
             num_cows+
             farm_size+
             price_market+
             sell_market+
             sell_coop+
             connections+
             `1`+`2`+`3`+`4`+`5`+`6`+`7`+`8`+`9`+`10`+`11`+`12`))

summary(lm(data=ra_data, betweenness ~ parti_gender+
             age+
             farming_years+
             education+
             hh_depen+
             perc_dairy+
             num_cows+
             farm_size+
             price_market+
             sell_market+
             sell_coop+
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
             price_market+
             sell_market+
             sell_coop+
             vaccine_use+
             feed_used+
             perc_crossbreed))

write.csv(ra_data, "rongo_regression.csv")
