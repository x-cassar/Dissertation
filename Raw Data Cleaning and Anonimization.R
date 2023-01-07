#packages and key functions####
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
'%!in%' <- function(x,y)!('%in%'(x,y))

#data import and cleaning####
raw_field <- read.csv("C:/Users/Xandru/OneDrive - University of Edinburgh/PS3/Placement/Rosie PHD/all_data.csv") %>%
  clean_names() 

raw_field[raw_field=="n/a"|raw_field=="999"|raw_field=="700000000"|raw_field=="9999"]<-NA
raw_field[colnames(raw_field)] <- lapply(raw_field[colnames(raw_field)],str_to_lower)
raw_field[colnames(raw_field)]<-lapply(raw_field[colnames(raw_field)],  gsub, pattern="  ", 
                                       replacement=" ")

excess_title <- as.data.frame(as.matrix(c("basic_info_", "criteria_check_", "personal_info_",
                                          "milk_sell_milk_","info_groups_","formal_networks_info_","informal_networks_",
                                          "feed_fodder_", "animal_health_", "breeding_", "animal_health_",
                                          "info_transmission_")))

for (m in 1:nrow(excess_title)) {
  colnames(raw_field)<-ifelse(str_detect(colnames(raw_field), excess_title$V1[m]),
                              gsub(excess_title$V1[m], "", colnames(raw_field)),
                              colnames(raw_field))
}

raw_field<- 
  raw_field %>% 
  mutate(enum_name=str_to_lower(enum_name)) %>% 
  mutate(enum_name= as.character(ifelse(str_detect(enum_name, "steve"),
                                           "steve",
                                           ifelse(str_detect(enum_name, "eunice"),
                                                  "eunice",
                                                  ifelse(str_detect(enum_name, "valerie"),
                                                         "valerie",
                                                         ifelse(str_detect(enum_name, "virginia"),
                                                                "virginia",
                                                                ifelse(str_detect(enum_name, "valentine"),
                                                                       "valentine", NA))))))) %>% 
  drop_na(enum_name) %>% 
  select(-c(meta_instance_id:x_xform_id))

raw_field[raw_field=="yes"|raw_field=="true"]<-"TRUE"
raw_field[raw_field=="no"|raw_field=="false"]<-"FALSE"

raw_field$decision_maker[raw_field$decision_maker=="myself"]<-"TRUE"
raw_field$decision_maker[raw_field$decision_maker=="No"|raw_field$decision_maker=="other"]<-"FALSE"


#menial cleaning by county####
##oyugis####
oyugis <- raw_field %>% 
  filter(as.Date(date, "%d/%m/%Y")<"2022-02-28") %>% 
  mutate(parti_name=str_to_lower(parti_name))

oyugis[oyugis=="martin okwach"]<-"martin adada"
oyugis[oyugis=="martin okwach"]<-"martin adada"
oyugis[oyugis=="samuel ongou ochieng"]<-"samuel ongou"
oyugis[oyugis=="70486043"]<-"704869043"
oyugis[oyugis=="agnes akinyi"]<-"agnes okeyo"
oyugis[oyugis=="edward mbori/philistine ogenga"]<-"philistine ogenga"
oyugis[oyugis=="706396802"]<-"706398802"
oyugis[oyugis=="philemon"] <-"philemon ondiek"
oyugis[oyugis=="caren adhiambo okello"|oyugis=="caren adhiambo"] <-"caren okello"
oyugis[oyugis=="emily"]<-"emily nyamburi"
oyugis[oyugis=="mboya"]<-"joshua mboya"
oyugis[oyugis=="eunice adhiambo"]<-"eunice otieno"
oyugis[oyugis=="margaret ojuando"|oyugis=="mrs margaret ojuando"]<-"margaret ojwando"
oyugis[oyugis=="daktari kwach"]<-"george okwach"
oyugis[oyugis=="emily awor omboto"]<-"emily awor"
oyugis[oyugis=="okeko okal"]<-"okoko abel"
oyugis[oyugis=="727209267"]<-"727209262"
oyugis[oyugis=="joshua ouma auko"]<-"joshua auko"
oyugis[oyugis=="moriss otieno"]<-"moris otieno"
oyugis[oyugis=="ajos"]<-"josephine auma"
oyugis$person_2_number <- ifelse(oyugis$person_2=="bonface okinda", 711856542, oyugis$person_2_number)
oyugis[oyugis=="714570782"]<-"714590782"
oyugis[oyugis=="722840760"]<-"722480760"
oyugis[oyugis=="714642870"]<-"722835898"
oyugis[oyugis=="714642870"]<-"722835898"
oyugis[oyugis=="philistine oganga"]<-"philistine ogenga"
oyugis[oyugis=="joseph apollo bwana"]<-"joseph apollo"
oyugis[oyugis=="eric ouma oyugi"]<-"eric ouma"
oyugis[oyugis=="grace odongo"]<-"grace oyugi"
oyugis$person_1[oyugis$person_1=="jane"]<-"jane anyango"
oyugis[oyugis=="john okoth"]<-"john okoth ndege"
oyugis[oyugis=="marin awoko odongo"]<-"marin odogo"
oyugis[oyugis=="726687880"]<-"7266878880"
oyugis[oyugis=="veronica odera"]<-"veronicah odera"
oyugis[oyugis=="lillian ogada"]<-"lillian ogado"
oyugis[oyugis=="sila ouko"]<-"erick ochieng ouko"
oyugis[oyugis=="moseremo"]<-"joram mosaremo"
oyugis[oyugis=="grace alade"]<-"grace atieno"
oyugis[oyugis=="jemima olunga"]<-"jemima onunga"
oyugis[oyugis=="eliakim auko"]<-"eliakim okeyo"
oyugis[oyugis=="okwach george"]<-"george okwach"
oyugis[oyugis=="emily omboto"]<-"emily awor"
oyugis[oyugis=="marin odogo"]<-"marian odongo"
oyugis[oyugis=="kefa ogada"]<-"newton kefa ogada"
oyugis[oyugis=="alcaza ouma"]<-"joseph apollo"#steve wife
oyugis[oyugis=="everyone okoko"]<-"okoko abel"#steve, everyone meant to be evelyn, wife of abel
oyugis[oyugis=="odhiambo"]<-"gilbert odhiambo"#steve
oyugis[oyugis=="tom"]<-"tom sithe/judy sithe"#steve

oyugis$hh_depen[oyugis$hh_depen=="133"]<-13
oyugis[oyugis=="nyatindo."]<-"nyatindo"
oyugis[oyugis=="kisacho"]<-"kisachu"
oyugis[oyugis=="ravinji"]<-"rawinji"
oyugis[oyugis=="cupana"]<-"copana"
oyugis[oyugis=="kamunga"]<-"kamuma"
oyugis[oyugis=="mathende"|
            oyugis=="mathende"]<-"madhenge"
oyugis$person_1_location[oyugis$person_1_location==30]<-NA
oyugis[oyugis=="700000000"]<-NA
oyugis$person_1_number <- ifelse(oyugis$person_1=="grace oyugi", 722279812, oyugis$person_1_number)

for (k in 14:17) {
  for (j in 1:nrow(oyugis)){
    oyugis[j,k] <-   ifelse(
      oyugis[j,k-12]%in%oyugis$parti_name,
      as.character(oyugis$parti_phone[which(oyugis$parti_name==oyugis[j,k-12])]),
      as.character(oyugis[j,k])
    )
  }
}


for(i in 9:12){
  oyugis[i]<- lapply(oyugis[i], as.character)
  for(j in 1:nrow(oyugis)){
    
    oyugis[j,i] <-   ifelse( oyugis[j,i-7]%in%oyugis$parti_name, as.character(oyugis$village_name[which(oyugis$parti_name==oyugis[j,i-7])]),
                                as.character(oyugis[j,i]))
  }
}

# oyugis_groups <- c("farmer_group_repeat_1_group_name",
#             #"religious_comm_repeat_1_which_religious_comm",
#             "finance_group_repeat_1_finance_group_name",
#             "finance_group_repeat_2_finance_group_name",
#             "finance_group_repeat_3_finance_group_name",
#             "social_group_repeat_1_social_groups_info",
#             "social_group_repeat_2_social_groups_info"#,
#             #"feed_training_source",
#             #"vaccine_benefits_source"
#             )
# oyugis[groups]<-lapply(oyugis[groups], gsub, pattern="from  ", replacement="")


oyugis$farmer_group_repeat_1_group_name <- (ifelse(str_detect(oyugis$farmer_group_repeat_1_group_name,"ayoro"), "ayoro coffee group", 
                                                  ifelse(str_detect(oyugis$farmer_group_repeat_1_group_name,"kasibondo"), "kasbondo dairy cooperative",
                                                         ifelse(str_detect(oyugis$farmer_group_repeat_1_group_name,"kasbondo"), "kasbondo dairy cooperative",
                                                                as.character(oyugis$farmer_group_repeat_1_group_name)))))

oyugis$religious_comm_repeat_1_which_religious_comm <- (
  ifelse(str_detect(oyugis$religious_comm_repeat_1_which_religious_comm,"advent|sda|seven"), "seventh day adventist",
         ifelse(str_detect(oyugis$religious_comm_repeat_1_which_religious_comm,"cath"), "catholic",
                ifelse(str_detect(oyugis$religious_comm_repeat_1_which_religious_comm,"bap"), "baptist",
                       ifelse(str_detect(oyugis$religious_comm_repeat_1_which_religious_comm,"church|christ"), "christian",
                              as.character(oyugis$religious_comm_repeat_1_which_religious_comm))))))

oyugis$finance_group_repeat_1_finance_group_name <- 
  (ifelse(str_detect(oyugis$finance_group_repeat_1_finance_group_name,"upendo"), 
          "ushirika na upendo",
          ifelse(str_detect(oyugis$finance_group_repeat_1_finance_group_name, "jirani"),
                 "jirani self help group",
                 ifelse(str_detect(oyugis$finance_group_repeat_1_finance_group_name, "losiek"),
                        "kodiek opong'obur self-help group",
                        ifelse(str_detect(oyugis$finance_group_repeat_1_finance_group_name, "kisach"), 
                               "kisachu", 
                               ifelse(str_detect(oyugis$finance_group_repeat_1_finance_group_name, "kosalo"),
                                      "kosalo",
                                      ifelse(str_detect(oyugis$finance_group_repeat_1_finance_group_name, "name"),
                                             "no name",
                                             as.character(oyugis$finance_group_repeat_1_finance_group_name))))))))

oyugis$finance_group_repeat_2_finance_group_name <- 
  (ifelse(str_detect(oyugis$finance_group_repeat_2_finance_group_name,"upendo"), 
          "ushirika na upendo",
          ifelse(str_detect(oyugis$finance_group_repeat_2_finance_group_name, "jirani"),
                 "jirani self help group",
                 ifelse(str_detect(oyugis$finance_group_repeat_2_finance_group_name, "losiek"),
                        "kodiek opong'obur self-help group",
                        ifelse(str_detect(oyugis$finance_group_repeat_2_finance_group_name, "kisach"), 
                               "kisachu", 
                               ifelse(str_detect(oyugis$finance_group_repeat_2_finance_group_name, "kosalo"),
                                      "kosalo",
                                      ifelse(str_detect(oyugis$finance_group_repeat_2_finance_group_name, "name"),
                                             "no name",
                                             as.character(oyugis$finance_group_repeat_2_finance_group_name))))))))

oyugis$finance_group_repeat_3_finance_group_name <- 
  (ifelse(str_detect(oyugis$finance_group_repeat_3_finance_group_name,"upendo"), 
          "ushirika na upendo",
          ifelse(str_detect(oyugis$finance_group_repeat_3_finance_group_name, "jirani"),
                 "jirani self help group",
                 ifelse(str_detect(oyugis$finance_group_repeat_3_finance_group_name, "losiek"),
                        "kodiek opong'obur self-help group",
                        ifelse(str_detect(oyugis$finance_group_repeat_3_finance_group_name, "kisach"), 
                               "kisachu", 
                               ifelse(str_detect(oyugis$finance_group_repeat_3_finance_group_name, "kosalo"),
                                      "kosalo",
                                      ifelse(str_detect(oyugis$finance_group_repeat_3_finance_group_name, "name"),
                                             "no name",
                                             as.character(oyugis$finance_group_repeat_3_finance_group_name))))))))

oyugis$social_group_repeat_1_social_groups_info <- 
  (ifelse(str_detect(oyugis$social_group_repeat_1_social_groups_info, "jirani"),
          "jirani self help group",
          ifelse(str_detect(oyugis$social_group_repeat_1_social_group_info, "basi"),
                 "basi village",
                 ifelse(str_detect(oyugis$social_group_repeat_1_social_groups_info, "upendo"),
                        "ushirika na upendo",
                        ifelse(str_detect(oyugis$social_group_repeat_1_social_groups_info, "kanyamwanda"),
                               "kayamo self help group", 
                               ifelse(str_detect(oyugis$social_group_repeat_1_social_groups_info,"rewinding"), 
                                      "rawinji self help group",
                                      ifelse(str_detect(day_1$social_group_repeat_1_social_groups_info,"ngwono"), 
                                             "nguono",
                                             ifelse(str_detect(oyugis$social_group_repeat_1_social_groups_info, "kamuma"),
                                                    "kamuma self-help group",
                                                    ifelse(str_detect(oyugis$social_group_repeat_1_social_groups_info, "minasi"),
                                                           "minasi",
                                                           ifelse(str_detect(oyugis$social_group_repeat_1_social_groups_info, "ringa"),
                                                                  "ringa self-help group",
                                                                  as.character(day_1$social_group_repeat_1_social_groups_info)))))))))))


oyugis$social_group_repeat_2_social_groups_info <- 
  (ifelse(str_detect(oyugis$social_group_repeat_2_social_groups_info, "jirani"),
          "jirani self help group",
          ifelse(str_detect(oyugis$social_group_repeat_2_social_group_info, "basi"),
                 "basi village",
                 ifelse(str_detect(oyugis$social_group_repeat_2_social_groups_info, "upendo"),
                        "ushirika na upendo",
                        ifelse(str_detect(oyugis$social_group_repeat_2_social_groups_info, "kanyamwanda"),
                               "kayamo self help group", 
                               ifelse(str_detect(oyugis$social_group_repeat_2_social_groups_info,"rewinding"), 
                                      "rawinji self help group",
                                      ifelse(str_detect(day_1$social_group_repeat_2_social_groups_info,"ngwono"), 
                                             "nguono",
                                             ifelse(str_detect(oyugis$social_group_repeat_2_social_groups_info, "kamuma"),
                                                    "kamuma self-help group",
                                                    ifelse(str_detect(oyugis$social_group_repeat_2_social_groups_info, "minasi"),
                                                           "minasi",
                                                           ifelse(str_detect(oyugis$social_group_repeat_2_social_groups_info, "ringa"),
                                                                  "ringa self-help group",
                                                                  as.character(day_1$social_group_repeat_2_social_groups_info)))))))))))

oyugis$feed_training_source <- gsub("from ", oyugis$feed_training_source, replacement = "")

oyugis$feed_training_source <- as.factor(ifelse(str_detect(oyugis$feed_training_source,"coop|cooperative|kasbon"), 
                                               "kasbondo dairy cooperative", 
                                               ifelse(str_detect(oyugis$feed_training_source,"non"), as.character(oyugis$feed_training_source),
                                                      ifelse(str_detect(oyugis$feed_training_source,"gov"), "government",
                                                             as.character(oyugis$feed_training_source)))))
oyugis_groups_cols <- c("farmer_group_repeat_1_group_name",
                 "farmer_group_repeat_2_group_name",
                 "religious_comm_repeat_1_which_religious_comm",
                 "finance_group_repeat_1_finance_group_name",
                 "finance_group_repeat_2_finance_group_name",
                 "social_group_repeat_1_social_groups_info",
                 "feed_training_source",
                 "vaccine_benefits_source"
                 )

oyugis[oyugis_groups_cols]<- lapply(oyugis[oyugis_groups_cols], gsub, pattern="from ", replacement="")
oyugis[oyugis_groups_cols]<- lapply(oyugis[oyugis_groups_cols], gsub, pattern=" and ", replacement=";")
oyugis[oyugis_groups_cols]<- lapply(oyugis[oyugis_groups_cols], gsub, pattern=",", replacement=";")
oyugis[oyugis_groups_cols]<- lapply(oyugis[oyugis_groups_cols], gsub, pattern="; ", replacement=";")
oyugis[oyugis_groups_cols]<- lapply(oyugis[oyugis_groups_cols], gsub, pattern="ngos", replacement="ngo")
oyugis[oyugis_groups_cols]<- lapply(oyugis[oyugis_groups_cols], gsub, pattern="other farmers", replacement="other farmer")
oyugis[oyugis_groups_cols]<- lapply(oyugis[oyugis_groups_cols], gsub, pattern="dairy cooperative|cooperative", replacement="kasbondo dairy cooperative")

oyugis[oyugis=="a vet"]<-"vet"
oyugis[oyugis=="an extension officer"|
        oyugis=="an extensionists"|
        oyugis=="extension officer"]<-"extensionist"
oyugis[oyugis=="coffee sacco"]<-"ayoro coffee group"
oyugis[oyugis=="cooperative ;kalro"]<-"cooperative;kalro"
oyugis[oyugis=="kasbondo kasbondo dairy cooperative"]<-"kasbondo dairy cooperative"
oyugis[oyugis=="gelo gi yie"]<-"geno gi yie"
oyugis[oyugis=="nguono"]<-"nguono youth group"
oyugis[oyugis=="radio vet"]<-"radio;vet"
oyugis[oyugis=="tv"|
        oyugis=="televisions"]<-"television"
oyugis[oyugis=="vet doctor"]<-"vet"
oyugis[oyugis=="vet;agrovet;reading books;television"]<-"vet;reading books;television"
oyugis[oyugis=="trainingsdairy extension officers"]<-"training;dairy extension officers"
oyugis[oyugis=="media"]<-"radio;television"

oyugis <- oyugis %>% separate(vaccine_benefits_source, c("vaccine_benefits_source_1",
                                                       "vaccine_benefits_source_2",
                                                       "vaccine_benefits_source_3"), ";")

oyugis[oyugis=="learned school"]<-"school"
oyugis[oyugis=="vets"]<-"vet"
oyugis[oyugis=="county agricultural officer"|
        oyugis=="government semina"|
        oyugis=="government extensionists"|
        oyugis=="ministry of agriculture"|
        oyugis=="government"|
        oyugis=="government extensionist"|
        oyugis=="county agricultural staff"|
        oyugis=="kalro"] <- "government extensionist"
oyugis[oyugis=="extension officers"|
        oyugis=="dairy extension officers"|
        oyugis=="extensionist"|
        oyugis=="non government extensionists"]<-"other extension officers"
oyugis[oyugis=="friends"]<-"other farmer"
oyugis[oyugis=="coop society"]<-"kasbondo dairy cooperative"
oyugis[oyugis=="farm training"|
        oyugis=="school"]<-"training"
oyugis[oyugis=="ngo"]<-"other ngo"

oyugis$person_3[oyugis$person_3=="kennedy otieno"]<-"kennedy otieno x"

oyugis<-oyugis %>% 
  unite("vaccine_benefits_source",vaccine_benefits_source_1:vaccine_benefits_source_3, sep = ";", na.rm=TRUE ) %>% 
  mutate_all(na_if,"") 
##rongo####
rongo <- raw_field %>% 
  filter(as.Date(date, "%d/%m/%Y")>"2022-02-27") %>% 
  filter(as.Date(date, "%d/%m/%Y")<"2022-03-03") %>% 
  mutate(parti_name=str_to_lower(parti_name))

rongo[rongo=="alphonse okuku"]<-"alphonce okuku"
rongo[rongo=="ann akinyi otieno"]<-"ann akinyi"
rongo[rongo=="atanasia odhiambo"]<-"attanasio odhiambo"
rongo[rongo=="barabas ayugi"]<-"barnabas otieno"
rongo[rongo=="cyprian rachuonyo"|
            rongo=="cyprin achieng"]<-"cyprine rachuonyo"
rongo[rongo=="david oure"]<-"david ouma oure"
rongo[rongo=="eric ochieng/jocinta akinyi"]<-"eric ochieng"
rongo[rongo=="frederick otieno aditi"|
            rongo=="fred otieno anditi"|
            rongo=="fredrick anditi" ] <-"fredrick otieno anditi"
rongo[rongo=="george mwangu"]<-"george mwango"
rongo[rongo=="greson otieno"]<-"gregson otieno"
rongo[rongo=="helen onyanch"]<-"helen onyach"
rongo[rongo=="john akiri okombo"]<-"john akiri"
rongo[rongo=="josenyadawa"]<-"joseph nyadawa"
rongo[rongo=="joseph ojowang"]<-"joseph ojwang ojwang"
rongo[rongo=="macolata orwa"]<-"macolata adhiambo arwo"
rongo[rongo=="maurice ario"|
            rongo=="maurice ariyo"|
            rongo=="morris ario"|
            rongo=="moris aroio anyango"|
            rongo=="morris ariyo"]<-"maurice ariyo anyango"
rongo[rongo=="milka ochieng"]<-"milkah ochieng"
rongo[rongo=="phillip oluoch"]<-"philip olouch ojowang"
rongo[rongo=="phyllis adoyo"]<-"phyllis adoyo ngere"
rongo[rongo=="plus opiyo mwango"]<-"pius opiyo mwango"
rongo[rongo=="rosemary akinyi"]<-"rosemary akinyi anditi"
rongo[rongo=="susan migunde"|
            rongo=="susan mugune"]<-"susan migune"
rongo[rongo=="wilikister oyugi othoo"]<-"wilikister oyugi"
rongo[rongo=="cyprin rachuonyo"]<-"cyprine rachuonyo"
rongo[rongo=="device kagunga"]<-"devince agunga"
rongo[rongo=="john orwa"]<-"john fredrick hurmphery orwa"
rongo[rongo=="joseph opany"]<-"joseph mwango opany"
rongo[rongo=="samsung onyango"]<-"samson onyango ngere"
rongo[rongo=="walter  ojuka"]<-"walter ojuka"
rongo[rongo=="wambi 0pany"]<-"wambi opany"
rongo[rongo=="jackline adede"|
            rongo=="jackline onyango"]<-"jackline onyango adede"#steve
rongo[rongo=="jane akeyo"|
            rongo=="jane ojijo"]<-"jane akeyo ojijo"#steve
rongo[rongo=="margaret opiyo"|
            rongo=="magret onyach"]<-"margaret opiyo onyach"#steve
rongo[rongo=="mevin otieno"]<-"mevin awino otieno"#steve
rongo[rongo=="michael mwango"|
            rongo=="michael odhiambo wanyango"]<-"michael odhiambo onyango"#steve
rongo[rongo=="pamela nasa"|
            rongo=="pamela orwa"|
            rongo=="pamela atieno rashid"]<-"pamela otiena orwa"#steve nasa and rashid nicknames 
rongo[rongo=="patrick ochieng"|
            rongo=="patrick odhiambo"]<-"patrick ochieng odhiambo"#steve
rongo[rongo=="ruth mwango"|
            rongo=="ruth obiero"]<-"ruth mwango obiero"#steve
rongo[rongo=="walter"]<-"walter ojuka"#steve
rongo[rongo=="kokong'o"]<-"kokongo"
rongo[rongo=="ktieno"]<-"kotieno"
rongo[rongo=="migorii"]<-"migori"
rongo[rongo=="njume"]<-"njuma"
rongo[rongo=="700000000"]<-NA
rongo$person_1_number <- ifelse(rongo$person_1=="alphonce okuku", 704695396, rongo$person_1_number)
rongo[rongo=="george"]<-"daktari george"
rongo[rongo=="millicent akinyi mbayo"]<-"millicent awour"
rongo[rongo=="molly achieng"|
            rongo=="moline anyango"|
            rongo=="molly"]<-"molly mbogo"
rongo$person_4_number <- ifelse(rongo$person_4=="philip olouch ojowang", 726573204, rongo$person_4_number)
rongo$person_2_number <- ifelse(rongo$person_2=="susan migunde", 724099516, rongo$person_2_number)
rongo[rongo=="jecinta akinyi"]<-"eric ochieng"
rongo[rongo=="joseph mwango opany"]<-"joseph opany"
rongo[rongo=="wambi rock"]<-"hezron wambirock"
rongo[rongo=="mourine matabero dihiambo"]<-"matabell"
rongo[rongo=="samuel dede, matabell adhiambo"|rongo=="matabell"]<-"matabell adhiambo"
rongo[rongo=="millicent akinyi mbayo"]<-"millicent awour"
rongo[rongo=="plus ooko"]<-"pius opiyo mwango"
rongo[rongo=="rosella achieng"]<-"roslyne achieng ochiyo"
rongo[rongo=="millicent awour"]<-"millicent bayo"
rongo[rongo=="710804853"]<-"710804854"
rongo[rongo=="ruth otieno"|rongo=="thomas mwango"]<-"ruth obiero"
rongo[rongo=="mary weke"]<-"mary achieng"
rongo[rongo=="okongo"]<-"zechariah okongo ochieng"

for (k in 14:17) {
  for (j in 1:nrow(rongo)){
    rongo[j,k] <-   ifelse(
      rongo[j,k-12]%in%rongo$parti_name,
      as.character(rongo$parti_phone[which(rongo$parti_name==rongo[j,k-12])]),
      as.character(rongo[j,k])
    )
  }
}


for(i in 9:12){
  rongo[i]<- lapply(rongo[i], as.character)
  for(j in 1:nrow(rongo)){
    
    rongo[j,i] <-   ifelse( rongo[j,i-7]%in%rongo$parti_name, as.character(rongo$village_name[which(rongo$parti_name==rongo[j,i-7])]),
                                as.character(rongo[j,i]))
  }
}

rongo$neg_relationship_village <- ifelse(str_detect(rongo$neg_relationship_village, "dispute"),
                                             "disputes",
                                             ifelse(str_detect(rongo$neg_relationship_village, "on their own"),
                                                    "lack of cummunity spirit", 
                                                    "other"))


rongo_qual <- c("no_feed_use",
          "feed_no_recomm",
          "hay_benefits_yes",
          "hay_making_no_recomm",
          "vaccine_no_reason",
          "vaccine_no_recomm",
          "ai_recommend_no",
          "hired_recommend_no")
rongo[rongo_qual] <- lapply(rongo[rongo_qual], gsub, pattern=",", replacement=";")
rongo[rongo_qual] <- lapply(rongo[rongo_qual], gsub, pattern="  ", replacement=" ")
rongo[rongo_qual] <- lapply(rongo[rongo_qual], gsub, pattern=" and ", replacement=";")
rongo[rongo_qual] <- lapply(rongo[rongo_qual], gsub, pattern=" ;", replacement=";")
rongo[rongo_qual] <- lapply(rongo[rongo_qual], gsub, pattern="; ", replacement=";")

rongo <- rongo %>% separate(no_feed_use, c("no_feed_use_1",
                                                   "no_feed_use_2"), ";")
rongo <- rongo %>% separate(hay_benefits_yes, c("hay_benefits_yes_1",
                                                        "hay_benefits_yes_2"), ";")

rongo$no_feed_use_1 <- ifelse(str_detect(rongo$no_feed_use_1, ("expens|cost|finance")),
                                  "expensive", 
                                  ifelse(str_detect(rongo$no_feed_use_1, ("cow|free")),
                                         "unimproved or free range cow/s",
                                         ifelse(str_detect(rongo$no_feed_use_1, ("idea|know")),
                                                "lack of knowledge",
                                                ifelse(str_detect(rongo$no_feed_use_1, ("seed|available|do not have|don't have|doesn't have|don't own|don't produce|doesn't grow")),
                                                       "no access to seed",
                                                       ifelse(str_detect(rongo$no_feed_use_1, ("land|size|space|small|limited|lznd")),
                                                              "limited space", 
                                                              ifelse(str_detect(rongo$no_feed_use_1, "time"),
                                                                     "not enough time",
                                                                     "other"))))))


rongo$no_feed_use_2 <- ifelse(str_detect(rongo$no_feed_use_2, ("expens|cost|finance")),
                                  "expensive", 
                                  ifelse(str_detect(rongo$no_feed_use_2, ("cow|free")),
                                         "unimproved or free range cow/s",
                                         ifelse(str_detect(rongo$no_feed_use_2, ("idea|know")),
                                                "lack of knowledge",
                                                ifelse(str_detect(rongo$no_feed_use_2, ("seed|available")),
                                                       "no access to seed",
                                                       ifelse(str_detect(rongo$no_feed_use_2, ("land|size|space|small|limited")),
                                                              "limited space", 
                                                              ifelse(str_detect(rongo$no_feed_use_2, "time"),
                                                                     "not enough time",
                                                                     "other"))))))

rongo$feed_no_recomm <- ifelse(str_detect(rongo$feed_no_recomm, ("expens|cost|finance")),
                                   "expensive", 
                                   ifelse(str_detect(rongo$feed_no_recomm, ("cow|free")),
                                          "unimproved or free range cow/s",
                                          ifelse(str_detect(rongo$feed_no_recomm, ("idea|know")),
                                                 "lack of knowledge",
                                                 ifelse(str_detect(rongo$feed_no_recomm, ("seed|available")),
                                                        "no access to seed",
                                                        ifelse(str_detect(rongo$feed_no_recomm, ("land|size|space|small|limited")),
                                                               "limited space", 
                                                               ifelse(str_detect(rongo$feed_no_recomm, "time"),
                                                                      "not enough time",
                                                                      rongo$feed_no_recomm))))))

rongo$hay_benefits_yes_1 <- ifelse(str_detect(rongo$hay_benefits_yes_1, ("expens|cost|finance|capital|funds|sive")),
                                       "expensive", 
                                       ifelse(str_detect(rongo$hay_benefits_yes_1, ("no enough|not enough|doesn't have enough|lack of enough|doesn't grow|do not have enough|no fodder|no feeds|resource|material")),
                                              "no/too little fodder",
                                              ifelse(str_detect(rongo$hay_benefits_yes_1, ("know|skill|practice|aware|capacity|no idea|knwo")),
                                                     "lack of knowledge",
                                                     ifelse(str_detect(rongo$hay_benefits_yes_1, "difficult|time|tedious|ectic"),
                                                            "not enough time/too difficult",
                                                            ifelse(str_detect(rongo$hay_benefits_yes_1, ("small|room|space")),
                                                                   "limited space", 
                                                                   "other")))))

rongo$hay_benefits_yes_2 <- ifelse(str_detect(rongo$hay_benefits_yes_2, ("financ|capital")),
                                       "expensive",
                                       ifelse(str_detect(rongo$hay_benefits_yes_2, "difficult|time|tedious|ectic"),
                                              "not enough time/too difficult",
                                              "other"))


rongo$vaccine_no_reason<- ifelse(str_detect(rongo$no_feed_use_2, ("idea")),
                                     "lack of knowledge",
                                     rongo$vaccine_no_reason)


rongo$farmer_group_repeat_1_group_name <- (ifelse(str_detect(rongo$farmer_group_repeat_1_group_name,"acre"), 
                                                  "one acre fund",
                                                  ifelse(str_detect(rongo$farmer_group_repeat_1_group_name, "rongo d"),
                                                         "rongo dairy farmers cooperative",
                                                         as.character(rongo$farmer_group_repeat_1_group_name))))

rongo$religious_comm_repeat_1_which_religious_comm <- (
  ifelse(str_detect(rongo$religious_comm_repeat_1_which_religious_comm,"cath"), "catholic",
         ifelse(str_detect(rongo$religious_comm_repeat_1_which_religious_comm,"bap"), "baptist",
                ifelse(str_detect(rongo$religious_comm_repeat_1_which_religious_comm,"church|christ"), "christian",
                       as.character(rongo$religious_comm_repeat_1_which_religious_comm)))))


rongo$finance_group_repeat_1_finance_group_name <- 
  (ifelse(str_detect(rongo$finance_group_repeat_1_finance_group_name,"alpha"), 
          "alpha women group",
          ifelse(str_detect(rongo$finance_group_repeat_1_finance_group_name, "rongo dairy farmers"),
                 "rongo dairy sacco",
                 ifelse(str_detect(rongo$finance_group_repeat_1_finance_group_name, "silk"),
                        "sirk",
                        ifelse(str_detect(rongo$finance_group_repeat_1_finance_group_name, "skirt red|redskirt"), 
                               "redskirt women group",
                               as.character(rongo$finance_group_repeat_1_finance_group_name))))))

rongo$finance_group_repeat_2_finance_group_name <- 
  (ifelse(str_detect(rongo$finance_group_repeat_2_finance_group_name,"alpha"), 
          "alpha women group",
          ifelse(str_detect(rongo$finance_group_repeat_2_finance_group_name, "rongo dairy farmers"),
                 "rongo dairy sacco",
                 ifelse(str_detect(rongo$finance_group_repeat_2_finance_group_name, "silk"),
                        "sirk",
                        ifelse(str_detect(rongo$finance_group_repeat_2_finance_group_name, "skirt red|redskirt"), 
                               "redskirt women group",
                               as.character(rongo$finance_group_repeat_2_finance_group_name))))))


rongo$social_group_repeat_1_social_groups_info <- 
  (ifelse(str_detect(rongo$social_group_repeat_1_social_groups_info, "community welfare"),
          "community group",
          ifelse(str_detect(rongo$social_group_repeat_1_social_groups_info, "jouinor|junior"),
                 "junior youth group",
                 ifelse(str_detect(rongo$social_group_repeat_1_social_groups_info, "kagwana"),
                        "kagwana",
                        ifelse(str_detect(rongo$social_group_repeat_1_social_groups_info, "karende"),
                               "karende self-help group", 
                               ifelse(str_detect(rongo$social_group_repeat_1_social_groups_info,"sirk"), 
                                      "sirk",
                                      as.character(rongo$social_group_repeat_1_social_groups_info)))))))


rongo$social_group_repeat_2_social_groups_info <- 
  (ifelse(str_detect(rongo$social_group_repeat_2_social_groups_info, "community welfare"),
          "community group",
          ifelse(str_detect(rongo$social_group_repeat_2_social_groups_info, "jouinor|junior"),
                 "junior youth group",
                 ifelse(str_detect(rongo$social_group_repeat_2_social_groups_info, "kagwana"),
                        "kagwana",
                        ifelse(str_detect(rongo$social_group_repeat_2_social_groups_info, "karende"),
                               "karende self-help group", 
                               ifelse(str_detect(rongo$social_group_repeat_2_social_groups_info,"sirk"), 
                                      "sirk",
                                      as.character(rongo$social_group_repeat_2_social_groups_info)))))))

rongo$feed_training_source <- gsub("from ", rongo$feed_training_source, replacement = "")
levels(as.factor(rongo$feed_training_source))
rongo$feed_training_source <- gsub(" and", rongo$feed_training_source, replacement = ";")

rongo$feed_training_source[rongo$feed_training_source=="cooperative;heifer international"]<-
  "rongo dairy farmers cooperative; heifer international"
rongo$feed_training_source[rongo$feed_training_source=="the dairy cooperative; other farmers" ]<-
  "rongo dairy farmers cooperative; other farmers"

rongo$feed_training_source[rongo$feed_training_source=="dairy cooperative"]<-
  "rongo dairy farmers cooperative"

rongo_groups_cols <- c("farmer_group_repeat_1_group_name",
                 "farmer_group_repeat_2_group_name",
                 "religious_comm_repeat_1_which_religious_comm",
                 "finance_group_repeat_1_finance_group_name",
                 "finance_group_repeat_2_finance_group_name",
                 "social_group_repeat_1_social_groups_info",
                 "feed_training_source",
                 "vaccine_benefits_source")

rongo[rongo_groups_cols]<- lapply(rongo[rongo_groups_cols], gsub, pattern="from ", replacement="")
rongo[rongo_groups_cols]<- lapply(rongo[rongo_groups_cols], gsub, pattern=" and ", replacement=";")
rongo[rongo_groups_cols]<- lapply(rongo[rongo_groups_cols], gsub, pattern=",", replacement=";")
rongo[rongo_groups_cols]<- lapply(rongo[rongo_groups_cols], gsub, pattern="; ", replacement=";")
rongo[rongo_groups_cols]<- lapply(rongo[rongo_groups_cols], gsub, pattern="ngos", replacement="ngo")
rongo[rongo_groups_cols]<- lapply(rongo[rongo_groups_cols], gsub, pattern="other farmers", replacement="other farmer")
rongo[rongo_groups_cols]<- lapply(rongo[rongo_groups_cols], gsub, pattern="vets", replacement="vet")
rongo[rongo_groups_cols]<- lapply(rongo[rongo_groups_cols], gsub, pattern="the radio", replacement="radio")
rongo[rongo_groups_cols]<- lapply(rongo[rongo_groups_cols], gsub, pattern="the cooperative", replacement="rongo dairy farmers cooperative")

rongo[rongo=="a vet"]<-"vet"
rongo[rongo=="a private vet"]<-"private vet"
rongo[rongo=="government extension officers"]<-"government extensionists"
rongo[rongo=="kageso"|
        rongo=="kageso welfare group"]<-"kangeso women group"
rongo[rongo=="lwala"]<-"luala group"
rongo[rongo=="neigbour"]<-"other farmer"
rongo[rongo=="one arce fund"]<-"one acre fund"
rongo[rongo=="rongo dairies"|
        rongo=="rongo dairy"|
        rongo=="rongo dairy cooperative"]<-"rongo dairy farmers cooperative"
rongo[rongo=="rongo dairy;other farmer"]<-"rongo dairy farmers cooperative;other farmer"
rongo[rongo=="rongo dairy farmers sacco"]<-"rongo dairy sacco"
rongo[rongo=="the cooperative."]<-"rongo dairy farmers cooperative"
rongo[rongo=="the government vets;other farmer"]<-"government vet;other farmer"
rongo[rongo=="veterinary officer"]<-"vet"

rongo <- rongo %>% separate(vaccine_benefits_source, c("vaccine_benefits_source_1",
                                                       "vaccine_benefits_source_2",
                                                       "vaccine_benefits_source_3"), ";")
rongo <- rongo %>% separate(feed_training_source, c("feed_training_source_1",
                                                    "feed_training_source_2"
), ";")

#str_split_fixed(rongo$vaccine_benefits_source, ";", 3)
rongo[rongo=="dairy cooperative"|
        rongo=="cooperative"|
        rongo=="rongo ccf"]<-"rongo dairy farmers cooperative"
rongo[rongo=="extension officers"|
        rongo=="extensionists"]<-"other extension officers"
rongo[rongo=="government"|
        rongo=="government extensionists."|
        rongo=="government livestock department"|
        rongo=="government vet"|
        rongo=="kenya livestock officers"|
        rongo=="narigp"]<-"government extensionists"
rongo[rongo=="in the training i attended"|
        rongo=="school"|
        rongo=="the benchmarking"|
        rongo=="volunteer farmer trainers"]<-"training"
rongo[rongo=="area chief"|
        rongo=="chief"|
        rongo=="village elders"]<-"chief"
rongo[rongo=="journals"]<-"newspapers"
rongo[rongo=="ngo"]<-"other ngo"
rongo[rongo=="private vet"]<-"vet"

rongo<-rongo %>% 
  unite("no_feed_use",no_feed_use_1:no_feed_use_2, sep = ";", na.rm=TRUE) %>% 
  unite("hay_benefits_yes",hay_benefits_yes_1:hay_benefits_yes_2, sep = ";", na.rm=TRUE ) %>% 
  unite("vaccine_benefits_source",vaccine_benefits_source_1:vaccine_benefits_source_3, sep = ";", na.rm=TRUE ) %>% 
  unite("feed_training_source",feed_training_source_1:feed_training_source_2, sep = ";", na.rm=TRUE ) %>% 
  mutate_all(na_if,"") 

rongo$person_3[rongo$person_3=="daktari"]<-"daktari_unknown"

##vihiga####
vihiga <- raw_field %>% 
  filter(as.Date(date, "%d/%m/%Y")>"2022-03-06") %>% 
  mutate(parti_name=str_to_lower(parti_name))


vihiga[vihiga=="frdrick zelovi"]<-"fredrick zelovi"
vihiga[vihiga=="josina lugalishi"]<-"josina lugarishi"
vihiga[vihiga=="rebecca swahili"|
            vihiga=="rebeka kasudi"]<-"rebecca kasudi swahili"
vihiga[vihiga=="margrate hambwe"]<-"margaret hambwe"
vihiga[vihiga=="homesley vulimu"]<-"wormsley vulimu"
vihiga[vihiga=="mike misalia"]<-"michael musalia"
vihiga[vihiga=="henry mengesa"]<-"margaret mwambasani"
vihiga[vihiga=="harun ogadi/ruth kasoa"|
            vihiga=="harun ongadi"]<-"ruth kasoa"
vihiga[vihiga=="frederick odiege"]<-"fredrick odiege"
vihiga[vihiga=="cyprus majanga"]<-"cyrus aluvisia"
vihiga[vihiga=="warmsley vulimu"|vihiga=="wordsley vulimu"]<-"wormsley vulimu"
vihiga[vihiga=="raphael makamu"]<-"raphael magamu"
vihiga[vihiga=="edga lohondo"]<-"edgar lihondo"
vihiga[vihiga=="nixom kidim"]<-"nickson kidmu"
vihiga[vihiga=="azibeta chazima"]<-"azibeta agiza"
vihiga[vihiga=="beatrice mahuku"]<-"beatrice maugo"
vihiga[vihiga=="dason kaverenge"]<-"danson kiverenge"
vihiga[vihiga=="defina mugora"]<-"difina mbone mugoha"
vihiga[vihiga=="esther dambiza"]<-"esther naomo dambiza"
vihiga[vihiga=="evans seva"]<-"evans luvinzu"
vihiga[vihiga=="hesbon luma"|
            vihiga=="hesbon avedi"|
            vihiga=="hesbon lluma"|
            vihiga=="hesbon lluma"]<- "hesborn lluma"
vihiga[vihiga=="margret kaluli"|
            vihiga=="margret karudi"]<-"margaret kaluli"
vihiga[vihiga=="nancy dembede"|
            vihiga=="nancy debende"|vihiga=="nancy dembele"]<-"nancy debede"
vihiga[vihiga=="mary rugeywa"|
            vihiga=="mary lugeywa"]<-"mary logeywa"
vihiga[vihiga=="peter muenge"]<-"peter muhenge"
vihiga[vihiga=="safan arigora"]<-"safania arigora"
vihiga[vihiga=="zainabu mbona"]<-"zainabu mbone"
vihiga[vihiga=="gloria kirai/benedict kiwanuka"]<-"benedict kiwanuka/gloria kirai"
vihiga[vihiga=="margret mengesa"]<-"margaret mwambasani"
vihiga[vihiga=="micheal musalia/maxmilla"|
            vihiga=="maxmilla kavai"]<-"micheal musalia"
vihiga[vihiga=="rose isavo"]<-"rose isabwa"
vihiga[vihiga=="ruth ongati"]<-"ruth kasoa"
vihiga[vihiga=="micheal musalia"]<-"michael musalia"
vihiga[vihiga=="stanley rumwaji"]<-"muhonja christabel"
vihiga[vihiga=="wiliam malesi"]<-"william malesi"
vihiga[vihiga=="danson kiverenge"]<-"dason kiverenge"
vihiga[vihiga=="mary khadambi"|
            vihiga=="mary khatambi"]<-"consolata mary khatambi"
vihiga[vihiga=="lugai raphael"]<-"raphael lugai"
vihiga[vihiga=="zephaniah aligora"]<-"safania arigora"
vihiga[vihiga=="difina mugoa"]<-"difina mbone mugoha"
vihiga[vihiga=="fredrick odiege"]<-"frederick ondiege"
vihiga[vihiga=="maheri"]<-"alfred maheli"
vihiga[vihiga=="azibeta"]<-"azibeta agiza"
vihiga[vihiga=="beatrice kaungusia"]<-"beatrice maugo"


vihiga$parti_phone <- ifelse(vihiga$parti_name=="mary logeywa", 727552712, vihiga$parti_phone)

vihiga[vihiga=="milulu"]<-"mululu"
vihiga[vihiga=="kivaze"| vihiga=="kivaze a"| vihiga=="kivaze b"]<-"kivazi"
vihiga[vihiga=="kaimedi"|
            vihiga=="kaimendi"|
            vihiga=="gaimendi"|vihiga=="ngaimedi"|vihiga=="ngaimendi"]<-"gaimedi"


for (k in 14:17) {
  for (j in 1:nrow(vihiga)){
    vihiga[j,k] <-   ifelse(
      vihiga[j,k-12]%in%vihiga$parti_name,
      as.character(vihiga$parti_phone[which(vihiga$parti_name==vihiga[j,k-12])]),
      as.character(vihiga[j,k])
    )
  }
}


for(i in 9:12){
  vihiga[i]<- lapply(vihiga[i], as.character)
  for(j in 1:nrow(vihiga)){
    
    vihiga[j,i] <-   ifelse( vihiga[j,i-7]%in%vihiga$parti_name, as.character(vihiga$village_name[which(vihiga$parti_name==vihiga[j,i-7])]),
                                as.character(vihiga[j,i]))
  }
}

vihiga$neg_relationship_village <- ifelse(str_detect(vihiga$neg_relationship_village, "dispute"),
                                             "disputes",
                                             ifelse(str_detect(vihiga$neg_relationship_village, "on their own"),
                                                    "lack of cummunity spirit", 
                                                    "other"))


vihiga_qual <- c("no_feed_use",
          "feed_no_recomm",
          "hay_benefits_yes",
          "hay_making_no_recomm",
          "vaccine_no_reason",
          "vaccine_no_recomm",
          "ai_recommend_no",
          "hired_recommend_no")
vihiga[vihiga_qual] <- lapply(vihiga[vihiga_qual], gsub, pattern=",", replacement=";")
vihiga[vihiga_qual] <- lapply(vihiga[vihiga_qual], gsub, pattern="  ", replacement=" ")
vihiga[vihiga_qual] <- lapply(vihiga[vihiga_qual], gsub, pattern=" and ", replacement=";")
vihiga[vihiga_qual] <- lapply(vihiga[vihiga_qual], gsub, pattern=" ;", replacement=";")
vihiga[vihiga_qual] <- lapply(vihiga[vihiga_qual], gsub, pattern="; ", replacement=";")

vihiga <- vihiga %>% separate(no_feed_use, c("no_feed_use_1",
                                                   "no_feed_use_2"), ";")
vihiga <- vihiga %>% separate(hay_benefits_yes, c("hay_benefits_yes_1",
                                                        "hay_benefits_yes_2"), ";")

vihiga$no_feed_use_1 <- ifelse(str_detect(vihiga$no_feed_use_1, ("expens|cost|finance")),
                                  "expensive", 
                                  ifelse(str_detect(vihiga$no_feed_use_1, ("cow|free")),
                                         "unimproved or free range cow/s",
                                         ifelse(str_detect(vihiga$no_feed_use_1, ("idea|know")),
                                                "lack of knowledge",
                                                ifelse(str_detect(vihiga$no_feed_use_1, ("seed|available|do not have|don't have|doesn't have|don't own|don't produce|doesn't grow")),
                                                       "no access to seed",
                                                       ifelse(str_detect(vihiga$no_feed_use_1, ("land|size|space|small|limited|lznd")),
                                                              "limited space", 
                                                              ifelse(str_detect(vihiga$no_feed_use_1, "time"),
                                                                     "not enough time",
                                                                     "other"))))))


vihiga$no_feed_use_2 <- ifelse(str_detect(vihiga$no_feed_use_2, ("expens|cost|finance")),
                                  "expensive", 
                                  ifelse(str_detect(vihiga$no_feed_use_2, ("cow|free")),
                                         "unimproved or free range cow/s",
                                         ifelse(str_detect(vihiga$no_feed_use_2, ("idea|know")),
                                                "lack of knowledge",
                                                ifelse(str_detect(vihiga$no_feed_use_2, ("seed|available")),
                                                       "no access to seed",
                                                       ifelse(str_detect(vihiga$no_feed_use_2, ("land|size|space|small|limited")),
                                                              "limited space", 
                                                              ifelse(str_detect(vihiga$no_feed_use_2, "time"),
                                                                     "not enough time",
                                                                     "other"))))))

vihiga$feed_no_recomm <- ifelse(str_detect(vihiga$feed_no_recomm, ("expens|cost|finance")),
                                   "expensive", 
                                   ifelse(str_detect(vihiga$feed_no_recomm, ("cow|free")),
                                          "unimproved or free range cow/s",
                                          ifelse(str_detect(vihiga$feed_no_recomm, ("idea|know")),
                                                 "lack of knowledge",
                                                 ifelse(str_detect(vihiga$feed_no_recomm, ("seed|available")),
                                                        "no access to seed",
                                                        ifelse(str_detect(vihiga$feed_no_recomm, ("land|size|space|small|limited")),
                                                               "limited space", 
                                                               ifelse(str_detect(vihiga$feed_no_recomm, "time"),
                                                                      "not enough time",
                                                                      vihiga$feed_no_recomm))))))

vihiga$hay_benefits_yes_1 <- ifelse(str_detect(vihiga$hay_benefits_yes_1, ("expens|cost|finance|capital|funds|sive")),
                                       "expensive", 
                                       ifelse(str_detect(vihiga$hay_benefits_yes_1, ("no enough|not enough|doesn't have enough|lack of enough|doesn't grow|do not have enough|no fodder|no feeds|resource|material")),
                                              "no/too little fodder",
                                              ifelse(str_detect(vihiga$hay_benefits_yes_1, ("know|skill|practice|aware|capacity|no idea|knwo")),
                                                     "lack of knowledge",
                                                     ifelse(str_detect(vihiga$hay_benefits_yes_1, "difficult|time|tedious|ectic"),
                                                            "not enough time/too difficult",
                                                            ifelse(str_detect(vihiga$hay_benefits_yes_1, ("small|room|space")),
                                                                   "limited space", 
                                                                   "other")))))

vihiga$hay_benefits_yes_2 <- ifelse(str_detect(vihiga$hay_benefits_yes_2, ("financ|capital")),
                                       "expensive",
                                       ifelse(str_detect(vihiga$hay_benefits_yes_2, "difficult|time|tedious|ectic"),
                                              "not enough time/too difficult",
                                              "other"))


vihiga$vaccine_no_reason<- ifelse(str_detect(vihiga$no_feed_use_2, ("idea")),
                                     "lack of knowledge",
                                     vihiga$vaccine_no_reason)

vihiga_groups_cols <- c("farmer_group_repeat_1_group_name",
                 "farmer_group_repeat_2_group_name",
                 "religious_comm_repeat_1_which_religious_comm",
                 "finance_group_repeat_1_finance_group_name",
                 "finance_group_repeat_2_finance_group_name",
                 "social_group_repeat_1_social_groups_info",
                 "feed_training_source",
                 "vaccine_benefits_source")


vihiga[vihiga_groups_cols]<- lapply(vihiga[vihiga_groups_cols], gsub, pattern="from ", replacement="")
vihiga[vihiga_groups_cols]<- lapply(vihiga[vihiga_groups_cols], gsub, pattern=" and ", replacement=";")
vihiga[vihiga_groups_cols]<- lapply(vihiga[vihiga_groups_cols], gsub, pattern=",", replacement=";")
vihiga[vihiga_groups_cols]<- lapply(vihiga[vihiga_groups_cols], gsub, pattern="; ", replacement=";")


vihiga[vihiga=="busare"]<-"busare dairy group"
vihiga[vihiga=="caritus"]<-"caritas"
vihiga[vihiga=="chamakanga"|
        vihiga=="chemakanga sublocation welfare group"|
        vihiga=="chamakanga team welfare"|
        vihiga=="chamakanga welfare"|
        vihiga=="chemakanga welfare group"]<-"chamakanga welfare group"
vihiga[vihiga=="christians"|
        vihiga=="church"]<-"christian"
vihiga[vihiga=="community welfare"]<-"community welfare group"
vihiga[vihiga=="jimodi welfare"]<-"jimodi welfare group"
vihiga[vihiga=="kitieto"]<-"kitiezo"
vihiga[vihiga=="trained by narigp project"]<-"narigp project"
vihiga[vihiga=="okoa group"]<-"okoa local farmer group"
vihiga[vihiga=="tea farming group"]<-"tea farmers association"
vihiga[vihiga=="village elders"]<-"village leaders"
vihiga[vihiga=="vumilia dairy"|
        vihiga=="vumilia"|
        vihiga=="vumilia group"|
        vihiga=="vumulia"]<-"vumilia dairy group"
vihiga[vihiga=="worth group"]<-"worth"
vihiga[vihiga=="a vet"]<-"vet"
vihiga[vihiga=="caritas"|vihiga=="caritus group"]<-"caritas group"
vihiga[vihiga=="farmers groups;lead farmers;ngos"]<-"farmers group;lead farmers;ngo"
vihiga[vihiga=="fips farm inputs promotion africa"|
        vihiga=="fips, farm inputs promotion africa"]<-"fips;farm inputs promotion africa"
vihiga[vihiga=="got trainingfips in oneof tge farmers farm"]<-"fips"
vihiga[vihiga=="government extensionist"]<-"government extensionists"
vihiga[vihiga=="agroecology"]<-"agroecology group"
vihiga[vihiga=="agroecology"]<-"agroecology group"
vihiga$finance_group_repeat_1_finance_group_name[vihiga$finance_group_repeat_1_finance_group_name=="dairy group"]<-"vumilia dairy group"#same village as others, so change not made for other person from "dairy group" from diff village
vihiga[vihiga=="muga self help group."]<-"muga self help group"
vihiga[vihiga=="n.g.o"]<-"ngo"
vihiga[vihiga_groups_cols]<- lapply(vihiga[vihiga_groups_cols], gsub, pattern="ngos", replacement="ngo")
vihiga[vihiga=="trained by the narigp project"]<-"narigp project"
vihiga[vihiga=="ngo project"]<-"ngo"
vihiga[vihiga=="has no name"]<-"no name"
vihiga[vihiga=="okoa local farmer group"|
        vihiga=="okoa chicken farmers"|
        vihiga=="okoa"]<-"okoa chicken farmers group"
vihiga[vihiga_groups_cols]<- lapply(vihiga[vihiga_groups_cols], gsub, pattern="other farmers", replacement="other farmer")
vihiga[vihiga=="prevent diseases"]<-NA
vihiga[vihiga=="vet;my neighbour"]<-"vet;other farmer"
vihiga[vihiga=="vets"]<-"vet"
vihiga[vihiga=="vumilia dairy farmers"|
        vihiga=="vumilia farmers"|
        vihiga=="vumilia farmers group"]<-"vumilia dairy group"
vihiga[vihiga=="worth"|
        vihiga=="worth program"]<-"worth women group"
vihiga[vihiga=="baraka"]<-"baraza"
vihiga[vihiga=="chama ya huruma"]<-"chama cha wazi"
vihiga[vihiga=="mululu welfare"]<-"mululu women group"
vihiga$finance_group_repeat_1_finance_group_name[vihiga$finance_group_repeat_1_finance_group_name=="mululu"]<-"mululu women group"
vihiga[vihiga=="tuinuane"]<-"two in one"

vihiga <- vihiga %>% separate(vaccine_benefits_source, c("vaccine_benefits_source_1",
                                                       "vaccine_benefits_source_2"), ";")
vihiga <- vihiga %>% separate(feed_training_source, c("feed_training_source_1",
                                                    "feed_training_source_2",
                                                    "feed_training_source_3"
), ";")

vihiga[vihiga=="community vet"]<-"vet"

vihiga[vihiga=="country government"|
        vihiga=="government extensionists"|
        vihiga=="government vets"|
        vihiga=="narig"|
        vihiga=="narigp project"]<-"government extensionists"#putting gov vets under gov not vets

vihiga[vihiga=="extension officer"]<-"other extension officers"

vihiga[vihiga=="lead farmer"|
        vihiga=="volunteers farmers"]<-"lead farmers"

vihiga[vihiga=="ngo"]<-"other ngo"

vihiga[vihiga=="neighbour"]<-"other farmer"

vihiga[vihiga=="school"|
        vihiga=="volunteer farmer trainer"]<-"training"
vihiga[vihiga=="farm inputs promotion africa"]<-"fips"
vihiga[vihiga=="area chief"|
        vihiga=="village leaders"]<-"village chief/s"

vihiga<-vihiga %>% 
  unite("no_feed_use",no_feed_use_1:no_feed_use_2, sep = ";", na.rm=TRUE) %>% 
  unite("hay_benefits_yes",hay_benefits_yes_1:hay_benefits_yes_2, sep = ";", na.rm=TRUE ) %>% 
  unite("vaccine_benefits_source",vaccine_benefits_source_1:vaccine_benefits_source_2, sep = ";", na.rm=TRUE ) %>% 
  unite("feed_training_source",feed_training_source_1:feed_training_source_3, sep = ";", na.rm=TRUE ) %>% 
  mutate_all(na_if,"") 

##siaya####
siaya <- raw_field %>% 
  filter(as.Date(date, "%d/%m/%Y")>"2022-03-02") %>% 
  filter(as.Date(date, "%d/%m/%Y")<"2022-03-06") %>% 
  mutate(parti_name=str_to_lower(parti_name))

siaya[siaya=="odhiambo"]<-"odhiambo george"
siaya[siaya=="alex madara olima"]<-"alex olima"
siaya[siaya=="felisia omondi"]<-"felesia omondi"
siaya[siaya=="odhiambo george/beatrice atieno"]<-"odhiambo george"
siaya[siaya=="owino"]<-"owino frederick/yvonne"
siaya[siaya=="oduge jalongo"]<-"odunge jalango"
siaya[siaya=="willingtone wesonga"]<-"willington wesonga"
siaya[siaya=="eric oduor"]<-"eric oduor/pamela adhiambo"
siaya[siaya=="753625751"]<-"718627333"
siaya[siaya=="wilson otieno wessa"]<-"willson ochieng wessa"
siaya[siaya=="meshach ogada"]<-"peris ogada"#same hh
siaya[siaya=="lillian awuor/john ouma"]<-"lillian awuor"
siaya[siaya=="margaret oloo"]<-"margaret adipo oloo"
siaya[siaya=="omondi george"]<-"george omondi onduge"
siaya[siaya=="odero rose"]<-"rose odero"
siaya[siaya=="nyasidindi"]<-"janet adhiambo obilo"
siaya[siaya=="alfred wanyera"]<-"rosemary wasonga/alfred wanyara"
siaya[siaya=="otieno jared"]<-"jared otieno"
siaya[siaya=="722269246"]<-"722269243"
siaya[siaya=="jane opondo"|
            siaya=="jane okwondo"]<-"jane anyango wanderi"
siaya[siaya=="eunice ochieng"|
            siaya=="eunice achieng"]<-"eunice akinyi ochieng"
siaya[siaya=="george ochieng"]<-"george ochieng onyango"
siaya[siaya=="james nyamoro"]<-"janes omondi"
siaya[siaya=="mathew's oduor"]<-"mathew's oduor/hellen"
siaya[siaya=="prisca odur"]<-"plista oduor"
siaya[siaya=="rose apio"|siaya=="rosaline apiyo"]<-"rosaline apio"
siaya[siaya=="anna njeri"]<-"anna wandere njeri"
siaya[siaya=="caleb obuor"]<-"caleb oburu wandere"
siaya[siaya=="consolota atieno"]<-"consolata musula"
siaya[siaya=="francis and onyango ayiemba"]<-"francis onyango ayiemba"
siaya[siaya=="john ouma"]<-"lillian awuor" # though to be same household based on same phone no.
siaya[siaya=="mama oloo"]<-"margaret adipo oloo" #assuming same since oloo unique 
siaya[siaya=="ochieng/angeline achieng"]<-"angeline achieng"
siaya[siaya=="phillip mugono"]<-"phillip migono"
siaya[siaya=="mary okelo"|
            siaya=="mary siaku"|
            siaya=="mary agutu"]<-"mary agutu okelo" #steve
siaya[siaya=="omondi jader"]<-"omunya jader"#steve same hh 
siaya[siaya=="plista atieno"]<-"plista oduor"#steve
siaya[siaya=="wycliffe odera"]<-"wiclife odera"#steve checked via mpesa 



siaya$person_2_number <- ifelse(siaya$person_2==
                                      "fred oguga", 
                                    721617529, siaya$person_2_number)

siaya$person_1_number <- ifelse(siaya$person_1==
                                      "meshach ogada", 
                                    721171436, siaya$person_1_number)

siaya[siaya=="sugulu"|
            siaya=="sgulu"|
            siaya=="sogulu"|
            siaya=="sigula"|
            siaya=="sngulu"]<-"sigulu"
siaya[siaya=="onono village"]<-"onono"
siaya[siaya=="hasero"|
            siaya=="hosera"]<-"asero"
siaya[siaya=="nalo a"]<-"nalo"

for (l in 9:10) {
  for (j in 1:nrow(siaya)) {
    siaya[j,l]<- ifelse(siaya[j,l-7]=="fred oguga", "onono", as.character(siaya[j,l]))
  }
}

for (k in 14:17) {
  for (j in 1:nrow(siaya)){
    siaya[j,k] <-   ifelse(
      siaya[j,k-12]%in%siaya$parti_name,
      as.character(siaya$parti_phone[which(siaya$parti_name==siaya[j,k-12])]),
      as.character(siaya[j,k])
    )
  }
}


for(i in 9:12){
  siaya[i]<- lapply(siaya[i], as.character)
  for(j in 1:nrow(siaya)){
    
    siaya[j,i] <-   ifelse( siaya[j,i-7]%in%siaya$parti_name, as.character(siaya$village_name[which(siaya$parti_name==siaya[j,i-7])]),
                                as.character(siaya[j,i]))
  }
}

siaya$neg_relationship_village <- ifelse(str_detect(siaya$neg_relationship_village, "dispute"),
                                             "disputes",
                                             ifelse(str_detect(siaya$neg_relationship_village, "on their own"),
                                                    "lack of cummunity spirit", 
                                                    "other"))


siaya_qual <- c("no_feed_use",
          "feed_no_recomm",
          "hay_benefits_yes",
          "hay_making_no_recomm",
          "vaccine_no_reason",
          "vaccine_no_recomm",
          "ai_recommend_no",
          "hired_recommend_no")
siaya[siaya_qual] <- lapply(siaya[siaya_qual], gsub, pattern=",", replacement=";")
siaya[siaya_qual] <- lapply(siaya[siaya_qual], gsub, pattern="  ", replacement=" ")
siaya[siaya_qual] <- lapply(siaya[siaya_qual], gsub, pattern=" and ", replacement=";")
siaya[siaya_qual] <- lapply(siaya[siaya_qual], gsub, pattern=" ;", replacement=";")
siaya[siaya_qual] <- lapply(siaya[siaya_qual], gsub, pattern="; ", replacement=";")

siaya <- siaya %>% separate(no_feed_use, c("no_feed_use_1",
                                                   "no_feed_use_2"), ";")
siaya <- siaya %>% separate(hay_benefits_yes, c("hay_benefits_yes_1",
                                                        "hay_benefits_yes_2"), ";")

siaya$no_feed_use_1 <- ifelse(str_detect(siaya$no_feed_use_1, ("expens|cost|finance")),
                                  "expensive", 
                                  ifelse(str_detect(siaya$no_feed_use_1, ("cow|free")),
                                         "unimproved or free range cow/s",
                                         ifelse(str_detect(siaya$no_feed_use_1, ("idea|know")),
                                                "lack of knowledge",
                                                ifelse(str_detect(siaya$no_feed_use_1, ("seed|available|do not have|don't have|doesn't have|don't own|don't produce|doesn't grow")),
                                                       "no access to seed",
                                                       ifelse(str_detect(siaya$no_feed_use_1, ("land|size|space|small|limited|lznd")),
                                                              "limited space", 
                                                              ifelse(str_detect(siaya$no_feed_use_1, "time"),
                                                                     "not enough time",
                                                                     "other"))))))


siaya$no_feed_use_2 <- ifelse(str_detect(siaya$no_feed_use_2, ("expens|cost|finance")),
                                  "expensive", 
                                  ifelse(str_detect(siaya$no_feed_use_2, ("cow|free")),
                                         "unimproved or free range cow/s",
                                         ifelse(str_detect(siaya$no_feed_use_2, ("idea|know")),
                                                "lack of knowledge",
                                                ifelse(str_detect(siaya$no_feed_use_2, ("seed|available")),
                                                       "no access to seed",
                                                       ifelse(str_detect(siaya$no_feed_use_2, ("land|size|space|small|limited")),
                                                              "limited space", 
                                                              ifelse(str_detect(siaya$no_feed_use_2, "time"),
                                                                     "not enough time",
                                                                     "other"))))))

siaya$feed_no_recomm <- ifelse(str_detect(siaya$feed_no_recomm, ("expens|cost|finance")),
                                   "expensive", 
                                   ifelse(str_detect(siaya$feed_no_recomm, ("cow|free")),
                                          "unimproved or free range cow/s",
                                          ifelse(str_detect(siaya$feed_no_recomm, ("idea|know")),
                                                 "lack of knowledge",
                                                 ifelse(str_detect(siaya$feed_no_recomm, ("seed|available")),
                                                        "no access to seed",
                                                        ifelse(str_detect(siaya$feed_no_recomm, ("land|size|space|small|limited")),
                                                               "limited space", 
                                                               ifelse(str_detect(siaya$feed_no_recomm, "time"),
                                                                      "not enough time",
                                                                      siaya$feed_no_recomm))))))

siaya$hay_benefits_yes_1 <- ifelse(str_detect(siaya$hay_benefits_yes_1, ("expens|cost|finance|capital|funds|sive")),
                                       "expensive", 
                                       ifelse(str_detect(siaya$hay_benefits_yes_1, ("no enough|not enough|doesn't have enough|lack of enough|doesn't grow|do not have enough|no fodder|no feeds|resource|material")),
                                              "no/too little fodder",
                                              ifelse(str_detect(siaya$hay_benefits_yes_1, ("know|skill|practice|aware|capacity|no idea|knwo")),
                                                     "lack of knowledge",
                                                     ifelse(str_detect(siaya$hay_benefits_yes_1, "difficult|time|tedious|ectic"),
                                                            "not enough time/too difficult",
                                                            ifelse(str_detect(siaya$hay_benefits_yes_1, ("small|room|space")),
                                                                   "limited space", 
                                                                   "other")))))

siaya$hay_benefits_yes_2 <- ifelse(str_detect(siaya$hay_benefits_yes_2, ("financ|capital")),
                                       "expensive",
                                       ifelse(str_detect(siaya$hay_benefits_yes_2, "difficult|time|tedious|ectic"),
                                              "not enough time/too difficult",
                                              "other"))


siaya$vaccine_no_reason<- ifelse(str_detect(siaya$no_feed_use_2, ("idea")),
                                     "lack of knowledge",
                                     siaya$vaccine_no_reason)


siaya_groups_cols <- c("farmer_group_repeat_1_group_name",
                 "farmer_group_repeat_2_group_name",
                 "religious_comm_repeat_1_which_religious_comm",
                 "finance_group_repeat_1_finance_group_name",
                 "finance_group_repeat_2_finance_group_name",
                 "social_group_repeat_1_social_groups_info",
                 "feed_training_source",
                 "vaccine_benefits_source")

siaya[siaya_groups_cols]<- lapply(siaya[siaya_groups_cols], gsub, pattern="from ", replacement="")
siaya[siaya_groups_cols]<- lapply(siaya[siaya_groups_cols], gsub, pattern=" and ", replacement=";")
siaya[siaya_groups_cols]<- lapply(siaya[siaya_groups_cols], gsub, pattern=",", replacement=";")
siaya[siaya_groups_cols]<- lapply(siaya[siaya_groups_cols], gsub, pattern="; ", replacement=";")
siaya[siaya_groups_cols]<- lapply(siaya[siaya_groups_cols], gsub, pattern="ngos", replacement="ngo")
siaya[siaya_groups_cols]<- lapply(siaya[siaya_groups_cols], gsub, pattern="other farmers", replacement="other farmer")


siaya[siaya=="chamluchi"]<-"chamluchi gi adier"
siaya[siaya=="jirani group"]<-"jirani"
siaya[siaya=="lolwe womans group"|
        siaya=="lolwe womens group"]<-"lolwe women group"
siaya[siaya=="ndiany group"]<-"ndiany"
siaya[siaya=="the radio"]<-"radio"
siaya[siaya=="sirk"|
        siaya=="silk women's group"]<-"sirk women group"
siaya[siaya=="my vet"|
        siaya=="the vet"]<-"vet"
siaya[siaya=="a farmers group"]<-"farmers group"
siaya[siaya=="ack"]<-"ack self-help group"
siaya[siaya=="ajwang"|
        siaya=="ajuang"]<-"ajuang women group"
siaya[siaya=="community welfare"]<-"community welfare group"
siaya[siaya=="does not remember the name of the group"]<-"no name"
siaya[siaya=="farmer trainings"|
        siaya=="farmwrs training"]<-"farmer training"
siaya[siaya=="frpm people in other vollages"]<-"other farmer"
siaya[siaya=="god last appeal"]<-"god last appeal church"
siaya[siaya=="got information village elders;assistant chief"]<-"village elders;assistant chief"
siaya[siaya=="government extension officer;kcap"]<-"government extensionists;kcap"
siaya[siaya=="government extension officers through county"]<-"county government extensionists"
siaya[siaya=="government extensionists."]<-"government extensionists"
siaya[siaya=="group asero"]<-"hasero group"
siaya[siaya=="jirani"]<-"jirani mwema women group"
siaya[siaya=="konyiri kendi"|
        siaya=="konyri kendi"]<-"konyiri kendi community group"
siaya[siaya=="lolwe friends welfare"|
        siaya=="lolwe group"|
        siaya=="lolwe hgroup"|
        siaya=="lolwe friends welfare group"]<-"lolwe women group"
siaya[siaya=="neibbours"]<-"other farmer"
siaya[siaya=="radio;anorher farmer"]<-"radio;other farmer"
siaya[siaya=="sugula"]<-"sugulu welfare group"
siaya[siaya=="sugulu farmers;uguja farmers group"]<-"sugulu welfare group;uguja farmers group"
siaya[siaya=="trainings the county government"]<-"country government"
siaya[siaya=="upendo"]<-"upendo iro women group"

for (n in 18:ncol(siaya)) {
  for (j in 1:nrow(siaya)) {
    siaya[j,n]<-ifelse(str_detect(siaya[j,n], "siaya"), 
                       "siaya bee keepers and processing",
                       ifelse(str_detect(siaya[j,n], "hosera"),
                              "hasero group",
                              ifelse(str_detect(siaya[j,n],"christian"), 
                                     "church",
                                     as.character(siaya[j,n]))))
  }
}


siaya <- siaya %>% separate(vaccine_benefits_source, c("vaccine_benefits_source_1",
                                                       "vaccine_benefits_source_2",
                                                       "vaccine_benefits_source_3"), ";")
siaya <- siaya %>% separate(feed_training_source, c("feed_training_source_1",
                                                    "feed_training_source_2"
), ";")

siaya[siaya=="chiff"|
        siaya=="area chief"|
        siaya=="assistant chief"|
        siaya=="village elder"|
        siaya=="village elders"]<-"chief"

siaya[siaya=="county government"|
        siaya=="county government extensionists"|
        siaya=="government extensionists"]<-"government extensionists"

siaya[siaya=="farmer training"|
        siaya=="trainings have received"]<-"training"

siaya[siaya=="television."]<-"television"

siaya<-siaya %>% 
  unite("no_feed_use",no_feed_use_1:no_feed_use_2, sep = ";", na.rm=TRUE) %>% 
  unite("hay_benefits_yes",hay_benefits_yes_1:hay_benefits_yes_2, sep = ";", na.rm=TRUE ) %>% 
  unite("vaccine_benefits_source",vaccine_benefits_source_1:vaccine_benefits_source_3, sep = ";", na.rm=TRUE ) %>% 
  unite("feed_training_source",feed_training_source_1:feed_training_source_2, sep = ";", na.rm=TRUE ) %>% 
  mutate_all(na_if,"") 

#binding####
siaya <- siaya %>% 
  mutate(county="siaya")
rongo <- rongo %>% 
  mutate(county="rongo")
vihiga <- vihiga %>% 
  mutate(county="vihiga")
oyugis <- oyugis %>% 
  mutate(county="oyugis")

clean_field <- rbind(siaya, rongo, vihiga, oyugis)

#class determination####
clean_field<-clean_field %>% 
  rename(person_4_distance=person_4_disance,
         person_3_distance=person_3_disance) %>% 
  select(-date, -time, -parti_phone, -person_1_number,-person_2_number,-person_3_number,-person_4_number) %>% 
  mutate_if(grepl("geo_",colnames(.)), as.logical) %>% #prurposely misclassified to remove later
  remove_empty(c("cols")) %>% 
  mutate_if(colnames(.)%in%numeric_vars$skim_variable|grepl("helpful",colnames(.))|grepl("distance",colnames(.)), as.numeric) %>%
  mutate_if(colnames(.)%in%logical_vars_2$skim_variable, as.logical) %>% 
  mutate_if(is.character, as.factor)

#sense checking####
skim(clean_field)

clean_field<-clean_field %>% 
  mutate(con_personal=ifelse(parti_name%in%c("zechariah okongo ochieng","okoko abel","emily nyamburi"), NA, con_personal),
         sell_coop=ifelse(parti_name%in%c("zechariah okongo ochieng","okoko abel","emily nyamburi"), NA, sell_coop),
         sell_market=ifelse(parti_name%in%c("zechariah okongo ochieng","okoko abel","emily nyamburi"), NA, sell_market)) %>% 
  mutate(sell_coop=ifelse(parti_name=="george okwach", 50, sell_coop)) %>% 
  mutate(con_personal=ifelse(parti_name%in%c("bonface okinda","raphael odoro"), con_personal/0.9, con_personal),
                  sell_coop=ifelse(parti_name%in%c("bonface okinda","raphael odoro"), sell_coop/0.9, sell_coop),
                  sell_market=ifelse(parti_name%in%c("bonface okinda","raphael odoro"), sell_market/0.9, sell_market))

#anonymization####

persons <- clean_field %>% 
  select(county,parti_name, person_1, person_2, person_3, person_4) %>% 
  mutate_all(as.character) %>% 
  pivot_longer(cols=c(parti_name, person_1, person_2, person_3, person_4), values_to = "person") %>% 
  arrange(name) %>% 
  select(-name) %>% 
  distinct(person, .keep_all = T) %>% 
  drop_na(person)

persons <- persons %>% 
  mutate(c=case_when(county=="siaya"~"s",
                     county=="vihiga"~"v",
                     county=="rongo"~"r",
                     county=="oyugis"~"o",
                     TRUE~"ISSUE"),
         cc=ifelse(person%in%clean_field$parti_name, "p", "x"))

persons <- cbind(persons, "code"=1:nrow(persons)) %>% 
  mutate(code=str_pad(code, width = 3, side = c("left"), pad="0")) %>% 
  unite("code", c:code, sep = "")
  
  
clean_field<- clean_field %>% 
  mutate(across(c(parti_name, person_1, person_2, person_3, person_4), as.character))
for (n in 1:nrow(persons)) {
  clean_field[clean_field==(persons$person[n])]<-(persons$code[n])
}
clean_field<- clean_field %>% 
  mutate(across(c(parti_name, person_1, person_2, person_3, person_4), as.factor))

#consolidating and exporting####
field_data<-clean_field
write_csv(field_data,"field_data.csv")