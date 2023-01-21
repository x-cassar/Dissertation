
#share of adopters in network 
mnq09_data_c<-merge(
  merge(
    expanded_data %>% 
      select(parti_name, person_1:person_4) %>% 
      pivot_longer(cols = c(2:5), 
                   names_to = "x",
                   values_to = "edge") %>% 
      drop_na() %>% 
      rename("node"=parti_name)
    ,
    expanded_data %>% 
      select(parti_name, crossbreed_use)
    ,
    by.x = "edge", by.y = "parti_name",
    all.x = T, all.y = F
  ) %>% 
    select(node, x,edge, crossbreed_use) %>%   
    select(-edge) %>% 
    pivot_wider(names_from = x, values_from = crossbreed_use) %>% 
    mutate_if(is.logical, as.numeric) %>% 
    mutate(users=rowSums(across(where(is.numeric)), na.rm=T)) %>% 
    mutate(valid=rowSums(!is.na(.))-2) %>% 
    mutate(use_rate=ifelse(users/valid>=0,users/valid, NA_real_ ))%>% 
    select(node, users, use_rate)
  ,
  expanded_data,
  by.y = "parti_name", by.x = "node",
  all.x = T, all.y = F
)

mnq09_data_v<-merge(
  merge(
    expanded_data %>% 
      select(parti_name, person_1:person_4) %>% 
      pivot_longer(cols = c(2:5), 
                   names_to = "x",
                   values_to = "edge") %>% 
      drop_na() %>% 
      rename("node"=parti_name)
    ,
    expanded_data %>% 
      select(parti_name, vaccine_use)
    ,
    by.x = "edge", by.y = "parti_name",
    all.x = T, all.y = F
  ) %>% 
    select(node, x,edge, vaccine_use) %>%   
    select(-edge) %>% 
    pivot_wider(names_from = x, values_from = vaccine_use) %>% 
    mutate_if(is.logical, as.numeric) %>% 
    mutate(users=rowSums(across(where(is.numeric)), na.rm=T)) %>% 
    mutate(valid=rowSums(!is.na(.))-2) %>% 
    mutate(use_rate=ifelse(users/valid>=0,users/valid, NA_real_ ))%>% 
    select(node, users, use_rate)
  ,
  expanded_data,
  by.y = "parti_name", by.x = "node",
  all.x = T, all.y = F
)

mnq09_data_f<-merge(
  merge(
    expanded_data %>% 
      select(parti_name, person_1:person_4) %>% 
      pivot_longer(cols = c(2:5), 
                   names_to = "x",
                   values_to = "edge") %>% 
      drop_na() %>% 
      rename("node"=parti_name)
    ,
    expanded_data %>% 
      select(parti_name, feed_use)
    ,
    by.x = "edge", by.y = "parti_name",
    all.x = T, all.y = F
  ) %>% 
    select(node, x,edge, feed_use) %>%   
    select(-edge) %>% 
    pivot_wider(names_from = x, values_from = feed_use) %>% 
    mutate_if(is.logical, as.numeric) %>% 
    mutate(users=rowSums(across(where(is.numeric)), na.rm=T)) %>% 
    mutate(valid=rowSums(!is.na(.))-2) %>% 
    mutate(use_rate=ifelse(users/valid>=0,users/valid, NA_real_ ))%>% 
    select(node, users, use_rate)
  ,
  expanded_data,
  by.y = "parti_name", by.x = "node",
  all.x = T, all.y = F
)
#replication of his equation - works better with education and size as cont
summary(glm(data = mnq09_data_c %>% 
              mutate(info_recieved_media=case_when(info_recieved_tv==T|info_recieved_radio==T|info_recieved_newspaper==T~T,
                                                   TRUE~F),
                     info_recieved_proj=case_when(info_recieved_ngo_proj==T|info_recieved_gov_proj==T~T,
                                                   TRUE~F),
                     info_recieved_ext=case_when(info_recieved_ext_off==T|info_recieved_gov_ext==T~T,
                                                  TRUE~F)),
            crossbreed_use~
              education_cont+
              farming_years+
              farm_size_cont+
              elec_access+internet_access+#instead of household exp
              crossbreed_aware+#info constraint
              farmer_group+finance_group+social_groups+
              market_distance+
           #additional
             hh_head_gender+con_personal+num_cows+hh_depen+
             info_recieved_media+info_recieved_proj+info_recieved_ext+
             info_recieved_coop+info_recieved_ffs+info_recieved_vol_trainers
            #+info_recieved_gov_ext+info_recieved_ext_off
             #+info_recieved_ngo_proj+info_recieved_gov_proj
              #+count_info_recieved
              +use_rate
            ,
            family = binomial("logit")
))

summary(glm(data = mnq09_data_f %>% 
              mutate(info_recieved_media=case_when(info_recieved_tv==T|info_recieved_radio==T|info_recieved_newspaper==T~T,
                                                   TRUE~F),
                     info_recieved_proj=case_when(info_recieved_ngo_proj==T|info_recieved_gov_proj==T~T,
                                                  TRUE~F),
                     info_recieved_ext=case_when(info_recieved_ext_off==T|info_recieved_gov_ext==T~T,
                                                 TRUE~F)),
            feed_use~
              education_cont+
              farming_years+
              farm_size_cont+
              elec_access+internet_access+#instead of household exp
              feed_aware+#info constraint
              farmer_group+finance_group+social_groups+
              market_distance+
              #additional
              hh_head_gender+con_personal+num_cows+hh_depen+
              info_recieved_media+info_recieved_proj+info_recieved_ext+
              info_recieved_coop+info_recieved_ffs+info_recieved_vol_trainers
            #+info_recieved_gov_ext+info_recieved_ext_off
            #+info_recieved_ngo_proj+info_recieved_gov_proj
            #+count_info_recieved
            +use_rate*feed_aware #in line with varshney statement that those with less knowledge more liable to endogenous effects
            +second_income_employment+second_income_labour #from ndah paper 
            ,
            family = binomial("logit")
))

summary(glm(data = mnq09_data_v %>% 
              mutate(info_recieved_media=case_when(info_recieved_tv==T|info_recieved_radio==T|info_recieved_newspaper==T~T,
                                                   TRUE~F),
                     info_recieved_proj=case_when(info_recieved_ngo_proj==T|info_recieved_gov_proj==T~T,
                                                  TRUE~F),
                     info_recieved_ext=case_when(info_recieved_ext_off==T|info_recieved_gov_ext==T~T,
                                                 TRUE~F)),
            vaccine_use~
              education_cont+
              farming_years+
              farm_size_cont+
              elec_access+internet_access+#instead of household exp
              vaccine_aware+#info constraint
              farmer_group+finance_group+social_groups+
              market_distance+
              #additional
              hh_head_gender+con_personal+num_cows+hh_depen+
              info_recieved_media+info_recieved_proj+info_recieved_ext+
              info_recieved_coop+info_recieved_ffs+info_recieved_vol_trainers
            #+info_recieved_gov_ext+info_recieved_ext_off
            #+info_recieved_ngo_proj+info_recieved_gov_proj
            #+count_info_recieved
            +use_rate
            ,
            family = binomial("logit")
))


#to find out which individuals we have interviewed at least half of their known connections
eligible<-expanded_data %>% 
  select(parti_name, person_1:person_4) %>% 
  mutate_all(as.character) %>% 
  mutate(across(c(person_1:person_4), ~ifelse(str_detect(.x, "p"),1,0))) %>%
  mutate_all(~replace(., is.na(.), 10)) %>% 
  mutate(xx=as.character(person_1+person_2+person_3+person_4)) %>%
  filter(xx%in%c("40","31","22","13","4"#,#100%
                 #"3",#75%
                 #"12",#66%
                 #"21","2"#50%
                 ))
#q would then be how to deal with unknowns- village average, cluster average, or completely ignore 

expanded_data %>% 
  select(parti_name, person_1:person_4) %>% 
  mutate(across(c(person_1:person_4), ~ifelse(str_detect(., "p"),paste(.),NA))) %>% 
  mutate_all(as.character) %>% 
  filter(parti_name%in%eligible$parti_name) %>% 
  pivot_longer(cols = c(1:5), names_to = "c", values_to = "participants") %>% 
  select(2) %>% 
  distinct() %>%
  drop_na() 

#data usage still very high - 94%(241) for when at least 50% of network is known
#sample to run regression on would be 199 (78%)
#214 and 139 when more than 50% known
#202 and 128 when at least 75% known 
#147 and 90 when 100% known

#can run separate regression on each sample?

