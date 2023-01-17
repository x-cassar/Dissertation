#replication of his equation - works better with education and size as cont
summary(glm(data = expanded_data,
            crossbreed_use~
              education_cont+farming_years+farm_size_cont+crossbreed_aware+farmer_group+finance_group+social_groups+
              market_distance,
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

