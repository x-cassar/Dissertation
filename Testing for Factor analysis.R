EFAtools::KMO(expanded_data %>% select(age,hh_depen,farming_years, perc_dairy,num_cows,amount_milk,con_personal,
                         market_distance,price_market,
                         social_capital,in_degree) %>% drop_na() %>% 
                mutate_all(standardize) %>% 
                filter(amount_milk<5,
                       market_distance<5,
                       num_cows<5,
                       in_degree<4) %>% 
                select(-price_market))


test <- prcomp(expanded_data %>% select(age,hh_depen,farming_years, perc_dairy,num_cows,amount_milk,con_personal,
                                        market_distance,price_market,
                                        social_capital,in_degree) %>% drop_na() %>% 
                 mutate_all(standardize) %>% 
                 filter(amount_milk<5,
                        market_distance<5,
                        num_cows<5,
                        in_degree<4) %>% 
                 select(-price_market), scal =T)
test$rotation<--1*test$rotation
test$x <- -1*test$x

biplot(test, scale = 0)

var_explained = test$sdev^2/sum(test$sdev^2)
qplot(c(1:10), var_explained)+
  geom_line()

sum(var_explained[1:6])
test$sdev


expanded_data %>% select(age,hh_depen,farming_years, perc_dairy,num_cows,amount_milk,con_personal,
                         market_distance,price_market,
                         social_capital,in_degree) %>% drop_na() %>% 
  mutate_all(standardize) %>% 
  pivot_longer(cols = c(age:in_degree), names_to = "var", values_to = "val") %>% 
  ggplot()+
  aes(y=val) +
  geom_boxplot()+
  facet_wrap(vars(var), scales = "free_y")
