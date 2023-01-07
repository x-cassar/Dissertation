oyugis <- read.csv("oyugis_regression.csv") %>% select(1:16, "betweenness", "connections")
rongo <- read.csv("rongo_regression.csv")%>% select(1:16, "betweenness", "connections")
vihiga <- read.csv("vihiga_regression.csv")%>%select(1:16, "betweenness", "connections")
siaya <- read.csv("siaya_regression.csv")%>% select(1:16, "betweenness", "connections")
all_networks <- rbind(oyugis, rongo, vihiga, siaya)
all_networks$sell <- (as.numeric(all_networks$sell_market)+as.numeric(all_networks$sell_coop))


summary(lm(data=all_networks, vaccine_use ~ parti_gender+
             age+
             farming_years+
             education+
             hh_depen+
             perc_dairy+
             num_cows+
             farm_size+
             price_market+ 
             sell+
             betweenness))

summary(lm(data=all_networks, vaccine_use ~ parti_gender+
             age+
             farming_years+
             education+
             hh_depen+
             perc_dairy+
             num_cows+
             farm_size+
             price_market+ 
             sell+
             connections))


summary(lm(data=all_networks, feed_used ~ parti_gender+
             age+
             farming_years+
             education+
             hh_depen+
             perc_dairy+
             num_cows+
             farm_size+
             price_market+ 
             sell+ 
             betweenness))

summary(lm(data=all_networks, feed_used ~ parti_gender+
             age+
             farming_years+
             education+
             hh_depen+
             perc_dairy+
             num_cows+
             farm_size+
             price_market+ 
             sell+ 
             connections))


summary(lm(data=all_networks, perc_crossbreed ~ parti_gender+
             age+
             farming_years+
             education+
             hh_depen+
             perc_dairy+
             num_cows+
             farm_size+
             price_market+ 
             sell+
             betweenness))

summary(lm(data=all_networks, perc_crossbreed ~ parti_gender+
             age+
             farming_years+
             education+
             hh_depen+
             perc_dairy+
             num_cows+
             farm_size+
             price_market+ 
             sell+
             connections))

summary(lm(data=all_networks, betweenness ~ parti_gender+
             age+
             farming_years+
             education+
             hh_depen+
             perc_dairy+
             num_cows+
             farm_size+
             price_market+ 
             sell+
             vaccine_use+
             feed_used+
             perc_crossbreed))

summary(lm(data=all_networks, connections ~ parti_gender+
             age+
             farming_years+
             education+
             hh_depen+
             perc_dairy+
             num_cows+
             farm_size+
             price_market+ 
             sell+
             vaccine_use+
             feed_used+
             perc_crossbreed))

all_networks %>% 
  ggplot(aes(y=sell, fill=as.factor(perc_crossbreed)))+
  geom_boxplot()+
  facet_wrap( ~ as.factor(perc_crossbreed))+
  ylab("Percentage milk sold")+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  ggtitle("Distribution of % milk sold for farmers 
          not using and using improved breeds")+
  theme(legend.position = 0)
