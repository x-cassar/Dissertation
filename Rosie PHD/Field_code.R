library(igraph)
library(dplyr)
library(tidyverse)
# 
# test_data <- read_csv("test_data.csv")
# 
# network_map <- test_data %>%
#   select(7,238,234,230,226)
# 
# column_names <- c("V7", "V238", "V234", "V230", "V226")
# colnames(network_map)<-column_names
# 
# edges1 <- network_map %>%
#   select(V7, V238) %>%
#   rename(nodes = V7, edges = V238)
# 
# edges1 <- edges1[-c(1),]
# 
# edges2<- network_map %>%
#   select(V7, V234) %>%
#   rename(nodes = V7, edges = V234)
# 
# edges2 <- edges2[-c(1),]
# 
# 
# edges3<- network_map %>%
#   select(V7, V230) %>%
#   rename(nodes = V7, edges = V230)
# 
# edges3 <- edges3[-c(1),]
# 
# 
# edges4<- network_map %>%
#   select(V7, V226) %>%
#   rename(nodes = V7, edges = V226)
# 
# edges4 <- edges4[-c(1),]
# 
# edges_final <- rbind(edges1, edges2, edges3, edges4) %>%
#   rename(person = nodes)
# 
# colnames(edges_final)<-c("nodes","edges")
# 
# nodes_1 <- edges_final %>%
#   select(nodes)
# nodes_2<- edges_final %>%
#   select(edges) %>%
#   rename(nodes = edges)
# 
# nodes_final <- rbind(nodes_1, nodes_2)
# nodes_final <- nodes_final %>%  distinct()
# 
# 
# g1 <- graph_from_data_frame(d=edges_final, vertices = nodes_final)
# 
# plot.igraph(g1, edge.arrow.size=0.1, vertex.label.dist=0.16, vertex.label.color=c("black"), label.cex=0.03, margin = -0.12, aso=0.95)

#actual####
# HH_ID <- read.csv("HH_ID.csv")
# HH_ID <- read_csv("Household ID data.csv")
library(readxl)

HH_ID <- read_excel("Rongo Field Spreadsheet.xlsx")

network_map <- HH_ID %>% 
  select(1,5,10,15,20)

column_names <- c("V7", "V238", "V234", "V230", "V226")
colnames(network_map)<-column_names

edges1 <- network_map %>% 
  select(V7, V238) %>% 
  rename(nodes = V7, edges = V238)


edges2<- network_map %>% 
  select(V7, V234) %>% 
  rename(nodes = V7, edges = V234)


edges3<- network_map %>% 
  select(V7, V230) %>% 
  rename(nodes = V7, edges = V230)



edges4<- network_map %>% 
  select(V7, V226) %>% 
  rename(nodes = V7, edges = V226)


edges_final <- rbind(edges1, edges2, edges3, edges4) %>% 
  rename(person = nodes)

edges_final <- na.omit(edges_final)

colnames(edges_final)<-c("nodes","edges")

nodes_1 <- edges_final %>% 
  select(nodes) 
nodes_2<- edges_final %>% 
  select(edges) %>% 
  rename(nodes = edges)

nodes_final <- rbind(nodes_1, nodes_2)
nodes_final <- nodes_final %>%  distinct()
nodes_final <- na.omit(nodes_final)

surveyed <- HH_ID %>% select(1:4) %>% filter(surveyor!=is.na(surveyor)) %>% select(1)
nodes_final <- nodes_final %>% 
  mutate(type = nodes_final$nodes%in%surveyed$participant_name)


g1 <- graph_from_data_frame(d=edges_final, vertices = nodes_final)

V(g1)$color <- ifelse(V(g1)$type, "red", "green")

plot.igraph(g1, edge.arrow.size=0.1, vertex.label.dist=0.16, vertex.label.color=c("black"), label.cex=0.03, margin = -0.12, aso=0.95)


