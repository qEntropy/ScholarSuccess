setwd("~/Desktop/part2")
library(plyr)
library(dplyr)
library(data.table)
library(readr)

brain_author <- read_csv("brain_author.csv")
brain_pub_authors <- read_csv("brain_publication_authors.csv")
cip_cat <- read_csv("CIP_category.csv")
colnames(cip_cat) <- c("cip_title", "cip_category")

setwd("~/Desktop/stats_project_part_2")

brain_author$scopus_id <- as.factor(brain_author$scopus_id)
brain_pub_authors$scopus_id <- as.factor(brain_pub_authors$scopus_id)
brain_pub_authors$co_author_scopus_id <- as.factor(brain_pub_authors$co_author_scopus_id)


init_node_list <- distinct(inner_join(brain_author, cip_cat, by="cip_title")) %>% 
  filter(num_publications > 10) %>% 
  filter(min_pub_year > 1960) %>% 
  select(scopus_id,region,dept_rank,cip_category,total_deflated_dollar_2010, citations, y_05)


init_edge_list <- inner_join(init_node_list, brain_pub_authors, by="scopus_id") %>% 
  filter(co_author_scopus_id %in% init_node_list$scopus_id) %>% 
  filter(scopus_id %in% init_node_list$scopus_id) %>% 
  filter(scopus_id != co_author_scopus_id)

init_edge_list <- init_edge_list %>% select(scopus_id, cip_category, region, co_author_scopus_id)
init_edge_list <- unique(init_edge_list)



##################################### USA ######################################

usaNode <- unique(init_node_list %>% filter(region == "US/Canada"))
us_auth_cip <- usaNode %>% select (scopus_id, cip_category)

usaEdges <- unique(init_edge_list %>% filter(region == "US/Canada"))
usaEdges$scopus_id <- as.factor(usaEdges$scopus_id)

names(usaEdges)[names(usaEdges) == 'cip_category'] <- 'author_cip'
colnames(us_auth_cip) <- c("co_author_scopus_id", "co_author_cip")

usa_edges_auth_cip <- inner_join(usaEdges, us_auth_cip, by = "co_author_scopus_id")

usa_edges_auth_cip_1 <- usa_edges_auth_cip %>% 
  mutate(cd = if_else(author_cip != co_author_cip, 1, 0))

usa_ed_with_cd <- usa_edges_auth_cip_1 %>% select(scopus_id, cd)
usa_ed_with_cd <- usa_ed_with_cd %>% group_by(scopus_id) %>% slice(which.max(cd))

# stricter definition of cross discp
usa_n_x <-  usa_ed_with_cd %>% 
  group_by(scopus_id) %>% 
  summarise(cross_disc = sum(cd))

usa_n_x_1 <- usa_n_x %>% 
  mutate(cd = if_else(cross_disc > 4, 1, 0))

# uncomment this to find a stricter definition of 
#usa_ed_with_cd <- usa_n_x_1 %>% select(scopus_id, cd)


usa_nodes <- inner_join(usaNode, usa_ed_with_cd, by = "scopus_id")

setDT(usa_nodes, keep.rownames = TRUE)[]
names(usa_nodes)[names(usa_nodes) == 'rn'] <- 'Source'

usaAuthors <- usa_nodes %>% select (Source, scopus_id)
usaAuthors$scopus_id <- as.factor(usaAuthors$scopus_id)

usa_edge_list <- inner_join(usaEdges, usaAuthors, by = "scopus_id") %>% 
  filter(scopus_id %in% usaAuthors$scopus_id)

colnames(usaAuthors) <- c("Target", "co_author_scopus_id")
usa_edge_complete <- inner_join(usa_edge_list, usaAuthors, by = "co_author_scopus_id")
usa_edge_complete$Source <- as.factor(usa_edge_complete$Source)
usa_edge_complete$Target <- as.factor(usa_edge_complete$Target)

names(usa_nodes)[names(usa_nodes) == 'Source'] <- 'Id'

# USA nodes
usa_nodes_complete <- usa_nodes %>% select(Id,
                                           dept_rank,
                                           cip_category, 
                                           total_deflated_dollar_2010,
                                           citations,
                                           y_05,
                                           cd)

names(usa_nodes_complete)[names(usa_nodes_complete) == 'Source'] <- 'Id'
usa_nodes_complete$Id <- as.factor(usa_nodes_complete$Id)
usa_nodes_complete$y_05 <- as.factor(usa_nodes_complete$y_05)
write.csv(usa_nodes_complete,"usa_nodes_complete.csv",row.names = FALSE)

# USA edges
usa_edge_complete <- usa_edge_complete %>% select(Source, Target)
usa_edge_complete$Source <- as.factor(usa_edge_complete$Source)
usa_edge_complete$Target <- as.factor(usa_edge_complete$Target)
write.csv(usa_edge_complete,"usa_edge_complete.csv",row.names = FALSE)




########################### Australasia ###########################

ausNode <- unique(init_node_list %>% filter(region == "Australasia"))
aus_auth_cip <- ausNode %>% select (scopus_id, cip_category)

ausEdges <- unique(init_edge_list %>% filter(region == "Australasia"))
ausEdges$scopus_id <- as.factor(ausEdges$scopus_id)

names(ausEdges)[names(ausEdges) == 'cip_category'] <- 'author_cip'
colnames(aus_auth_cip) <- c("co_author_scopus_id", "co_author_cip")

aus_edges_auth_cip <- inner_join(ausEdges, aus_auth_cip, by = "co_author_scopus_id")

aus_edges_auth_cip_1 <- aus_edges_auth_cip %>% 
  mutate(cd = if_else(author_cip != co_author_cip, 1, 0))

aus_ed_with_cd <- aus_edges_auth_cip_1 %>% select(scopus_id, cd)
aus_ed_with_cd <- aus_ed_with_cd %>% group_by(scopus_id) %>% slice(which.max(cd))

aus_nodes <- inner_join(ausNode, aus_ed_with_cd, by = "scopus_id")

setDT(aus_nodes, keep.rownames = TRUE)[]
names(aus_nodes)[names(aus_nodes) == 'rn'] <- 'Source'

ausAuthors <- aus_nodes %>% select (Source, scopus_id)

aus_edge_list <- inner_join(ausEdges, ausAuthors, by = "scopus_id") %>% 
  filter(scopus_id %in% ausAuthors$scopus_id)

colnames(ausAuthors) <- c("Target", "co_author_scopus_id")
aus_edge_complete <- inner_join(aus_edge_list, ausAuthors, by = "co_author_scopus_id")
aus_edge_complete$Source <- as.factor(aus_edge_complete$Source)
aus_edge_complete$Target <- as.factor(aus_edge_complete$Target)



# AUS nodes
aus_nodes_complete <- aus_nodes %>% select(Source,
                                           cip_category, 
                                           total_deflated_dollar_2010,
                                           citations,
                                           y_05,
                                           cd)
names(aus_nodes_complete)[names(aus_nodes_complete) == 'Source'] <- 'Id'
aus_nodes_complete$Id <- as.factor(aus_nodes_complete$Id)
aus_nodes_complete$y_05 <- as.factor(aus_nodes_complete$y_05)
write.csv(aus_nodes_complete,"aus_nodes_complete.csv",row.names = FALSE)


# AUS edges
aus_edge_complete <- aus_edge_complete %>% select(Source, Target)
aus_edge_complete$Source <- as.factor(aus_edge_complete$Source)
aus_edge_complete$Target <- as.factor(aus_edge_complete$Target)
write.csv(aus_edge_complete,"aus_edge_complete.csv",row.names = FALSE)



##################################### Europe ######################################


eurNode <- unique(init_node_list %>% filter(region == "Europe"))
eur_auth_cip <- eurNode %>% select (scopus_id, cip_category)

eurEdges <- unique(init_edge_list %>% filter(region == "Europe"))
eurEdges$scopus_id <- as.factor(eurEdges$scopus_id)

names(eurEdges)[names(eurEdges) == 'cip_category'] <- 'author_cip'
colnames(eur_auth_cip) <- c("co_author_scopus_id", "co_author_cip")

eur_edges_auth_cip <- inner_join(eurEdges, eur_auth_cip, by = "co_author_scopus_id")

eur_edges_auth_cip_1 <- eur_edges_auth_cip %>% 
  mutate(cd = if_else(author_cip != co_author_cip, 1, 0))

eur_ed_with_cd <- eur_edges_auth_cip_1 %>% select(scopus_id, cd)
eur_ed_with_cd <- eur_ed_with_cd %>% group_by(scopus_id) %>% slice(which.max(cd))

eur_nodes <- inner_join(eurNode, eur_ed_with_cd, by = "scopus_id")

setDT(eur_nodes, keep.rownames = TRUE)[]
names(eur_nodes)[names(eur_nodes) == 'rn'] <- 'Source'

eurAuthors <- eur_nodes %>% select (Source, scopus_id)

eur_edge_list <- inner_join(eurEdges, eurAuthors, by = "scopus_id") %>% 
  filter(scopus_id %in% eurAuthors$scopus_id)

colnames(eurAuthors) <- c("Target", "co_author_scopus_id")
eur_edge_complete <- inner_join(eur_edge_list, eurAuthors, by = "co_author_scopus_id")
eur_edge_complete$Source <- as.factor(eur_edge_complete$Source)
eur_edge_complete$Target <- as.factor(eur_edge_complete$Target)


# Europe nodes
eur_nodes_complete <- eur_nodes %>% select(Source,
                                           cip_category, 
                                           total_deflated_dollar_2010,
                                           citations,
                                           y_05,
                                           cd)
names(eur_nodes_complete)[names(eur_nodes_complete) == 'Source'] <- 'Id'
eur_nodes_complete$Id <- as.factor(eur_nodes_complete$Id)
eur_nodes_complete$y_05 <- as.factor(eur_nodes_complete$y_05)
write.csv(eur_nodes_complete,"eur_nodes_complete.csv",row.names = FALSE)


# Europe edges
eur_edge_complete <- eur_edge_complete %>% select(Source, Target)
eur_edge_complete$Source <- as.factor(eur_edge_complete$Source)
eur_edge_complete$Target <- as.factor(eur_edge_complete$Target)
write.csv(eur_edge_complete,"eur_edge_complete.csv",row.names = FALSE)



