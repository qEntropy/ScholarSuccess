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


init_node_list_1 <- distinct(inner_join(brain_author, cip_cat, by="cip_title")) %>% 
  filter(num_publications > 10) %>% 
  filter(min_pub_year > 1960) %>% 
  filter(region != "RESTOFWORLD") %>%
  select(scopus_id,region,dept_rank,cip_category,total_deflated_dollar_2010, citations, y_05)

init_node_list_1 <- init_node_list_1 %>% drop_na(region)
scopus_to_cip_map <- init_node_list_1 %>% select(scopus_id, cip_category)

init_edge_list_1 <- inner_join(init_node_list_1, brain_pub_authors, by="scopus_id") %>% 
  filter(co_author_scopus_id %in% init_node_list$scopus_id) %>% 
  filter(scopus_id %in% init_node_list$scopus_id) %>% 
  filter(scopus_id != co_author_scopus_id)

init_edge_list_1 <- init_edge_list_1 %>%select(scopus_id, cip_category, eids, authors, co_author_scopus_id)
names(init_edge_list_1)[names(init_edge_list_1) == 'cip_category'] <- 'author_cip'
colnames(scopus_to_cip_map) <- c("co_author_scopus_id", "co_author_cip")

auth_co_auth_cips <- inner_join(init_edge_list_1, scopus_to_cip_map, by = "co_author_scopus_id")

auth_co_auth_cips <- auth_co_auth_cips %>% 
  mutate(cd = if_else(author_cip != co_author_cip, 1, 0))

cross_disc_total <-  auth_co_auth_cips %>% 
  group_by(scopus_id) %>%
  summarise(cross_disc = sum(cd))

strict_cross_disc <-  cross_disc_total %>% 
  mutate(cd = if_else(cross_disc > 3, 1, 0))


strict_cross_disc <- strict_cross_disc %>% select(scopus_id, cd)

strict_cross_disc_1 <- inner_join(strict_cross_disc, init_node_list_1, by = "scopus_id")

write.csv(strict_cross_disc_1, "stricter_cross_disc.csv", row.names = F)
