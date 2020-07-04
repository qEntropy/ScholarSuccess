library(readr)
library(dplyr)
library(data.table)
library(tidyr)
library(ggplot2)

us_l_m_pagerank <- read_csv("USA_LINEAR_MODEL.csv")
us_required <- us_l_m_pagerank %>% select(Id,
                                          citations,
                                          cd,
                                          pageranks,
                                          dept_rank,
                                          cip_category,
                                          total_deflated_dollar_2010,
                                          y_05)
                                           
us_required$cip_category <- as.factor(us_required$cip_category)
us_required$y_05 <- as.factor(us_required$y_05)
us_required$cd <- as.factor(us_required$cd)

us_required$total_deflated_dollar_2010[is.na(us_required$total_deflated_dollar_2010)] <- 0
us_required <- us_required[complete.cases(us_required), ]

department <- us_required$cip_category
department <- as.factor(department)
department <- relevel(department, ref = "Technology")

y_05_age <- us_required$y_05
y_05_age <- as.factor(y_05_age)
y_05_age <- relevel(y_05_age, ref = "2015")

cross_disciplinarity <- us_required$cd
cross_disciplinarity <- as.factor(cross_disciplinarity)
cross_disciplinarity <- relevel(cross_disciplinarity, ref = "0")


lmmodel <- lm(log(citations+1) ~ factor(cross_disciplinarity) +
                log(total_deflated_dollar_2010+1) +  
                log(pageranks+1) + 
                factor(department) + 
                factor(y_05_age), 
              data = us_required)
summary(lmmodel)

plot(lmmodel)



# stricter cd linear model
strict_cd_data <- read_csv("stricter_cross_disc.csv")
strict_cd_usa <- strict_cd_data %>% filter(region == "US/Canada")
strict_cd_usa <- strict_cd_usa %>% select(scopus_id, cd)
colnames(strict_cd_usa) <- c("scopus_id", "strict_cd")
strict_cd_usa$scopus_id <- as.factor(strict_cd_usa$scopus_id)


# assuming usaAuthors is in globalEnv
colnames(usaAuthors) <- c("Id", "scopus_id")
id_to_cd_map <- inner_join(strict_cd_usa, usaAuthors, by = "scopus_id")
id_to_cd_map$Id <- as.factor(id_to_cd_map$Id)
us_required$Id <- as.factor(us_required$Id)
new_linear_with_strict <- inner_join(us_required, id_to_cd_map, by = "Id")
strict_model_data <- new_linear_with_strict %>% select(citations,
                                                       strict_cd,
                                                       pageranks,
                                                       dept_rank,
                                                       cip_category,
                                                       total_deflated_dollar_2010,
                                                       y_05)


strict_model_data$total_deflated_dollar_2010[is.na(strict_model_data$total_deflated_dollar_2010)] <- 0
strict_model_data <- strict_model_data[complete.cases(strict_model_data), ]

department <- strict_model_data$cip_category
department <- as.factor(department)
department <- relevel(department, ref = "Technology")

y_05_age <- strict_model_data$y_05
y_05_age <- as.factor(y_05_age)
y_05_age <- relevel(y_05_age, ref = "2015")

cross_disciplinarity <- strict_model_data$strict_cd
cross_disciplinarity <- as.factor(cross_disciplinarity)
cross_disciplinarity <- relevel(cross_disciplinarity, ref = "0")


lmmodel <- lm(log(citations+1) ~ factor(cross_disciplinarity) +
                dept_rank+
                log(total_deflated_dollar_2010+1) +  
                log(pageranks+1) + 
                factor(department) + 
                factor(y_05_age), 
              data = strict_model_data)
summary(lmmodel)
dwplot(lmmodel, conf.level = .80)

year <- c(1960, 1965, 1970, 1975, 1980, 1985, 1990, 1995)
est <- c(3.434, 3.351, 3.236, 3.023, 2.790, 2.721, 2.380, 2.031 )
plot(year, est)
dat <- data.frame(year, est)
ggplot(dat, aes(x=year, y=est)) +
  geom_point() +
  xlab( expression(paste("First year of publication (", y["i,5"]^0, ")"))) +
  ylab("Regression Coefficient") +
  ggtitle("US/Canada Regression Coefficient")
  









#######   EUROPE   ######

eur_l_m_pagerank <- read_csv("EUR_LINEAR_MODEL.csv")
eur_required <- eur_l_m_pagerank %>% select(Id,
                                          citations,
                                          cd,
                                          pageranks,
                                          cip_category,
                                          total_deflated_dollar_2010,
                                          y_05)

eur_required$cip_category <- as.factor(eur_required$cip_category)
eur_required$y_05 <- as.factor(eur_required$y_05)
eur_required$cd <- as.factor(eur_required$cd)

eur_required$total_deflated_dollar_2010[is.na(eur_required$total_deflated_dollar_2010)] <- 0

eur_required <- eur_required %>% mutate(cip_category = relevel(cip_category, ref = "Technology"))
eur_required <- eur_required %>% mutate(y_05 = relevel(y_05, ref = "2015"))
eur_required <- eur_required %>% mutate(cd = relevel(cd, ref = "0"))

# less stricter
lmmodel_eur <- lm(log(citations+1) ~ factor(cd) +
                log(total_deflated_dollar_2010+1) +  
                log(pageranks+1) + 
                factor(cip_category) + 
                factor(y_05), 
              data = eur_required)
summary(lmmodel_eur)
plot(lmmodel_eur)



# stricter cd linear model
strict_cd_data <- read_csv("stricter_cross_disc.csv")
strict_cd_eur <- strict_cd_data %>% filter(region == "Europe")
strict_cd_eur <- strict_cd_eur %>% select(scopus_id, cd)
colnames(strict_cd_eur) <- c("scopus_id", "strict_cd")
strict_cd_eur$scopus_id <- as.factor(strict_cd_eur$scopus_id)


# assuming eurAuthors is in globalEnv
colnames(eurAuthors) <- c("Id", "scopus_id")
id_to_cd_map <- inner_join(strict_cd_eur, eurAuthors, by = "scopus_id")
id_to_cd_map$Id <- as.factor(id_to_cd_map$Id)
eur_required$Id <- as.factor(eur_required$Id)
new_linear_with_strict <- inner_join(eur_required, id_to_cd_map, by = "Id")
strict_model_data <- new_linear_with_strict %>% select(citations,
                                                       strict_cd,
                                                       pageranks,
                                                       cip_category,
                                                       total_deflated_dollar_2010,
                                                       y_05)


strict_model_data$total_deflated_dollar_2010[is.na(strict_model_data$total_deflated_dollar_2010)] <- 0
strict_model_data$strict_cd <- as.factor(strict_model_data$strict_cd)

strict_model_data <- strict_model_data %>% mutate(cip_category = relevel(cip_category, ref = "Technology"))
strict_model_data <- strict_model_data %>% mutate(y_05 = relevel(y_05, ref = "2015"))
strict_model_data <- strict_model_data %>% mutate(strict_cd = relevel(strict_cd, ref = "0"))


lmmodel <- lm(log(citations+1) ~ factor(strict_cd) +
                log(total_deflated_dollar_2010+1) +  
                log(pageranks+1) + 
                factor(cip_category) + 
                factor(y_05), 
              data = strict_model_data)
summary(lmmodel)
dwplot(lmmodel, conf.level = .95)

year <- c(1960, 1965, 1970, 1975, 1980, 1985, 1990, 1995, 2000, 2005, 2010)
est <- c(5.023991, 4.816302, 4.854092, 4.681855, 4.589514, 4.330084, 4.021388, 3.787628, 3.340317, 2.675564, 1.941127)
plot(year, est)
dat <- data.frame(year, est)
ggplot(dat, aes(x=year, y=est)) +
  geom_point() +
  xlab( expression(paste("First year of publication (", y["i,5"]^0, ")"))) +
  ylab("Regression Coefficient") +
  ggtitle("Europe Regression Coefficient")






############## Australiasia ###########

aus_l_m_pagerank <- read_csv("AUS_LINEAR_MODEL.csv")
aus_required <- aus_l_m_pagerank %>% select(Id,
                                            citations,
                                            cd,
                                            pageranks,
                                            cip_category,
                                            total_deflated_dollar_2010,
                                            y_05)

aus_required$cip_category <- as.factor(aus_required$cip_category)
aus_required$y_05 <- as.factor(aus_required$y_05)
aus_required$cd <- as.factor(aus_required$cd)


aus_required <- aus_required %>% mutate(cip_category = relevel(cip_category, ref = "Technology"))
aus_required <- aus_required %>% mutate(y_05 = relevel(y_05, ref = "2015"))
aus_required <- aus_required %>% mutate(cd = relevel(cd, ref = "0"))

# less stricter
lmmodel_aus <- lm(log(citations+1) ~ factor(cd) +
                    log(pageranks+1) + 
                    factor(cip_category) + 
                    factor(y_05), 
                  data = eur_required)
summary(lmmodel_aus)
plot(lmmodel_aus)



# stricter cd linear model
strict_cd_data <- read_csv("stricter_cross_disc.csv")
strict_cd_aus <- strict_cd_data %>% filter(region == "Australasia")
strict_cd_aus <- strict_cd_aus %>% select(scopus_id, cd)
colnames(strict_cd_aus) <- c("scopus_id", "strict_cd")
strict_cd_aus$scopus_id <- as.factor(strict_cd_aus$scopus_id)


# assuming eurAuthors is in globalEnv
colnames(ausAuthors) <- c("Id", "scopus_id")
id_to_cd_map <- inner_join(strict_cd_aus, ausAuthors, by = "scopus_id")
id_to_cd_map$Id <- as.factor(id_to_cd_map$Id)
aus_required$Id <- as.factor(aus_required$Id)
new_linear_with_strict <- inner_join(aus_required, id_to_cd_map, by = "Id")
strict_model_data <- new_linear_with_strict %>% select(citations,
                                                       strict_cd,
                                                       pageranks,
                                                       cip_category,
                                                       y_05)

strict_model_data$strict_cd <- as.factor(strict_model_data$strict_cd)

strict_model_data <- strict_model_data %>% mutate(cip_category = relevel(cip_category, ref = "Technology"))
strict_model_data <- strict_model_data %>% mutate(y_05 = relevel(y_05, ref = "2015"))
strict_model_data <- strict_model_data %>% mutate(strict_cd = relevel(strict_cd, ref = "0"))


lmmodel <- lm(log(citations+1) ~ factor(strict_cd) +
                log(pageranks+1) + 
                factor(cip_category) + 
                factor(y_05), 
              data = strict_model_data)
summary(lmmodel)
dwplot(lmmodel, conf.level = .95)

year <- c(1960, 1965, 1970, 1975, 1980, 1985, 1990, 1995, 2000, 2005, 2010)
est <- c(5.11996, 4.59106, 4.07571, 3.99312, 3.80360, 3.72072, 3.29626, 2.96964, 2.33217, 1.77932, 1.29580)
plot(year, est)
dat <- data.frame(year, est)
ggplot(dat, aes(x=year, y=est)) +
  geom_point() +
  xlab( expression(paste("First year of publication (", y["i,5"]^0, ")"))) +
  ylab("Regression Coefficient") +
  ggtitle("Austrlasia Regression Coefficient")