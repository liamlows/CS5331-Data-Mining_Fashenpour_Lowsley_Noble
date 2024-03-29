library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggdendro)
library(seriation)
library (cluster)
library(factoextra)

clust.centroid <- function(i, dat, clusters) {
  ind = (clusters == i)
  colMeans(dat[ind,])
}


# read in Texas data csv
cases_TX <- read.csv("./data/COVID-19_cases_TX.csv")
colnames(cases_TX)
# remove state and state_fips_code features
cases_TX <- subset(cases_TX, select = c("county_fips_code",
                                        "county_name",
                                        "date",
                                        "confirmed_cases",
                                        "deaths"))

# remove statewide allocated values from dataframe
cases_TX <- cases_TX[-with(cases_TX, which(cases_TX$county_name == "Statewide Unallocated", arr.ind = TRUE)),]
row.names(cases_TX) <- NULL
# cast date to date
cases_TX$date <- ymd(cases_TX$date)
# check types
lapply(cases_TX, class)
# check for NA values
lapply(cases_TX, anyNA)
# remove instances up to first recorded case in any county
cases_TX <- subset(cases_TX, date >= "2020-03-05")

# AT THIS POINT NO NA VALUES AND TYPES ARE CORRECT

# -----------------------------------------------------------------------------------

# read in US data w/ census csv
cases_US_census <- read.csv("./Projects/Project\ 1/data/COVID-19_cases_plus_census.csv")
colnames(cases_US_census)
# subset to include chosen data attributes
cases_US_census <- subset(cases_US_census, select = c("county_fips_code",
                                                      "county_name",
                                                      "state",
                                                      "total_pop",
                                                      "male_pop",
                                                      "female_pop",
                                                      "median_age",
                                                      "white_pop",
                                                      "black_pop",
                                                      "asian_pop",
                                                      "hispanic_pop",
                                                      "amerindian_pop",
                                                      "other_race_pop",
                                                      "median_income",
                                                      "income_per_capita",
                                                      "commuters_by_public_transportation",
                                                      "worked_at_home",
                                                      "poverty",
                                                      "gini_index",
                                                      "associates_degree",
                                                      "bachelors_degree",
                                                      "high_school_diploma",
                                                      "high_school_including_ged"))

# subset to only use Texas data
cases_TX_census <- subset(cases_US_census, state == "TX")
# check types
lapply(cases_TX_census, class)
# check NA
lapply(cases_TX_census, anyNA)

summary(cases_TX_census)
# AT THIS POINT NO NA VALUES AND TYPES ARE CORRECT

# cumulative number of cases/deaths at 01/25/21 for each county
dates <- unique(cases_TX$date)
cases_TX_cumulative <- subset(cases_TX, date == dates[length(dates)])
# remove fips code and date
cases_TX_cumulative <- cases_TX_cumulative [-c(1,3)]
# add populations
dataset <- merge(cases_TX_cumulative, cases_TX_census, by="county_name")
# remove state
dataset <- dataset [-c(5)]

# change county name from DeWitt to De Witt to fix map issue
dataset$county_name[dataset$county_name=="DeWitt County" | 
                    dataset$county_name=="Dewitt County" ] <- "De witt County"

# add per 1000 values
dataset <- dataset %>% mutate(
  cases_per_1000 = confirmed_cases/total_pop*1000,
  deaths_per_1000 = deaths/total_pop*1000,
  commuters_public_1000 = commuters_by_public_transportation/total_pop*1000,
  work_home_1000 = worked_at_home/total_pop*1000,
  poverty_1000 = poverty/total_pop*1000,
  white_per_1000 = white_pop/total_pop*1000,
  hispanic_per_1000 = hispanic_pop/total_pop*1000,
  black_per_1000 = black_pop/total_pop*1000,
  asian_per_1000 = asian_pop/total_pop*1000,
  amerindian_per_1000 = amerindian_pop/total_pop*1000,
  other_per_1000 = other_race_pop/total_pop*1000,
  male_per_1000 = male_pop/total_pop*1000,
  female_per_1000 = female_pop/total_pop*1000,
  fatality_rate = deaths/confirmed_cases,
  associates_per_1000 = associates_degree/total_pop*1000,
  bachelors_per_1000 = bachelors_degree/total_pop*1000,
  high_school_per_1000 = high_school_diploma/total_pop*1000,
  ged_per_1000 = high_school_including_ged/total_pop*1000
)

colnames(dataset)

counties <- as_tibble(map_data("county"))
counties_TX <- counties %>% dplyr::filter(region == "texas") %>% rename(c(county = subregion))

dataset <- dataset %>% mutate(county = county_name %>% 
           str_to_lower() %>% str_replace('\\s+county\\s*$', ''))

head(dataset)
summary(dataset)

lapply(dataset, FUN = mean)
lapply(dataset, FUN = median)
lapply(dataset, FUN = Mode)
lapply(dataset, FUN = Freq)
lapply(dataset, FUN = sd)
lapply(dataset, FUN = var)
lapply(dataset, FUN = min)
lapply(dataset, FUN = max)
lapply(dataset, FUN = Rnge)

################################# Cluster 1 #################################

poverty_commuters_age <- dataset %>%
  select(
    poverty_1000,
    commuters_public_1000,
    median_age
  ) %>% scale() %>% as_tibble()

# FIND BEST CLUSTER SIZE
set.seed(1234)
ks <- 2:10

d7 <- dist(poverty_commuters_age)

# finding best cluster number by knee
WSS <- sapply(ks, FUN = function(k) {
  kmeans(poverty_commuters_age, centers = k, nstart = 5)$tot.withinss
})
ggplot(as_tibble(ks, WSS), aes(ks, WSS)) + geom_line()

# finding best cluster number by silhouette width
ASW <- sapply(ks, FUN=function(k) {
  fpc::cluster.stats(d7, kmeans(poverty_commuters_age, centers=k, nstart = 5)$cluster)$avg.silwidth
})
best_k_ASW <- ks[which.max(ASW)]
ggplot(as_tibble(ks, ASW), aes(ks, ASW)) + geom_line() +
  geom_vline(xintercept = best_k_ASW, color = "red", linetype = 2) 

# finding best cluster number by Dunn index
DI <- sapply(ks, FUN=function(k) {
  fpc::cluster.stats(d7, kmeans(poverty_commuters_age, centers=k, nstart=5)$cluster)$dunn
})
best_k_DI <- ks[which.max(DI)]
ggplot(as_tibble(ks, DI), aes(ks, DI)) + geom_line() +
  geom_vline(xintercept = best_k_DI, color = "red", linetype = 2)

# k-means clustering
summary(poverty_commuters_age)

km7 <- kmeans(poverty_commuters_age, centers = 5)
km7

ggplot(pivot_longer(as_tibble(km7$centers,  rownames = "cluster"), cols = colnames(km7$centers)), aes(y = name, x = value)) +
  geom_bar(stat = "identity") +
  facet_grid(rows = vars(cluster))

poverty_commuters_age_cluster <- counties_TX %>%
  left_join(dataset %>% add_column(cluster = factor(km7$cluster)))

ggplot(poverty_commuters_age_cluster, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() + 
  scale_fill_viridis_d() 

poverty_commuters_age_stats <- dataset %>% add_column(cluster = factor(km7$cluster))

poverty_commuters_age_stats %>% group_by(cluster) %>% summarize(
  avg_cases = mean(cases_per_1000), 
  avg_deaths = mean(deaths_per_1000),
  avg_fatality = mean(fatality_rate))

# hierarchical clustering
hc7 <- hclust(d7, method = "complete")
ggdendrogram(hc7, labels = FALSE, theme_dendro = FALSE)
hc7_clusters <- cutree(hc7, k = 5)
hc7_centers <- sapply(unique(hc7_clusters), clust.centroid, poverty_commuters_age, hc7_clusters)

hc7_centers <- t(hc7_centers)
rownames(hc7_centers) <- seq(from=1, to=5, by=1)

ggplot(pivot_longer(as_tibble(hc7_centers,  rownames = "cluster"), cols = colnames(hc7_centers)), aes(y = name, x = value)) +
  geom_bar(stat = "identity") +
  facet_grid(rows = vars(cluster))


poverty_commuters_age_hccluster <- counties_TX %>%
  left_join(dataset %>% add_column(cluster = factor(hc7_clusters)))

rownames(poverty_commuters_age) <- dataset$county_name

factoextra::fviz_cluster(list(data = poverty_commuters_age, cluster = hc7_clusters)) +
  labs(title = "Hierarchical Clusters of Poverty/Commuters Per 1000 and Median Age")

ggplot(poverty_commuters_age_hccluster, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() + 
  scale_fill_viridis_d()

poverty_commuters_age_hccluster %>% group_by(cluster) %>% summarize(
  avg_cases = mean(cases_per_1000), 
  avg_deaths = mean(deaths_per_1000),
  avg_fatality = mean(fatality_rate))

dissplot(d7^2, labels = km7$cluster, options=list(main="k-means with k=5"))


fviz_silhouette(silhouette(km7$cluster, d7 ))

################################# Cluster 2 #################################

cases_deaths_fatality <- dataset %>%
  select(
    cases_per_1000,
    deaths_per_1000,
    fatality_rate
  ) %>% scale() %>% as_tibble()

# FIND BEST CLUSTER SIZE
set.seed(1234)
ks <- 2:10

d2 <- dist(cases_deaths_fatality)

# finding best cluster number by knee
WSS <- sapply(ks, FUN = function(k) {
  kmeans(cases_deaths_fatality, centers = k, nstart = 5)$tot.withinss
})
ggplot(as_tibble(ks, WSS), aes(ks, WSS)) + geom_line()

# finding best cluster number by silhouette width
ASW <- sapply(ks, FUN=function(k) {
  fpc::cluster.stats(d2, kmeans(cases_deaths_fatality, centers=k, nstart = 5)$cluster)$avg.silwidth
})
best_k_ASW <- ks[which.max(ASW)]
ggplot(as_tibble(ks, ASW), aes(ks, ASW)) + geom_line() +
  geom_vline(xintercept = best_k_ASW, color = "red", linetype = 2) 

# finding best cluster number by Dunn index
DI <- sapply(ks, FUN=function(k) {
  fpc::cluster.stats(d2, kmeans(cases_deaths_fatality, centers=k, nstart=5)$cluster)$dunn
})
best_k_DI <- ks[which.max(DI)]
ggplot(as_tibble(ks, DI), aes(ks, DI)) + geom_line() +
  geom_vline(xintercept = best_k_DI, color = "red", linetype = 2) 

# k-means clustering
summary(cases_deaths_fatality)

km2 <- kmeans(cases_deaths_fatality, centers = 7)
km2

ggplot(pivot_longer(as_tibble(km2$centers,  rownames = "cluster"), cols = colnames(km2$centers)), aes(y = name, x = value)) +
  geom_bar(stat = "identity") +
  facet_grid(rows = vars(cluster)) 

cases_deaths_fatality_cluster <- counties_TX %>%
  left_join(dataset %>% add_column(cluster = factor(km2$cluster)))

ggplot(cases_deaths_fatality_cluster, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() + 
  scale_fill_viridis_d()

cases_deaths_fatality_stats <- dataset %>% add_column(cluster = factor(km2$cluster))

cases_deaths_fatality_stats %>% group_by(cluster) %>% summarize(
  avg_poverty = mean(poverty_1000), 
  avg_income = mean(median_income),
  avg_income_capita = mean(income_per_capita))

# hierarchical clustering
hc2 <- hclust(d2, method = "complete")
ggdendrogram(hc2, labels = FALSE, theme_dendro = FALSE)
hc2_clusters <- cutree(hc2, k = 7)
hc2_centers <- sapply(unique(hc2_clusters), clust.centroid, cases_deaths_fatality, hc2_clusters)

hc2_centers <- t(hc2_centers)
rownames(hc2_centers) <- seq(from=1, to=7, by=1)

ggplot(pivot_longer(as_tibble(hc2_centers,  rownames = "cluster"), cols = colnames(hc2_centers)), aes(y = name, x = value)) +
  geom_bar(stat = "identity") +
  facet_grid(rows = vars(cluster))

cases_deaths_fatality_hccluster <- counties_TX %>%
  left_join(dataset %>% add_column(cluster = factor(hc2_clusters)))

rownames(cases_deaths_fatality) <- dataset$county_name

factoextra::fviz_cluster(list(data = cases_deaths_fatality, cluster = hc2_clusters)) +
  labs(title = "Hierarchical Clusters of Cases/Deaths Per 1000 And Fatality Rate")

ggplot(cases_deaths_fatality_hccluster, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() + 
  scale_fill_viridis_d()

cases_deaths_fatality_hccluster %>% group_by(cluster) %>% summarize(
  avg_poverty = mean(poverty_1000), 
  avg_income = mean(median_income),
  avg_income_capita = mean(income_per_capita))

dissplot(d2, labels = km2$cluster, options=list(main="k-means with k=7"))
fviz_silhouette(silhouette(km2$cluster, d2 ))

################################# cluster 3 #################################
race_county <- dataset %>%
  select(
    white_per_1000,
    black_per_1000,
    asian_per_1000,
    hispanic_per_1000,
    amerindian_per_1000,
    other_per_1000
  ) %>% scale() %>% as_tibble()

# FIND BEST CLUSTER SIZE
set.seed(1234)
ks <- 2:10

d6 <- dist(race_county)
str(d6)

# finding best cluster number by knee
WSS <- sapply(ks, FUN = function(k) {
  kmeans(race_county, centers = k, nstart = 5)$tot.withinss
})
ggplot(as_tibble(ks, WSS), aes(ks, WSS)) + geom_line() 

# finding best cluster number by silhouette width
ASW <- sapply(ks, FUN=function(k) {
  fpc::cluster.stats(d6, kmeans(race_county, centers=k, nstart = 5)$cluster)$avg.silwidth
})
best_k_ASW <- ks[which.max(ASW)]
ggplot(as_tibble(ks, ASW), aes(ks, ASW)) + geom_line() +
  geom_vline(xintercept = best_k_ASW, color = "red", linetype = 2) 

# finding best cluster number by Dunn index
DI <- sapply(ks, FUN=function(k) {
  fpc::cluster.stats(d6, kmeans(race_county, centers=k, nstart=5)$cluster)$dunn
})
best_k_DI <- ks[which.max(DI)]
ggplot(as_tibble(ks, DI), aes(ks, DI)) + geom_line() +
  geom_vline(xintercept = best_k_DI, color = "red", linetype = 2) 

# k-means clustering
summary(race_county)

km6 <- kmeans(race_county, centers = 7)
km6

ggplot(pivot_longer(as_tibble(km6$centers,  rownames = "cluster"), cols = colnames(km6$centers)), aes(y = name, x = value)) +
  geom_bar(stat = "identity") +
  facet_grid(rows = vars(cluster)) 

race_county_cluster <- counties_TX %>%
  left_join(dataset %>% add_column(cluster = factor(km6$cluster)))

ggplot(race_county_cluster, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() + 
  scale_fill_viridis_d() 

race_county_stats <- dataset %>% add_column(cluster = factor(km6$cluster))

race_county_stats %>% group_by(cluster) %>% summarize(
  avg_cases = mean(cases_per_1000), 
  avg_deaths = mean(deaths_per_1000),
  avg_fatality = mean(fatality_rate))

# hierarchical clustering
hc6<- hclust(d6, method = "complete")
ggdendrogram(hc6, labels = FALSE, theme_dendro = FALSE)
hc6_clusters <- cutree(hc6, k = 7)
hc6_centers <- sapply(unique(hc6_clusters), clust.centroid, race_county, hc6_clusters)

hc6_centers <- t(hc6_centers)
rownames(hc6_centers) <- seq(from=1, to=7, by=1)
head(hc6_centers)

ggplot(pivot_longer(as_tibble(hc6_centers,  rownames = "cluster"), cols = colnames(hc6_centers)), aes(y = name, x = value)) +
  geom_bar(stat = "identity") +
  facet_grid(rows = vars(cluster)) 

race_county_hccluster <- counties_TX %>%
  left_join(dataset %>% add_column(cluster = factor(hc6_clusters)))

rownames(race_county) <- dataset$county_name

# factoextra::fviz_cluster(list(data = race_county, cluster = hc6_clusters)) +
#   labs(title = "Hierarchical Clusters of Racial Populations Per 1000")

ggplot(race_county_hccluster, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() + 
  scale_fill_viridis_d() 

race_county_hccluster %>% group_by(cluster) %>% summarize(
  avg_cases = mean(cases_per_1000), 
  avg_deaths = mean(deaths_per_1000),
  avg_fatality = mean(fatality_rate))

dissplot(d6, labels = km6$cluster, options=list(main="k-means with k=7"))
fviz_silhouette(silhouette(km6$cluster, d6 ))

################################# cluster 4 #################################

age_poverty_hispanic <- dataset %>%
  select(
    median_age,
    poverty_1000,
    hispanic_per_1000
  ) %>% scale() %>% as_tibble()

# FIND BEST CLUSTER SIZE
set.seed(1234)
ks <- 2:10

# finding best cluster number by knee
WSS <- sapply(ks, FUN = function(k) {
  kmeans(age_poverty_hispanic, centers = k, nstart = 5)$tot.withinss
})
ggplot(as_tibble(ks, WSS), aes(ks, WSS)) + geom_line() 

# finding best cluster number by silhouette width
d3 <- dist(age_poverty_hispanic)
str(d3)
ASW <- sapply(ks, FUN=function(k) {
  fpc::cluster.stats(d3, kmeans(age_poverty_hispanic, centers=k, nstart = 5)$cluster)$avg.silwidth
})
best_k_ASW <- ks[which.max(ASW)]
best_k_ASW

ggplot(as_tibble(ks, ASW), aes(ks, ASW)) + geom_line() +
  geom_vline(xintercept = best_k_ASW, color = "red", linetype = 2)

# finding best cluster number by Dunn index
DI <- sapply(ks, FUN=function(k) {
  fpc::cluster.stats(d3, kmeans(age_poverty_hispanic, centers=k, nstart=5)$cluster)$dunn
})
best_k_DI <- ks[which.max(DI)]
best_k_DI

ggplot(as_tibble(ks, DI), aes(ks, DI)) + geom_line() +
  geom_vline(xintercept = best_k_DI, color = "red", linetype = 2) 

# k-means clustering
summary(age_poverty_hispanic)

km3 <- kmeans(age_poverty_hispanic, centers = 5)
km3

ggplot(pivot_longer(as_tibble(km3$centers,  rownames = "cluster"), cols = colnames(km3$centers)), aes(y = name, x = value)) +
  geom_bar(stat = "identity") +
  facet_grid(rows = vars(cluster)) 

age_poverty_hispanic_cluster <- counties_TX %>%
  left_join(dataset %>% add_column(cluster = factor(km3$cluster)))

ggplot(age_poverty_hispanic_cluster, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() + 
  scale_fill_viridis_d() 

age_poverty_hispanic_stats <- dataset %>% add_column(cluster = factor(km3$cluster))

age_poverty_hispanic_stats %>% group_by(cluster) %>% summarize(
  avg_cases = mean(cases_per_1000), 
  avg_deaths = mean(deaths_per_1000),
  avg_fatality = mean(fatality_rate))

# hierarchical clustering
hc3 <- hclust(d, method = "complete")
ggdendrogram(hc3, labels = FALSE, theme_dendro = FALSE)
hc3_clusters <- cutree(hc3, k = 5)
hc3_centers <- sapply(unique(hc3_clusters), clust.centroid, age_poverty_hispanic, hc3_clusters)

hc3_centers <- t(hc3_centers)
rownames(hc3_centers) <- c(1,2,3,4,5)

ggplot(pivot_longer(as_tibble(hc3_centers,  rownames = "cluster"), cols = colnames(hc3_centers)), aes(y = name, x = value)) +
  geom_bar(stat = "identity") +
  facet_grid(rows = vars(cluster)) 


age_poverty_hispanic_hccluster <- counties_TX %>%
  left_join(dataset %>% add_column(cluster = factor(hc3_clusters)))

rownames(age_poverty_hispanic) <- dataset$county_name

factoextra::fviz_cluster(list(data = age_poverty_hispanic, cluster = hc3_clusters)) 

ggplot(age_poverty_hispanic_hccluster, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() + 
  scale_fill_viridis_d() 

age_poverty_hispanic_hccluster %>% group_by(cluster) %>% summarize(
  avg_cases = mean(cases_per_1000), 
  avg_deaths = mean(deaths_per_1000),
  avg_fatality = mean(fatality_rate))



dissplot(d3, labels = km3$cluster, options=list(main="k-means with k=5"))
fviz_silhouette(silhouette(km3$cluster, d3 ))

################################# cluster 5 #################################

edu_poverty <- dataset %>%
  select(
    poverty_1000,
    associates_degree_1000,
    bachelors_degree_1000,
    high_school_diploma_1000,
    high_school_including_ged_1000
  ) %>% scale() %>% as_tibble()

# finding best cluster number by knee
WSS <- sapply(ks, FUN = function(k) {
  kmeans(edu_poverty, centers = k, nstart = 5)$tot.withinss
})
ggplot(as_tibble(ks, WSS), aes(ks, WSS)) + geom_line() +
  labs(title = "Kmeans Within Sum Squared of Clusters For Education and Poverty")

# finding best cluster number by silhouette width
d <- dist(edu_poverty)
str(d)
ASW <- sapply(ks, FUN=function(k) {
  fpc::cluster.stats(d, kmeans(edu_poverty, centers=k, nstart = 5)$cluster)$avg.silwidth
})
best_k_ASW <- ks[which.max(ASW)]
best_k_ASW

ggplot(as_tibble(ks, ASW), aes(ks, ASW)) + geom_line() +
  geom_vline(xintercept = best_k_ASW, color = "red", linetype = 2) +
  labs(title = "Kmeans Average Silhoutte Width of Clusters For Education and Poverty")

# finding best cluster number by Dunn index
DI <- sapply(ks, FUN=function(k) {
  fpc::cluster.stats(d, kmeans(edu_poverty, centers=k, nstart=5)$cluster)$dunn
})
best_k_DI <- ks[which.max(DI)]
best_k_DI

ggplot(as_tibble(ks, DI), aes(ks, DI)) + geom_line() +
  geom_vline(xintercept = best_k_DI, color = "red", linetype = 2) +
  labs(title = "Kmeans Dunn Index of Clusters For CEducation and Poverty")

summary(edu_poverty)

km5 <- kmeans(edu_poverty, centers = 7)
km5

ggplot(pivot_longer(as_tibble(km5$centers,  rownames = "cluster"), cols = colnames(km5$centers)), aes(y = name, x = value)) +
  geom_bar(stat = "identity") +
  facet_grid(rows = vars(cluster))

edu_poverty_cluster <- counties_TX %>%
  left_join(dataset %>% add_column(cluster = factor(km5$cluster)))

ggplot(edu_poverty_cluster, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() + 
  scale_fill_viridis_d() + 
  labs(title = "Clusters of Education and Poverty")

edu_stats <- dataset %>% add_column(cluster = factor(km5$cluster))

edu_stats %>% group_by(cluster) %>% summarize(
  avg_cases = mean(cases_per_1000), 
  avg_deaths = mean(deaths_per_1000),
  avg_fatality = mean(fatality_rate))

# hierarchical clustering
hc5 <- hclust(d, method = "complete")
ggdendrogram(hc5, labels = FALSE, theme_dendro = FALSE)
hc5_clusters <- cutree(hc5, k = 7)
hc5_centers <- sapply(unique(hc5_clusters), clust.centroid, edu_poverty, hc5_clusters)

hc5_centers <- t(hc5_centers)
rownames(hc5_centers) <- seq(from=1, to=7, by=1)

ggplot(pivot_longer(as_tibble(hc5_centers,  rownames = "cluster"), cols = colnames(hc5_centers)), aes(y = name, x = value)) +
  geom_bar(stat = "identity") +
  facet_grid(rows = vars(cluster)) +
  labs(title = "Hierarchical Cluster Centers Summary For Education and Poverty")


edu_poverty_hccluster <- counties_TX %>%
  left_join(dataset %>% add_column(cluster = factor(hc5_clusters)))

rownames(edu_poverty) <- dataset$county_name

factoextra::fviz_cluster(list(data = edu_poverty, cluster = hc5_clusters)) +
  labs(title = "Hierarchical Clusters of Education and Poverty")

ggplot(edu_poverty_hccluster, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() + 
  scale_fill_viridis_d() + 
  labs(title = "Hierarchical Clusters of Education and Poverty")

edu_poverty_hccluster %>% group_by(cluster) %>% summarize(
  avg_cases = mean(cases_per_1000), 
  avg_deaths = mean(deaths_per_1000),
  avg_fatality = mean(fatality_rate))

library(factoextra)
library(seriation)
library(cluster)
pimage(d, order=order(km5$cluster))
dissplot(d, labels = km5$cluster, options=list(main="K-means with k=7"))
fviz_silhouette(silhouette(km5$cluster, d))

################################ extra cluster ################################

public_commuters_cases_deaths <- dataset %>%
  select(
    commuters_by_public_transportation,
    deaths,
    confirmed_cases
  ) %>% scale() %>% as_tibble()

# FIND BEST CLUSTER SIZE
set.seed(1234)
ks <- 2:10

# finding best cluster number by knee
WSS <- sapply(ks, FUN = function(k) {
  kmeans(public_commuters_cases_deaths, centers = k, nstart = 5)$tot.withinss
})
ggplot(as_tibble(ks, WSS), aes(ks, WSS)) + geom_line() +
  labs(title = "Kmeans Within Sum Squared of Clusters For Commuters By Public Transportation and Case/Death Counts")

# finding best cluster number by silhouette width
d <- dist(public_commuters_cases_deaths)
str(d)
ASW <- sapply(ks, FUN=function(k) {
  fpc::cluster.stats(d, kmeans(public_commuters_cases_deaths, centers=k, nstart = 5)$cluster)$avg.silwidth
})
best_k_ASW <- ks[which.max(ASW)]
best_k_ASW

ggplot(as_tibble(ks, ASW), aes(ks, ASW)) + geom_line() +
  geom_vline(xintercept = best_k_ASW, color = "red", linetype = 2) +
  labs(title = "Kmeans Average Silhoutte Width of Clusters For Commuters By Public Transportation and Case/Death Counts")

# finding best cluster number by Dunn index
DI <- sapply(ks, FUN=function(k) {
  fpc::cluster.stats(d, kmeans(public_commuters_cases_deaths, centers=k, nstart=5)$cluster)$dunn
})
best_k_DI <- ks[which.max(DI)]
best_k_DI

ggplot(as_tibble(ks, DI), aes(ks, DI)) + geom_line() +
  geom_vline(xintercept = best_k_DI, color = "red", linetype = 2) +
  labs(title = "Kmeans Dunn Index of Clusters For Commuters By Public Transportation and Case/Death Counts")

# k-means clustering
summary(public_commuters_cases_deaths)

km1 <- kmeans(public_commuters_cases_deaths, centers = 3)
km1

ggplot(pivot_longer(as_tibble(km1$centers,  rownames = "cluster"), cols = colnames(km1$centers)), aes(y = name, x = value)) +
  geom_bar(stat = "identity") +
  facet_grid(rows = vars(cluster)) +
  labs(title = "Kmeans Cluster Centers Summary For Commuters By Public Transportation and Case/Death Counts")

public_commuters_cases_deaths_cluster <- counties_TX %>%
  left_join(dataset %>% add_column(cluster = factor(km1$cluster)))

ggplot(public_commuters_cases_deaths_cluster, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() + 
  scale_fill_viridis_d() + 
  labs(title = "Kmeans Clusters of Commuters By Public Transportation and Case/Death Counts")

# hierarchical clustering
hc1 <- hclust(d, method = "complete")
ggdendrogram(hc1, labels = FALSE, theme_dendro = FALSE)
clusters <- cutree(hc1, k = 4)

clust.centroid = function(i, dat, clusters) {
  ind = (clusters == i)
  colMeans(dat[ind,])
}
centers <- sapply(unique(clusters), clust.centroid, public_commuters_cases_deaths, clusters)

centers_inv <- t(centers)
rownames(centers_inv) <- c(1,2,3,4)

ggplot(pivot_longer(as_tibble(centers_inv,  rownames = "cluster"), cols = colnames(centers_inv)), aes(y = name, x = value)) +
  geom_bar(stat = "identity") +
  facet_grid(rows = vars(cluster)) +
  labs(title = "Hierarchical Cluster Centers Summary For Commuters By Public Transportation and Case/Death Counts")


public_commuters_cases_deaths_hccluster <- counties_TX %>%
  left_join(dataset %>% add_column(cluster = factor(clusters)))

rownames(public_commuters_cases_deaths) <- dataset$county_name

factoextra::fviz_cluster(list(data = public_commuters_cases_deaths, cluster = clusters)) +
  labs(title = "Hierarchical Clusters of Commuters By Public Transportation and Case/Death Counts")

ggplot(public_commuters_cases_deaths_hccluster, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() + 
  scale_fill_viridis_d() + 
  labs(title = "Hierarchical Clusters of Commuters By Public Transportation and Case/Death Counts")
