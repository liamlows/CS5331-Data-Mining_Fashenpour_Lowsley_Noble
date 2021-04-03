library(tidyverse)
library(lubridate)
library(ggplot2)
library(ggdendro)

# read in texas data csv
cases_TX <- read.csv("./Projects/Project\ 2/data/COVID-19_cases_TX.csv")
colnames(cases_TX)
head(cases_TX, 20)
# remove state and state_fips_code features
cases_TX <- subset(cases_TX, select = c("county_fips_code",
                                        "county_name",
                                        "date",
                                        "confirmed_cases",
                                        "deaths"))

# remove statewide unallocated values from dataframe
cases_TX <- cases_TX[-with(cases_TX, which(cases_TX$county_name == "Statewide Unallocated", arr.ind = TRUE)),]
row.names(cases_TX) <- NULL
cases_TX
# cast date to date
cases_TX$date <- ymd(cases_TX$date)
# check types
lapply(cases_TX, class)
# check for NA values
lapply(cases_TX, anyNA)
# remove instances up to first recorded case in any county
cases_TX <- subset(cases_TX, date >= "2020-03-05")

# AT THIS POINT NO NA VALUES AND TYPES ARE CORRECT

# STAT DATA

lapply(cases_TX, FUN = mean)
lapply(cases_TX, FUN = median)
lapply(cases_TX, FUN = Mode)
lapply(cases_TX, FUN = Freq)
lapply(cases_TX, FUN = sd)
lapply(cases_TX, FUN = var)
lapply(cases_TX, FUN = min)
lapply(cases_TX, FUN = max)
lapply(cases_TX, FUN = Rnge)


# -----------------------------------------------------------------------------------

# read in US data w/ census csv
cases_US_census <- read.csv("./Projects/Project\ 2/data/COVID-19_cases_plus_census.csv")
colnames(cases_US_census)
head(cases_US_census, 20)
# subset to include chosen data attributes
cases_US_census <- subset(cases_US_census, select = c("county_fips_code",
                                                      "county_name",
                                                      "state",
                                                      "total_pop",
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
                                                      "poverty"))

# subset to only use Texas data
cases_TX_census <- subset(cases_US_census, state == "TX")
head(cases_TX_census)
# check types
lapply(cases_TX_census, class)
# check NA
lapply(cases_TX_census, anyNA)

summary(cases_TX_census)

# AT THIS POINT NO NA VALUES AND TYPES ARE CORRECT

# STATS

lapply(cases_TX_census, FUN = mean)
lapply(cases_TX_census, FUN = median)
lapply(cases_TX_census, FUN = Mode)
lapply(cases_TX_census, FUN = Freq)
lapply(cases_TX_census, FUN = sd)
lapply(cases_TX_census, FUN = var)
lapply(cases_TX_census, FUN = min)
lapply(cases_TX_census, FUN = max)
lapply(cases_TX_census, FUN = Rnge)

# cumulative number of cases/deaths at 01/25/21 for each county
dates <- unique(cases_TX$date)
cases_TX_cumulative <- subset(cases_TX, date == dates[length(dates)])
# remove fips code and date
cases_TX_cumulative <- cases_TX_cumulative [-c(1,3)]
# add populations
dataset <- merge(cases_TX_cumulative, cases_TX_census, by="county_name")
# remove state
dataset <- dataset [-c(5)]
head(dataset)

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
  fatality_rate = deaths/confirmed_cases,
)


# add cases per 1000 and deaths per 1000 columns
#cases_per_1000 <- (dataset$confirmed_cases/dataset$total_pop) * 1000
#dataset <- cbind(dataset, cases_per_1000)
#deaths_per_1000 <- (dataset$deaths/dataset$total_pop) * 1000
#dataset <- cbind(dataset, deaths_per_1000)
#white_per_1000 <- (dataset$white_pop/dataset$total_pop) * 1000
#dataset <- cbind(dataset, white_per_1000)
#hispanic_per_1000 <- (dataset$hispanic_pop/dataset$total_pop) * 1000
#dataset <- cbind(dataset, hispanic_per_1000)
#black_per_1000 <- (dataset$black_pop/dataset$total_pop) * 1000
#dataset <- cbind(dataset, black_per_1000)
#asian_per_1000 <- (dataset$asian_pop/dataset$total_pop) * 1000
#dataset <- cbind(dataset, asian_per_1000)
#amerindian_per_1000 <- (dataset$amerindian_pop/dataset$total_pop) * 1000
#dataset <- cbind(dataset, amerindian_per_1000)
#other_per_1000 <- (dataset$other_race_pop/dataset$total_pop) * 1000
#dataset <- cbind(dataset, other_per_1000)
head(dataset)
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

public_commuters_cases_deaths <- dataset %>%
  select(
    commuters_by_public_transportation,
    deaths,
    confirmed_cases
  ) %>%
  scale() %>%
  as_tibble()

# FIND BEST CLUSTER SIZE
set.seed(1234)
ks <- 2:10

# finding best cluster num by knee
WSS <- sapply(ks, FUN = function(k) {
  kmeans(public_commuters_cases_deaths, centers = k, nstart = 5)$tot.withinss
})
ggplot(as_tibble(ks, WSS), aes(ks, WSS)) + geom_line() +
  labs(title = "Kmeans Within Sum Squared of Clusters For Commuters By Public Transportation and Case/Death Counts")

# finding best cluster num by silhoutte width
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

# finding best cluster num by dunn index
DI <- sapply(ks, FUN=function(k) {
  fpc::cluster.stats(d, kmeans(public_commuters_cases_deaths, centers=k, nstart=5)$cluster)$dunn
})
best_k_DI <- ks[which.max(DI)]
best_k_DI
ggplot(as_tibble(ks, DI), aes(ks, DI)) + geom_line() +
  geom_vline(xintercept = best_k_DI, color = "red", linetype = 2) +
  labs(title = "Kmeans Dunn Index of Clusters For Commuters By Public Transportation and Case/Death Counts")


# kmeans clustering
summary(public_commuters_cases_deaths)

km1 <- kmeans(public_commuters_cases_deaths, centers = 3)
km1

ggplot(pivot_longer(as_tibble(km1$centers,  rownames = "cluster"), 
  cols = colnames(km1$centers)), 
  aes(y = name, x = value)) +
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

# hierachical clustering
hc1 <- hclust(d, method = "complete")
ggdendrogram(hc1, labels = FALSE, theme_dendro = FALSE)
clusters <- cutree(hc1, k = 4)

clust.centroid = function(i, dat, clusters) {
  ind = (clusters == i)
  colMeans(dat[ind,])
}
centers <- sapply(unique(clusters), clust.centroid, public_commuters_cases_deaths, clusters)

centers_inv <- t(centers)
centers_inv
rownames(centers_inv) <- c(1,2,3,4)
centers_inv

class(centers)

ggplot(pivot_longer(as_tibble(centers_inv,  rownames = "cluster"), 
                    cols = colnames(centers_inv)), 
       aes(y = name, x = value)) +
  geom_bar(stat = "identity") +
  facet_grid(rows = vars(cluster)) +
  labs(title = "Hierarchical Cluster Centers Summary For Commuters By Public Transportation and Case/Death Counts")


public_commuters_cases_deaths_hccluster <- counties_TX %>%
  left_join(dataset %>% add_column(cluster = factor(clusters)))

rownames(public_commuters_cases_deaths) <- dataset$county_name

public_commuters_cases_deaths

factoextra::fviz_cluster(list(data = public_commuters_cases_deaths, cluster = clusters)) +
  labs(title = "Hierarchical Clusters of Commuters By Public Transportation and Case/Death Counts")

ggplot(public_commuters_cases_deaths_hccluster, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() + 
  scale_fill_viridis_d() + 
  labs(title = "Hierarchical Clusters of Commuters By Public Transportation and Case/Death Counts")

################################# Cluster 2 #################################

races_fatality <- dataset %>%
  select(
    fatality_rate,
    total_pop,
#    median_age
#    white_pop,
#    hispanic_pop,
#    black_pop,
#    asian_pop,
#    amerindian_pop,
#    other_race_pop
  ) %>%
  scale() %>%
  as_tibble()

summary(races_fatality)

km2 <- kmeans(races_fatality, centers = 4)
km2

ggplot(pivot_longer(as_tibble(km2$centers,  rownames = "cluster"), 
                    cols = colnames(km2$centers)), 
       aes(y = name, x = value)) +
  geom_bar(stat = "identity") +
  facet_grid(rows = vars(cluster))

races_fatality_cluster <- counties_TX %>%
  left_join(dataset %>% add_column(cluster = factor(km2$cluster)))

ggplot(races_fatality_cluster, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() + 
  scale_fill_viridis_d() + 
  labs(title = "Clusters of Commuters By Public Transportation and Case/Death Counts")


################################# cluster 1 #################################
scaled_TX_race <- sub %>% 
  select(
    median_income,
    white_pop,
    black_pop,
    asian_pop,
    hispanic_pop,
    amerindian_pop,
    other_race_pop
  ) %>% 
  scale() %>% as_tibble()

summary(scaled_TX_race)

km1 <- kmeans(scaled_TX_race, centers = 4)
km1

ggplot(pivot_longer(as_tibble(km1$centers,  rownames = "cluster"), 
  cols = colnames(km1$centers)), 
  aes(y = name, x = value)) +
  geom_bar(stat = "identity") +
  facet_grid(rows = vars(cluster))

# map
counties <- as_tibble(map_data("county"))
counties_TX <- counties %>% dplyr::filter(region == "texas") %>% 
  rename(c(county = subregion))

cases_TX_cluster <- sub %>% mutate(county = county_name %>% 
  str_to_lower() %>% str_replace('\\s+county\\s*$', ''))

head(cases_TX_cluster)


counties_TX_clust <- counties_TX %>% left_join(cases_TX_cluster %>% 
  add_column(cluster = factor(km1$cluster)))

ggplot(counties_TX_clust, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() + 
  scale_fill_viridis_d() + 
  labs(title = "Clusters", subtitle = "Only counties reporting 100+ cases")



################################# cluster 2 #################################
scaled_TX_race_1000 <- sub %>% 
  select(
    median_income,
    white_per_1000,
    black_per_1000,
    asian_per_1000,
    hispanic_per_1000,
    amerindian_per_1000,
    other_per_1000
  ) %>% 
  scale() %>% as_tibble()

summary(scaled_TX_race_1000)

# scaled_TX_race_1000[, c(1)] <- scale(scaled_TX_race_1000[, c(1)])

# summary(scaled_TX_race_1000)

km2 <- kmeans(scaled_TX_race_1000, centers = 4)
km2

ggplot(pivot_longer(as_tibble(km2$centers,  rownames = "cluster"), 
                    cols = colnames(km2$centers)), 
       aes(y = name, x = value)) +
  geom_bar(stat = "identity") +
  facet_grid(rows = vars(cluster))

# map

counties <- as_tibble(map_data("county"))
counties_TX <- counties %>% dplyr::filter(region == "texas") %>% 
  rename(c(county = subregion))

scaled_TX_race_1000 <- sub %>% mutate(county = county_name %>% 
                                     str_to_lower() %>% str_replace('\\s+county\\s*$', ''))

head(scaled_TX_race_1000)


counties_TX_clust_1000 <- counties_TX %>% left_join(scaled_TX_race_1000 %>% 
                                                 add_column(cluster = factor(km2$cluster)))

ggplot(counties_TX_clust_1000, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() + 
  scale_fill_viridis_d() + 
  labs(title = "Clusters", subtitle = "Only counties reporting 100+ cases")

################################# cluster 3 #################################















