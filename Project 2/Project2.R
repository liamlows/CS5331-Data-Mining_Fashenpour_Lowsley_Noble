library(tidyverse)
library(lubridate)
library(ggplot2)

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
                                                      "income_per_capita"))

# subset to only use Texas data
cases_TX_census <- subset(cases_US_census, state == "TX")
head(cases_TX_census)
# check types
lapply(cases_TX_census, class)
# check NA
lapply(cases_TX_census, anyNA)

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
sub <- subset(cases_TX, date == dates[length(dates)])
head(cases_TX_census)
# add populations
sub <- merge(sub, subset(cases_TX_census, select=c("county_name",
                                                   "total_pop",
                                                   "median_age",
                                                   "white_pop",
                                                   "black_pop",
                                                   "asian_pop",
                                                   "hispanic_pop",
                                                   "amerindian_pop",
                                                   "other_race_pop",
                                                   "median_income",
                                                   "income_per_capita")), by="county_name")
head(sub)
# add cases per 1000 and deaths per 1000 columns
cases_per_1000 <- (sub$confirmed_cases/sub$total_pop) * 1000
sub <- cbind(sub, cases_per_1000)
deaths_per_1000 <- (sub$deaths/sub$total_pop) * 1000
sub <- cbind(sub, deaths_per_1000)
white_per_1000 <- (sub$white_pop/sub$total_pop) * 1000
sub <- cbind(sub, white_per_1000)
hispanic_per_1000 <- (sub$hispanic_pop/sub$total_pop) * 1000
sub <- cbind(sub, hispanic_per_1000)
black_per_1000 <- (sub$black_pop/sub$total_pop) * 1000
sub <- cbind(sub, black_per_1000)
asian_per_1000 <- (sub$asian_pop/sub$total_pop) * 1000
sub <- cbind(sub, asian_per_1000)
amerindian_per_1000 <- (sub$amerindian_pop/sub$total_pop) * 1000
sub <- cbind(sub, amerindian_per_1000)
other_per_1000 <- (sub$other_race_pop/sub$total_pop) * 1000
sub <- cbind(sub, other_per_1000)
head(sub)



# cluster 1
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



# cluster 2
scaled_TX_race_1000 <- sub %>% 
  select(
    median_income,
    white_per_1000,
    black_per_1000,
    asian_per_1000,
    hispanic_per_1000,
    amerindian_per_1000,
    other_per_1000
  ) %>% as_tibble()

summary(scaled_TX_race_1000)

scaled_TX_race_1000[, c(1)] <- scale(scaled_TX_race_1000[, c(1)])

summary(scaled_TX_race_1000)

km2 <- kmeans(scaled_TX_race_1000, centers = 4)
km2

ggplot(pivot_longer(as_tibble(km2$centers,  rownames = "cluster"), 
                    cols = colnames(km2$centers)), 
       aes(y = name, x = value)) +
  geom_bar(stat = "identity") +
  facet_grid(rows = vars(cluster))

# map

scaled_TX_race_1000 <- scaled_TX_race_1000 %>% mutate(county = county_name %>% 
                                     str_to_lower() %>% str_replace('\\s+county\\s*$', ''))

head(scaled_TX_race_1000)


counties_TX_clust <- counties_TX %>% left_join(cases_TX_cluster %>% 
                                                 add_column(cluster = factor(km1$cluster)))

ggplot(counties_TX_clust, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = cluster)) +
  coord_quickmap() + 
  scale_fill_viridis_d() + 
  labs(title = "Clusters", subtitle = "Only counties reporting 100+ cases")

















