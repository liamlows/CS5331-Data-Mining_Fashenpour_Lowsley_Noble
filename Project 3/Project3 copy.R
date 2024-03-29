library(tidyverse)
library(ggplot2)
library(caret)
library(FSelector)
library(DT)
library(seriation)
# -----------------------------------------------------------------------------------
# STAT FUNCTIONS
Mode <- function(x) {
  ux <- unique(x)
  ux[which.max(tabulate(match(x, ux)))]
}
Freq <- function (x){
  max(table(x))
}
Rnge <- function(x) {
  if(is.numeric(x)){
    (max(x) - min(x))
  }
}
# -----------------------------------------------------------------------------------
# read in US data w/ census csv
cases_US_census <- read.csv("./Projects/Project\ 3/data/04-24-21_COVID-CENSUS.csv")
# subset to include chosen data attributes
# cases_US_census <- subset(cases_US_census, select = c("county_name",
#                                                       "state",
#                                                       "total_pop",
#                                                       "confirmed_cases",
#                                                       "deaths",
#                                                       "male_pop",
#                                                       "female_pop",
#                                                       "white_pop",
#                                                       "black_pop",
#                                                       "asian_pop",
#                                                       "hispanic_pop",
#                                                       "amerindian_pop",
#                                                       #"other_race_pop",
#                                                       "commuters_by_public_transportation",
#                                                       "worked_at_home",
#                                                       "poverty",
#                                                       "associates_degree",
#                                                       "bachelors_degree",
#                                                       "high_school_diploma",
#                                                       "high_school_including_ged",
#                                                       "gini_index",
#                                                       "in_undergrad_college",
#                                                       "in_school",
#                                                       "median_age",
#                                                       "median_income",
#                                                       "income_per_capita"))

#head(cases_US_census)
# check types
#lapply(cases_US_census, class)
# check NA
#lapply(cases_US_census, anyNA)
# AT THIS POINT NO NA VALUES AND TYPES ARE CORRECT
# -----------------------------------------------------------------------------------

# county names in Louisiana contain 'Parish' at the end, need to remove them to
# match the map counties
la_county <- cases_US_census$county_name[cases_US_census$state == 'LA']
la_county <- gsub("[.]","",la_county) 
la_county <- gsub("\\s*\\w*$", "", la_county)
la_county <- gsub("\\s*\\w*$", "", la_county)
cases_US_census$county_name[cases_US_census$state == 'LA'] <- la_county
cases_US_census$county_name[cases_US_census$state == 'LA']
cases_US_census$county_name[cases_US_census$county_name=="DeWitt County " | cases_US_census$county_name=="Dewitt County " ] <- "De witt County"

cases_US_census <- cases_US_census %>% select(-state_fips_code, -county_fips_code, -geo_id)

# Make character factors for analysis
cases_US_census <- cases_US_census %>% mutate_if(is.character, factor)

cases_US_census <- cases_US_census %>% filter(confirmed_cases > 0) 

# add per 1000 values
dataset <- cases_US_census %>% mutate(
  confirmed_cases = confirmed_cases/total_pop*1000,
  deaths = deaths/total_pop*1000,
  # male_pop = male_pop/total_pop,
  # female_pop = female_pop/total_pop,
  # white_pop = white_pop/total_pop,
  # black_pop = black_pop/total_pop,
  # asian_pop = asian_pop/total_pop,
  # hispanic_pop = hispanic_pop/total_pop,
  # amerindian_pop = amerindian_pop/total_pop,
  # #other_race_pop = other_race_pop/total_pop,
  # commuters_by_public_transportation = commuters_by_public_transportation/total_pop,
  # worked_at_home = worked_at_home/total_pop,
  # poverty = poverty/total_pop,
  # associates_degree = associates_degree/total_pop,
  # bachelors_degree = bachelors_degree/total_pop,
  # high_school_diploma = high_school_diploma/total_pop,
  # high_school_including_ged = high_school_including_ged/total_pop,
  # in_school = in_school/total_pop,
  # in_undergrad_college = in_undergrad_college/total_pop,
  fatality_rate = deaths/confirmed_cases
)
summary(dataset)

dataset <- dataset[ , apply(dataset, 2, function(x) !any(is.na(x)))]





# remove NA values because 0/0
#dataset[which(is.na(dataset$fatality_rate)),]
#dataset[which(is.na(dataset$fatality_rate)),]$fatality_rate <- 0
# check
#summary(dataset)

# statistic functions
#lapply(dataset, FUN = mean)
#lapply(dataset, FUN = median)
#lapply(dataset, FUN = Mode)
#lapply(dataset, FUN = Freq)
#lapply(dataset, FUN = sd)
#lapply(dataset, FUN = var)
#lapply(dataset, FUN = min)
#lapply(dataset, FUN = max)
#lapply(dataset, FUN = Rnge)

# deaths per 1000 > 1.8
dataset_sel <- dataset %>% mutate(fatal = as.factor(deaths > 1.8))



#head(dataset_sel)

# balance is ok
dataset_sel %>% pull(fatal) %>% table()

# states affected worst
dataset_sel %>% group_by(state) %>% 
  summarize(fatal_pct = sum(fatal == TRUE)/n()) %>%
  arrange(desc(fatal_pct))

# ########################### TRAINING #############################

# train is not TX
cases_train1 <- dataset_sel %>% filter(!(state %in% c("TX")))
cases_train1 %>% pull(fatal) %>% table()

# test is TX
cases_test1 <-  dataset_sel %>% filter((state %in% c("TX")))
cases_test1 %>% pull(fatal) %>% table()

# obtain counties
counties <- as_tibble(map_data("county"))
counties <- counties %>% 
  rename(c(county = subregion, state = region)) %>%
  mutate(state = state.abb[match(state, tolower(state.name))]) %>%
  select(state, county, long, lat, group)
counties

# add county names to map
counties_all <- counties %>% left_join(cases_train1 %>% 
    mutate(county = county_name %>% str_to_lower() %>% 
    str_replace('\\s+county\\s*$', '')))

# plot
ggplot(counties_all, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = fatal), color = "black", size = 0.1) + 
  coord_quickmap() + scale_fill_manual(values = c('TRUE' = 'red', 'FALSE' = 'grey')) +
  labs(title = "U.S. Map of Deaths Per 1000", subtitle = "Red = greater than 1.8 per 1000, Grey = less than 1.8 per 1000")

colnames(cases_train1)

# check variable importance
cases_train1 <- cases_train1 %>% select(-deaths, -confirmed_cases, -fatality_rate)
cases_train1 <- cases_train1 %>% select(-date, -do_date)

weights <- cases_train1 %>%  chi.squared(fatal ~ ., data = .) %>% as_tibble(rownames = "feature") %>% arrange(desc(attr_importance))

weights
print.data.frame(weights)

ggplot(weights,
  aes(x = attr_importance, y = reorder(feature, attr_importance))) +
  geom_bar(stat = "identity") +
  xlab("Importance score") + ylab("Feature") + labs(title = "Chi Squared Importance Test")

head(cases_train1)

# RPART METHOD
fit1 <- cases_train1 %>%
  train(fatal ~ . - county_name - state,
        data = . ,
        method = "rpart",
        tuneLength = 10,
        trControl = trainControl(method = "cv", number = 10),
        preProcess= "scale"
  )
fit1
varImp(fit1)$importance

# RF METHOD
fit2 <- cases_train1 %>%
  train(fatal ~ . - county_name - state,
        data = . ,
        method = "rf",
        tuneLength = 10,
        trControl = trainControl(method = "cv", number = 10),
  )
fit2
varImp(fit2)$importance

# NB METHOD
fit3 <- cases_train1 %>%
  train(fatal ~ . - county_name - state,
        data = . ,
        method = "nb",
        tuneLength = 10,
        trControl = trainControl(method = "cv", number = 10),
  )
fit3
varImp(fit3)

# KNN METHOD
fit4 <- cases_train1 %>%
  train(fatal ~ . - county_name - state,
        data = . ,
        method = "knn",
        tuneLength = 10,
        trControl = trainControl(method = "cv", number = 10),
        preProcess= "scale"
  )
fit4
varImp(fit4)

# NEURAL NET METHOD
fit5 <- cases_train1 %>%
  train(fatal ~ . - county_name - state,
        data = . ,
        method = "nnet",
        trControl = trainControl(method = "cv", number = 10),
        trace=FALSE
  )
fit5
varImp(fit5)


# ########################### TESTING ########################### 

# RPART
test_fit1 <- cases_test1
test_fit1 <- test_fit1 %>% na.omit
test_fit1$fatal_predicted <- predict(fit1, test_fit1)

counties2 <- as_tibble(map_data("county"))
counties_TX <- counties2 %>% dplyr::filter(region == "texas") %>% rename(c(county = subregion))

counties_test <- counties_TX %>% left_join(test_fit1 %>% 
  mutate(county = county_name %>% str_to_lower() %>% 
  str_replace('\\s+county\\s*$', '')))

ggplot(counties_test, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = fatal), color = "black", size = 0.1) + 
  coord_quickmap() + 
  scale_fill_manual(values = c('TRUE' = 'red', 'FALSE' = 'grey'))

ggplot(counties_test, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = fatal_predicted), color = "black", size = 0.1) + 
  coord_quickmap() + 
  scale_fill_manual(values = c('TRUE' = 'red', 'FALSE' = 'grey'))

confusionMatrix(data = test_fit1$fatal_predicted, ref = test_fit1$fatal)

# RF
test_fit2 <- cases_test1
test_fit2 <- test_fit2 %>% na.omit
test_fit2$fatal_predicted <- predict(fit2, test_fit2)

counties_test <- counties_TX %>% left_join(test_fit2 %>% 
  mutate(county = county_name %>% str_to_lower() %>% 
  str_replace('\\s+county\\s*$', '')))

ggplot(counties_test, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = fatal), color = "black", size = 0.1) + 
  coord_quickmap() + 
  scale_fill_manual(values = c('TRUE' = 'red', 'FALSE' = 'grey'))

ggplot(counties_test, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = fatal_predicted), color = "black", size = 0.1) + 
  coord_quickmap() + 
  scale_fill_manual(values = c('TRUE' = 'red', 'FALSE' = 'grey'))

confusionMatrix(data = test_fit2$fatal_predicted, ref = test_fit2$fatal)

# NB
test_fit3 <- cases_test1
test_fit3 <- test_fit3 %>% na.omit
test_fit3$fatal_predicted <- predict(fit3, test_fit3)

counties_test <- counties_TX %>% left_join(test_fit3 %>% 
  mutate(county = county_name %>% str_to_lower() %>% 
  str_replace('\\s+county\\s*$', '')))

ggplot(counties_test, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = fatal), color = "black", size = 0.1) + 
  coord_quickmap() + 
  scale_fill_manual(values = c('TRUE' = 'red', 'FALSE' = 'grey'))

ggplot(counties_test, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = fatal_predicted), color = "black", size = 0.1) + 
  coord_quickmap() + 
  scale_fill_manual(values = c('TRUE' = 'red', 'FALSE' = 'grey'))

confusionMatrix(data = test_fit3$fatal_predicted, ref = test_fit3$fatal)

