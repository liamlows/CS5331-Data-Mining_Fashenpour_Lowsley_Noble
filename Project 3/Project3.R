library(tidyverse)
library(ggplot2)
library(caret)
library(FSelector)
library(DT)
library(seriation)
library(pROC)
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
draw_confusion_matrix <- function(cm) {
  
  layout(matrix(c(1,1,2)))
  par(mar=c(2,2,2,2))
  plot(c(100, 345), c(300, 450), type = "n", xlab="", ylab="", xaxt='n', yaxt='n')
  title('CONFUSION MATRIX', cex.main=2)
  
  # create the matrix 
  rect(150, 430, 240, 370, col='#3F97D0')
  text(195, 435, 'Class1', cex=1.2)
  rect(250, 430, 340, 370, col='#F7AD50')
  text(295, 435, 'Class2', cex=1.2)
  text(125, 370, 'Predicted', cex=1.3, srt=90, font=2)
  text(245, 450, 'Actual', cex=1.3, font=2)
  rect(150, 305, 240, 365, col='#F7AD50')
  rect(250, 305, 340, 365, col='#3F97D0')
  text(140, 400, 'Class1', cex=1.2, srt=90)
  text(140, 335, 'Class2', cex=1.2, srt=90)
  
  # add in the cm results 
  res <- as.numeric(cm$table)
  text(195, 400, res[1], cex=1.6, font=2, col='white')
  text(195, 335, res[2], cex=1.6, font=2, col='white')
  text(295, 400, res[3], cex=1.6, font=2, col='white')
  text(295, 335, res[4], cex=1.6, font=2, col='white')
  
  # add in the specifics 
  plot(c(100, 0), c(100, 0), type = "n", xlab="", ylab="", main = "DETAILS", xaxt='n', yaxt='n')
  text(10, 85, names(cm$byClass[1]), cex=1.2, font=2)
  text(10, 70, round(as.numeric(cm$byClass[1]), 3), cex=1.2)
  text(30, 85, names(cm$byClass[2]), cex=1.2, font=2)
  text(30, 70, round(as.numeric(cm$byClass[2]), 3), cex=1.2)
  text(50, 85, names(cm$byClass[5]), cex=1.2, font=2)
  text(50, 70, round(as.numeric(cm$byClass[5]), 3), cex=1.2)
  text(70, 85, names(cm$byClass[6]), cex=1.2, font=2)
  text(70, 70, round(as.numeric(cm$byClass[6]), 3), cex=1.2)
  text(90, 85, names(cm$byClass[7]), cex=1.2, font=2)
  text(90, 70, round(as.numeric(cm$byClass[7]), 3), cex=1.2)
  
  # add in the accuracy information 
  text(30, 35, names(cm$overall[1]), cex=1.5, font=2)
  text(30, 20, round(as.numeric(cm$overall[1]), 3), cex=1.4)
  text(70, 35, names(cm$overall[2]), cex=1.5, font=2)
  text(70, 20, round(as.numeric(cm$overall[2]), 3), cex=1.4)
}  
# -----------------------------------------------------------------------------------
# read in US data w/ census csv
cases_US_census <- read.csv("./Projects/Project\ 3/data/04-24-21_COVID-CENSUS.csv")
# subset to include chosen data attributes
cases_US_census <- subset(cases_US_census, select = c("county_name",
                                                      "state",
                                                      "total_pop",
                                                      "confirmed_cases",
                                                      "deaths",
                                                      "male_pop",
                                                      "female_pop",
                                                      "white_pop",
                                                      "black_pop",
                                                      "asian_pop",
                                                      "hispanic_pop",
                                                      "amerindian_pop",
                                                      #"other_race_pop",
                                                      "commuters_by_public_transportation",
                                                      "worked_at_home",
                                                      "poverty",
                                                      "bachelors_degree",
                                                      "high_school_diploma",
                                                      "high_school_including_ged",
                                                      "gini_index",
                                                      "in_undergrad_college",
                                                      "in_school",
                                                      "million_dollar_housing_units",
                                                      "walked_to_work",
                                                      "masters_degree",
                                                      "median_age",
                                                      "median_income",
                                                      "income_per_capita"))

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

# Make character factors for analysis
cases_US_census <- cases_US_census %>% mutate_if(is.character, factor)
# remove any entries where confirmed cases are = 0
cases_US_census <- cases_US_census %>% filter(confirmed_cases > 0) 

# add per 1000 values
dataset <- cases_US_census %>% mutate(
  confirmed_cases = confirmed_cases/total_pop*1000,
  deaths = deaths/total_pop*1000,
  male_pop = male_pop/total_pop,
  female_pop = female_pop/total_pop,
  white_pop = white_pop/total_pop,
  black_pop = black_pop/total_pop,
  asian_pop = asian_pop/total_pop,
  hispanic_pop = hispanic_pop/total_pop,
  amerindian_pop = amerindian_pop/total_pop,
  #other_race_pop = other_race_pop/total_pop,
  commuters_by_public_transportation = commuters_by_public_transportation/total_pop,
  worked_at_home = worked_at_home/total_pop,
  poverty = poverty/total_pop,
  #associates_degree = associates_degree/total_pop,
  bachelors_degree = bachelors_degree/total_pop,
  high_school_diploma = high_school_diploma/total_pop,
  high_school_including_ged = high_school_including_ged/total_pop,
  in_school = in_school/total_pop,
  in_undergrad_college = in_undergrad_college/total_pop,
  million_dollar_housing_units = million_dollar_housing_units/total_pop,
  walked_to_work = walked_to_work/total_pop,
  masters_degree = masters_degree/total_pop,
  fatality_rate = deaths/confirmed_cases
)
summary(dataset)

# -----------------------------------------------------------------------------------

# deaths per 1000 > 1.8
dataset_sel <- dataset %>% mutate(fatal = as.factor(deaths > 1.6))

mean(dataset$deaths)
median(dataset$deaths)
sum(dataset$deaths > 1.6)
sum(dataset$deaths < 1.6)
sum(dataset$state == "TX")

# balance is ok
dataset_sel %>% pull(fatal) %>% table()

# states affected worst
dataset_sel %>% group_by(state) %>% 
  summarize(fatal_pct = sum(fatal == TRUE)/n()) %>%
  arrange(desc(fatal_pct)) %>% print(n=51)

# ########################### TRAINING #############################

# train is not TX
cases_train1 <- dataset_sel %>% filter(!(state %in% c("TX", "CA", "NY", "FL", "ND", "LA", "VT")))
train_bal <- cases_train1 %>% pull(fatal) %>% table()

# test is TX
cases_test1 <-  dataset_sel %>% filter((state %in% c("TX", "CA", "NY", "FL", "ND", "LA", "VT")))
test_bal <- cases_test1 %>% pull(fatal) %>% table()

# remove these for training
cases_train1 <- cases_train1 %>% select(-deaths, -confirmed_cases, -fatality_rate)

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
  labs(title = "U.S. Map of Deaths Per 1000", subtitle = "Red = greater than 1.6 per 1000, Grey = less than 1.6 per 1000")

# check variable importance
weights <- cases_train1 %>%  chi.squared(fatal ~ ., data = .) %>% as_tibble(rownames = "feature") %>% arrange(desc(attr_importance))
weights
print.data.frame(weights)

ggplot(weights,
  aes(x = attr_importance, y = reorder(feature, attr_importance))) +
  geom_bar(stat = "identity") +
  xlab("Importance score") + ylab("Feature") + labs(title = "Chi Squared Importance Test")

# RPART METHOD
fit1 <- cases_train1 %>%
  train(fatal ~ . - county_name - state,
        data = . ,
        method = "rpart",
        tuneLength = 10,
        trControl = trainControl(method = "cv", number = 10),
  )
fit1
ggplot(fit1) + labs(title = "Accuracy With Increasing Complexity Parameter")

ggplot(varImp(fit1),
  aes(x = Overall, y = reorder(feature, Overall))) +
  geom_bar(stat = "identity") +
  xlab("Variable") + ylab("Importance %") + labs(title = "RPart Variable Importance")

# RF METHOD
fit2 <- cases_train1 %>%
  train(fatal ~ . - county_name - state,
        data = . ,
        method = "rf",
        tuneLength = 10,
        trControl = trainControl(method = "cv", number = 10),
  )
fit2
ggplot(fit2) + labs(title = "Accuracy With Increasing MTRY Value")

ggplot(varImp(fit2),
       aes(x = Overall, y = reorder(feature, Overall))) +
  geom_bar(stat = "identity") +
  xlab("Variable") + ylab("Importance %") + labs(title = "Random Forest Variable Importance")

# NB METHOD
fit3 <- cases_train1 %>%
  train(fatal ~ . - county_name - state,
        data = . ,
        method = "nb",
        tuneLength = 10,
        trControl = trainControl(method = "cv", number = 10),
  )
fit3
ggplot(fit3) + labs(title = "Accuracy With Different Kernel Type")

ggplot(varImp(fit3),
       aes(x = Overall, y = reorder(feature, Overall))) +
  geom_bar(stat = "identity") +
  xlab("Variable") + ylab("Importance %") + labs(title = "Naive Bayes Variable Importance")

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
ggplot(fit4) + labs(title = "Accuracy With Increasing Number of Neighbors")

ggplot(varImp(fit4),
       aes(x = Overall, y = reorder(feature, Overall))) +
  geom_bar(stat = "identity") +
  xlab("Variable") + ylab("Importance %") + labs(title = "KNN Variable Importance")

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
cm1 <- confusionMatrix(data = test_fit1$fatal_predicted, ref = test_fit1$fatal)
draw_confusion_matrix(cm1)

prob <- predict(fit1, test_fit1, type="prob")

roc_graph <- roc(test_fit1$fatal == "TRUE", prob[,"TRUE"])
roc_graph$auc
ggroc(roc_graph) + geom_abline(intercept = 1, slope = 1, color = "darkgrey") + labs(title="ROC Curve for RPART")

counties2 <- as_tibble(map_data("county"))
counties_TX <- counties2 %>% dplyr::filter(region == "texas") %>% rename(c(county = subregion))

counties_test1 <- counties %>% left_join(test_fit1 %>% 
  mutate(county = county_name %>% str_to_lower() %>% 
  str_replace('\\s+county\\s*$', '')))

# ggplot(counties_test1, aes(long, lat)) + 
#   geom_polygon(aes(group = group, fill = fatal), color = "black", size = 0.1) + 
#   coord_quickmap() + 
#   scale_fill_manual(values = c('TRUE' = 'red', 'FALSE' = 'grey'))
# 
# ggplot(counties_test1, aes(long, lat)) + 
#   geom_polygon(aes(group = group, fill = fatal_predicted), color = "black", size = 0.1) + 
#   coord_quickmap() + 
#   scale_fill_manual(values = c('TRUE' = 'red', 'FALSE' = 'grey'))

ggplot(counties_test1, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = fatal), color = "black", size = 0.1) + 
  coord_quickmap() + scale_fill_manual(values = c('TRUE' = 'red', 'FALSE' = 'grey')) +
  labs(title = "Actual Classifications", subtitle = "Red = greater than 1.6 per 1000, Grey = less than 1.6 per 1000")

ggplot(counties_test1, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = fatal_predicted), color = "black", size = 0.1) + 
  coord_quickmap() + scale_fill_manual(values = c('TRUE' = 'red', 'FALSE' = 'grey')) +
  labs(title = "RPART Predicted Classifications", subtitle = "Red = greater than 1.6 per 1000, Grey = less than 1.6 per 1000")

# RF
test_fit2 <- cases_test1
test_fit2 <- test_fit2 %>% na.omit
test_fit2$fatal_predicted <- predict(fit2, test_fit2)
cm2 <- confusionMatrix(data = test_fit2$fatal_predicted, ref = test_fit2$fatal)
cm2
draw_confusion_matrix(cm2)

counties_test2 <- counties %>% left_join(test_fit2 %>% 
  mutate(county = county_name %>% str_to_lower() %>% 
  str_replace('\\s+county\\s*$', '')))

# ggplot(counties_test2, aes(long, lat)) + 
#   geom_polygon(aes(group = group, fill = fatal), color = "black", size = 0.1) + 
#   coord_quickmap() + 
#   scale_fill_manual(values = c('TRUE' = 'red', 'FALSE' = 'grey'))
# 
# ggplot(counties_test2, aes(long, lat)) + 
#   geom_polygon(aes(group = group, fill = fatal_predicted), color = "black", size = 0.1) + 
#   coord_quickmap() + 
#   scale_fill_manual(values = c('TRUE' = 'red', 'FALSE' = 'grey'))

ggplot(counties_test2, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = fatal), color = "black", size = 0.1) + 
  coord_quickmap() + scale_fill_manual(values = c('TRUE' = 'red', 'FALSE' = 'grey')) +
  labs(title = "Actual Classifications", subtitle = "Red = greater than 1.6 per 1000, Grey = less than 1.6 per 1000")

ggplot(counties_test2, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = fatal_predicted), color = "black", size = 0.1) + 
  coord_quickmap() + scale_fill_manual(values = c('TRUE' = 'red', 'FALSE' = 'grey')) +
  labs(title = "RF Predicted Classifications", subtitle = "Red = greater than 1.6 per 1000, Grey = less than 1.6 per 1000")


# NB
test_fit3 <- cases_test1
test_fit3 <- test_fit3 %>% na.omit
test_fit3$fatal_predicted <- predict(fit3, test_fit3)
cm3 <- confusionMatrix(data = test_fit3$fatal_predicted, ref = test_fit3$fatal)

draw_confusion_matrix(cm3)

counties_test3 <- counties %>% left_join(test_fit3 %>% 
  mutate(county = county_name %>% str_to_lower() %>% 
  str_replace('\\s+county\\s*$', '')))

# ggplot(counties_test3, aes(long, lat)) + 
#   geom_polygon(aes(group = group, fill = fatal), color = "black", size = 0.1) + 
#   coord_quickmap() + 
#   scale_fill_manual(values = c('TRUE' = 'red', 'FALSE' = 'grey'))
# 
# ggplot(counties_test3, aes(long, lat)) + 
#   geom_polygon(aes(group = group, fill = fatal_predicted), color = "black", size = 0.1) + 
#   coord_quickmap() + 
#   scale_fill_manual(values = c('TRUE' = 'red', 'FALSE' = 'grey'))

ggplot(counties_test3, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = fatal), color = "black", size = 0.1) + 
  coord_quickmap() + scale_fill_manual(values = c('TRUE' = 'red', 'FALSE' = 'grey')) +
  labs(title = "Actual Classifications", subtitle = "Red = greater than 1.6 per 1000, Grey = less than 1.6 per 1000")

ggplot(counties_test3, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = fatal_predicted), color = "black", size = 0.1) + 
  coord_quickmap() + scale_fill_manual(values = c('TRUE' = 'red', 'FALSE' = 'grey')) +
  labs(title = "NB Predicted Classifications", subtitle = "Red = greater than 1.6 per 1000, Grey = less than 1.6 per 1000")


# KNN
test_fit4 <- cases_test1
test_fit4 <- test_fit4 %>% na.omit
test_fit4$fatal_predicted <- predict(fit4, test_fit4)
cm4 <- confusionMatrix(data = test_fit4$fatal_predicted, ref = test_fit4$fatal)
draw_confusion_matrix(cm4)

counties_test4 <- counties %>% left_join(test_fit4 %>% 
  mutate(county = county_name %>% str_to_lower() %>% 
  str_replace('\\s+county\\s*$', '')))

ggplot(counties_test4, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = fatal), color = "black", size = 0.1) + 
  coord_quickmap() + 
  scale_fill_manual(values = c('TRUE' = 'red', 'FALSE' = 'grey')) +
  labs(title = "Actual Classifications", subtitle = "Red = greater than 1.6 per 1000, Grey = less than 1.6 per 1000")

ggplot(counties_test4, aes(long, lat)) + 
  geom_polygon(aes(group = group, fill = fatal_predicted), color = "black", size = 0.1) + 
  coord_quickmap() + 
  scale_fill_manual(values = c('TRUE' = 'red', 'FALSE' = 'grey')) +
  labs(title = "KNN Predicted Classifications", subtitle = "Red = greater than 1.6 per 1000, Grey = less than 1.6 per 1000")




prob <- predict(fit1, test_fit1, type="prob")
roc1 <- roc(test_fit1$fatal == "TRUE", prob[,"TRUE"])

prob <- predict(fit2, test_fit2, type="prob")
roc2 <- roc(test_fit2$fatal == "TRUE", prob[,"TRUE"])

prob <- predict(fit3, test_fit3, type="prob")
roc3 <- roc(test_fit3$fatal == "TRUE", prob[,"TRUE"])

prob <- predict(fit4, test_fit4, type="prob")
roc4 <- roc(test_fit4$fatal == "TRUE", prob[,"TRUE"])

ggroc(list("RPART ROC"=roc1, "RF ROC"=roc2, "NB ROC"=roc3, "KNN ROC"=roc4)) + 
  geom_abline(intercept = 1, slope = 1, color = "darkgrey") + 
  labs(title="ROC Curves for each Model")
roc1$auc
roc2$auc
roc3$auc
roc4$auc

resamps <- resamples(list(
  rpart = fit1,
  rf = fit2,
  nb = fit3,
  knn = fit4
))
summary(resamps)
diffs <- diff(resamps)
summary(diffs)




