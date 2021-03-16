library(tidyverse)
library(lubridate)
library(ggplot2)
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

# read in texas data csv
cases_TX <- read.csv("./Projects/Project\ 1/data/COVID-19_cases_TX.csv")
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
cases_US_census <- read.csv("./Projects/Project\ 1/data/COVID-19_cases_plus_census.csv")
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


# ------------------------------------------------------------------------------
# ----------------------------------  GRAPH 1  ---------------------------------
# ------------------------------------------------------------------------------

# library
library(ggplot2)
library(dplyr)
library(hrbrthemes)

# Build dataset with different distributions
data <- data.frame(
  race = c( rep("White", 254), rep("Black", 254), 
            rep("Asian", 254), rep("Hispanic", 254), 
            rep("American Indian", 254), rep("Other Race", 254) ),
  value = c( cases_TX_census$white_pop, cases_TX_census$black_pop, 
             cases_TX_census$asian_pop, cases_TX_census$hispanic_pop, 
             cases_TX_census$amerindian_pop, cases_TX_census$other_race_pop )
)
data

# Represent it
p <- data %>%
  ggplot( aes(x=log1p(value), fill=race)) +
  geom_histogram( color="#e9ecef", alpha=0.5, position = 'identity', bins = 100) +
  # scale_fill_manual(values=c("#69b3a2", "#404080")) +
  theme_ipsum() +
  labs(fill="")

p

# ------------------------------------------------------------------------------
# ----------------------------------  GRAPH 2  ---------------------------------
# ------------------------------------------------------------------------------
# library
library(ggridges)
library(ggplot2)


white_per_1000 <- (cases_TX_census$white_pop/cases_TX_census$total_pop) * 1000
black_per_1000 <- (cases_TX_census$black_pop/cases_TX_census$total_pop) * 1000
asian_per_1000 <- (cases_TX_census$asian_pop/cases_TX_census$total_pop) * 1000
hispanic_per_1000 <- (cases_TX_census$hispanic_pop/cases_TX_census$total_pop) * 1000
amerindian_per_1000 <- (cases_TX_census$amerindian_pop/cases_TX_census$total_pop) * 1000
other_per_1000 <- (cases_TX_census$other_race_pop/cases_TX_census$total_pop) * 1000

# obtain total race populations per county
data_ridges <- data.frame(
  race = c( rep("White", 254), rep("Black", 254), 
            rep("Asian", 254), rep("Hispanic", 254), 
            rep("American Indian", 254), rep("Other Race", 254) ),
  value = c( white_per_1000, black_per_1000, 
             asian_per_1000, hispanic_per_1000, 
             amerindian_per_1000, other_per_1000 )
)

# basic example
ggplot(data_ridges, aes(x = value, y = race, fill = race)) +
  geom_density_ridges() +
  ggtitle("Race populations per county") +
  xlab("Racial populations per 1000") + ylab("Race") +
  theme_ridges() + 
  theme(legend.position = "none")

# ------------------------------------------------------------------------------
# ----------------------------------  GRAPH 3  ---------------------------------
# ------------------------------------------------------------------------------

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



# create combined dataframe
hist_combined <- data.frame(
  type = c( rep("Confirmed Cases Per 1000", 254), rep("Deaths Per 1000", 254) ), 
  value = c( sub$cases_per_1000, sub$deaths_per_1000 )
)

# create cases dataframe
hist_cases <- data.frame(
  type = c( rep("Confirmed Cases Per 1000", 254) ), 
  value = c( sub$cases_per_1000 )
)

# create deaths dataframe
hist_deaths <- data.frame(
  type = c( rep("Deaths Per 1000", 254) ), 
  value = c( sub$deaths_per_1000 )
)

# graph of combined
p <- hist_combined %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity', bins = 75) +
  scale_fill_manual(values=c("#69b3a2", "#404080")) +
  ggtitle("Histogram of Confirmed Cases and Deaths by County") +
  xlab("Groupings of counties") + ylab("Frequency of groupings") +
  theme_ipsum() +
  labs(fill="")
p

# graph of cases
p1 <- hist_cases %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity', show.legend = FALSE) +
  stat_bin (geom="text", aes(label=..count..), vjust=-.4) +
  scale_fill_manual(values=c("#69b3a2")) +
  ggtitle("Histogram of Confirmed Cases by County") +
  xlab("Cases per 1000") + ylab("Frequency of groupings") +
  theme_ipsum() +
  labs(fill="")
p1

# graph of deaths
p2 <- hist_deaths %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity', show.legend = FALSE) +
  stat_bin (geom="text", aes(label=..count..), vjust=-.4) +
  scale_fill_manual(values=c( "#404080")) +
  ggtitle("Histogram of Deaths by County") +
  xlab("Deaths per 1000") + ylab("Frequency of groupings") +
  theme_ipsum() +
  labs(fill="")
p2

# ------------------------------------------------------------------------------
# ----------------------------------  GRAPH 4  ---------------------------------
# ------------------------------------------------------------------------------

# create ages dataframe
hist_age_data <- data.frame(
  type = c( rep("Median Age", 254) ), 
  value = c( cases_TX_census$median_age )
)

# graph of ages
hist_age <- hist_age_data %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity', show.legend = FALSE, fill="#ff9800") +
  stat_bin (geom="text", aes(label=..count..), vjust=-.4) +
  ggtitle("Histogram of Median Age by County") +
  xlab("Median Age") + ylab("Frequency of groupings") +
  theme_ipsum() +
  labs(fill="")
hist_age

# ------------------------------------------------------------------------------
# ----------------------------------  GRAPH 5  ---------------------------------
# ------------------------------------------------------------------------------

# create income dataframe
hist_income_data <- data.frame(
  type = c( rep("Median Income", 254) ), 
  value = c( cases_TX_census$median_income )
)

# graph of income
hist_income <- hist_income_data %>%
  ggplot( aes(x=value, fill=type)) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity', show.legend = FALSE, fill="#ff5722") +
  stat_bin (geom="text", aes(label=..count..), vjust=-.4) +
  scale_x_continuous(labels=scales::dollar_format()) +
  ggtitle("Histogram of Median Income by County") +
  xlab("Median Income") + ylab("Frequency of groupings") +
  theme_ipsum() +
  labs(fill="")
hist_income

# ------------------------------------------------------------------------------
# ----------------------------------  GRAPH 6  ---------------------------------
# ------------------------------------------------------------------------------

# create income dataframe
hist_pop_data <- data.frame(
  county = c( cases_TX_census$county_name),
  value = c( cases_TX_census$total_pop )
)


hist_pop_data

# graph of income
hist_pop <- hist_pop_data %>%
  ggplot( aes(x=value) ) +
  geom_histogram( color="#e9ecef", alpha=0.6, position = 'identity', 
                  show.legend = FALSE, fill = "#03a9f4") +
  stat_bin (geom="text", aes(label=..count..), vjust=-.4) +
  scale_fill_manual(values=c( "#404080")) +
  scale_x_continuous(trans = "log10") +
  ggtitle("Histogram of Total Population by County") +
  xlab("Total Population (log10)") + ylab("Frequency of groupings") +
  theme_ipsum() +
  labs(fill="")
hist_pop



# ------------------------------------------------------------------------------
# ----------------------------------  GRAPH 7  ---------------------------------
# ------------------------------------------------------------------------------

library(ggrepel)

head(cases_TX_census)

head(sub)

# create income dataframe
hist_ageincomepop_data <- data.frame(
  name = c( sub$county_name ),
  income = c( sub$median_income ), 
  age = c( sub$median_age ), 
  Population = c( sub$total_pop ) ,
  Deaths_Per_1000 = c ( sub$deaths_per_1000 )
)
head(hist_ageincomepop_data)

ggplot(hist_ageincomepop_data, aes(x=age, y=income, size = Population, color=Deaths_Per_1000, label=name)) +
  geom_point(alpha=0.7) + 
  geom_text_repel(data = subset(hist_ageincomepop_data, Population > quantile(Population, .96)),
                  size = 3.5,
                  max.overlaps = Inf,
                  nudge_x = .15,
                  box.padding = 2,
                  nudge_y = 1,
                  segment.curvature = -0.1,
                  segment.ncp = 3,
                  direction = "y",
                  hjust = 1,
                  xlim = c(50, NA),
                  segment.angle = 20) +
  scale_y_continuous(labels=scales::dollar_format()) +
  ggtitle("Age vs. Income With Population and Death Per 1000 Data") +
  xlab("Age") + ylab("Income")

# ------------------------------------------------------------------------------
# ----------------------------------  GRAPH 8  ---------------------------------
# ------------------------------------------------------------------------------

library(ggcorrplot)

head(cases_TX_census)

cases_TX_select <- subset(sub, select = c("confirmed_cases",
                                          "deaths",
                                          "total_pop",
                                          "median_age",
                                          "white_pop",
                                          "black_pop",
                                          "asian_pop",
                                          "hispanic_pop",
                                          "amerindian_pop",
                                          "other_race_pop",
                                          "median_income",
                                          "income_per_capita"
                                          ))

head(cases_TX_select)
lapply(cases_TX_select, FUN = class)
cases_TX_select[] <- lapply(cases_TX_select, FUN = as.numeric)
head(cases_TX_select)

cor_TX <- cor(cases_TX_select)
ggcorrplot(cor_TX, 
           type = "lower",
           p.mat = cor_pmat(cases_TX_select), 
           hc.order = TRUE, 
           lab = TRUE, 
           lab_size = 3)
# round(cor(cases_TX_select$median_income, cases_TX_select$deaths), digits =  2)

# ------------------------------------------------------------------------------
# ----------------------------------  map graphs   ---------------------------------
# ------------------------------------------------------------------------------
library("maps")

head(sub)
# change county name from DeWitt to De Witt to fix map issue
sub$county_name[sub$county_name=="DeWitt County" | 
                  sub$county_name=="Dewitt County" ] <- "De witt County"

# get all counties in US
co <- as_tibble(map_data("county"))
# get a subset of counties in Texas
counties_TX <- subset(co, region == "texas") %>% rename(c(county = subregion))
# create new dataframe of sub but change format of county name ("Dallas County" to "dallas")
cases_TX_co <- sub %>% mutate(county = county_name %>% str_to_lower() %>% 
                                str_replace('\\s+county\\s*$', ''))
# join texas county information with cases_TX_co dataframe on county
counties_TX <- counties_TX %>% left_join(cases_TX_co %>% 
                                           select(c(county, total_pop, median_age)))

# plot total population in each county
ggplot(counties_TX, aes(long, lat, label = county)) + 
  geom_polygon(aes(group = group, fill = total_pop)) +
  coord_quickmap() + 
  scale_fill_gradient(low="blue", high="red") +
  labs(title = "Total Population per County in Texas", 
       x="Longitude", y="Latitude")

# plot median age in each county
ggplot(counties_TX, aes(long, lat, label = county)) + 
  geom_polygon(aes(group = group, fill = median_age)) +
  coord_quickmap() + 
  scale_fill_gradient(low="blue", high="red") +
  labs(title = "Median Age per County in Texas", 
       x="Longitude", y="Latitude")

#==== Adding up all cases and deaths in each of the counties on each date =====
i <- 1
texas <- data.frame(date = character(0), confirmed_cases = numeric(0), deaths = numeric(0))
dates <- unique(cases_TX$date)
while(i <= length(dates)){
  texas[nrow(texas)+1, ] <- c(as.character(dates[i]), 
                              sum(subset(cases_TX, date == dates[i])$confirmed_cases), 
                              sum(subset(cases_TX, date == dates[i])$deaths)
  )
  i <- i + 1
}
texas$date <- ymd(texas$date)
texas$confirmed_cases <- as.numeric(texas$confirmed_cases)
texas$deaths <- as.numeric(texas$deaths)
lapply(texas, class)
texas

# sources - https://www.dshs.state.tx.us/coronavirus/execorders.aspx
# https://www.texastribune.org/2020/07/31/coronavirus-timeline-texas/
data_events <- data.frame(date = c("2020-03-05", "2020-03-26", "2020-03-31", 
                                   "2020-04-03", "2020-05-01", "2020-07-02", 
                                   "2020-09-17", "2020-10-8"),
                          name = c("First Confirmed\nCase in Texas",
                                   "Self-Quaratine\nRules for Travelers", 
                                   "Stay-at-Home Order,\nSocial Distancing,\nEssential Services Only",
                                   "CDC Recommends\nUse of Face Coverings",
                                   "Re-opening Services\nto 25%",
                                   "Start of Mask Mandate,\nLimit on Group Sizes", 
                                   "\nRe-opening Services\nto 50%",
                                   "\nRe-opening Services\nto 75%"
                          ),
                          ystart = c(70000, 200000, 400000, 700000, 900000, 1000000, 1250000, 1500000)
)
i <- 1
cases <- vector()
while(i <= length(data_events$date)){
  cases <- append(cases, texas$confirmed_cases[texas$date == data_events$date[i]])
  i <- i + 1
}
data_events <- cbind(data_events, cases)
data_events$date <- ymd(data_events$date)
data_events$yend <- data_events$ystart * 1.5
data_events

# time series plot of confirmed cases and deaths in Texas
ggplot() + 
  geom_line(data=texas, mapping=aes(x = date, y = confirmed_cases), linetype="solid", color = "steelblue") + 
  geom_line(data=texas, mapping=aes(x = date, y = deaths), linetype="solid", color="darkred") + labs(x="Date", y="Count") +
  geom_segment(data=data_events, mapping=aes(x = date, y = cases, xend = date, yend = yend), color = "black") +
  geom_text(data=data_events, mapping=aes(x = date, y = yend * 1.15, label = name), color = "black")+
  scale_x_date(date_breaks = "1 month", date_labels = "%b-%y") + 
  labs(title="Number of Confirmed COVID-19 Cases (Blue) and Deaths (Red) in Texas", 
       subtitle="With Major Events From March 5, 2020 to January 25, 2021")

#===============================================================================

#============ Correlation between median_age and deaths_per_1000 ===============

# cumulative number of cases/deaths at 01/25/21 for each county
dates <- unique(cases_TX$date)
sub <- subset(cases_TX, date == dates[length(dates)])
# add populations
sub <- merge(sub, subset(cases_TX_census, select=c("total_pop", "county_name", 
                                                   "median_age", "median_income")), 
             by="county_name")
sub
# add cases per 1000 and deaths per 1000 columns
cases_per_1000 <- (sub$confirmed_cases/sub$total_pop) * 1000
sub <- cbind(sub, cases_per_1000)
deaths_per_1000 <- (sub$deaths/sub$total_pop) * 1000
sub <- cbind(sub, deaths_per_1000)
sub

library(ggrepel)
ggplot(sub, aes(x=median_age, y=deaths_per_1000, label=county_name)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_ipsum() + 
  geom_text_repel(data = subset(sub, deaths_per_1000 > quantile(deaths_per_1000, .96)),
                  size = 3.5,
                  max.overlaps = Inf,
                  box.padding = 2,
                  nudge_y = 1,
                  segment.curvature = -0.1,
                  direction = "x",
                  ylim = c(NA, 7),
                  vjust=0.5,)+
  ggtitle("Median Age vs. Deaths per 1000") +
  xlab("Median Age") + ylab("Deaths per 1000") + 
  labs(subtitle="Counties in the 96th Quantile of the Deaths per 1000 are Labeled")
round(cor(sub$median_age, sub$deaths_per_1000), digits=2)

ggplot(sub, aes(x=median_income, y=deaths_per_1000, label=county_name)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_ipsum() + 
  geom_text_repel(data = subset(sub, deaths_per_1000 > quantile(deaths_per_1000, .96)),
                  size = 3.5,
                  max.overlaps = Inf,
                  box.padding = 2,
                  nudge_y = 1,
                  segment.curvature = -0.1,
                  direction = "x",
                  ylim = c(NA, 7),
                  vjust=0.5,)+
  ggtitle("Median Income vs. Deaths per 1000") +
  xlab("Median Income") + ylab("Deaths per 1000") + 
  labs(subtitle="Counties in the 96th Quantile of the Deaths per 1000 are Labeled")
round(cor(sub$median_income, sub$deaths_per_1000), digits=2)

ggplot(sub, aes(x=median_income, y=median_age)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_ipsum()
round(cor(sub$median_income, sub$median_age), digits=2)


#===============================================================================

#================ Find the rate of spread in each county =======================
dates <- unique(cases_TX$date)
counties <- sub$county_name

date_range <- as.numeric(dates[length(dates)] - dates[1])
i <- 1
spread_rate <- vector()
while(i <= length(counties)){
  # spread rate equals the difference in the number of confirmed cases on the first available date
  # and the last available date divided by the number of days seperating those dates (ie. slope)
  spread_rate <- append(spread_rate, 
                        (sub$confirmed_cases[sub$county_name == counties[i]] -
                           cases_TX$confirmed_cases[cases_TX$date==dates[1] & cases_TX$county_name == counties[i]])
                        / date_range
  )
  i <- i + 1
}
spread_rate

sub <- cbind(sub, spread_rate)
sub

# find the correlation between the population size of each county and 
# the rate of spread of the virus

ggplot(sub, aes(x=total_pop, y=spread_rate, label=county_name)) +
  geom_point() +
  geom_smooth(method=lm , color="red", fill="#69b3a2", se=TRUE) +
  theme_ipsum() + 
  geom_text_repel(data = subset(sub, spread_rate > quantile(spread_rate, .95)),
                  size = 3,
                  max.overlaps = Inf,
                  box.padding = 2,
                  ylim = c(300, NA),
                  direction = "both"
  )+
  ggtitle("Total Population vs. Spread Rate") +
  xlab("Total Population") + ylab("Spread Rate") + 
  labs(subtitle="Counties in the 95th Quantile of Spread Rate are Labeled")
round(cor(sub$total_pop, sub$spread_rate), digits=2)


#===============================================================================

#======================== Spread Rate on Texas Map =============================
library(dplyr)
# change county name from DeWitt to De Witt to fix map issue
sub$county_name[sub$county_name=="DeWitt County" | 
                  sub$county_name=="Dewitt County" ] <- "De witt County"

# get all counties in US
co <- as_tibble(map_data("county"))
# get a subset of counties in Texas
counties_TX <- subset(co, region == "texas") %>% rename(c(county = subregion))
# create new dataframe of sub but change format of county name ("Dallas County" to "dallas")
cases_TX_co <- sub %>% mutate(county = county_name %>% str_to_lower() %>% 
                                str_replace('\\s+county\\s*$', ''))
# join texas county information with cases_TX_co dataframe on county
counties_TX <- counties_TX %>% left_join(cases_TX_co %>% 
                                           select(c(county, cases_per_1000, deaths_per_1000, spread_rate)))

# plot spread rate in each county
ggplot(counties_TX, aes(long, lat, label = county)) + 
  geom_polygon(aes(group = group, fill = spread_rate)) +
  coord_quickmap() + 
  scale_fill_gradient(low="blue", high="red") +
  labs(title = "Spread Rate for Each County in Texas", 
       subtitle = "Spread Rate = (Cases as of Jan 25, 2021 - Cases as of Mar 5, 2020)/Total Time in Days",
       x="Longitude", y="Latitude")
#===============================================================================

#======================== Fatality Rate on Texas Map =============================
library(dplyr)
# change county name from DeWitt to De Witt to fix map issue
sub$county_name[sub$county_name=="DeWitt County" | 
                  sub$county_name=="Dewitt County" ] <- "De witt County"

fatality_rate <- sub$deaths / sub$confirmed_cases
sub <- cbind(sub, fatality_rate)
sub
colnames(sub)
# get all counties in US
co <- as_tibble(map_data("county"))
# get a subset of counties in Texas
counties_TX <- subset(co, region == "texas") %>% rename(c(county = subregion))
# create new dataframe of sub but change format of county name ("Dallas County" to "dallas")
cases_TX_co <- sub %>% mutate(county = county_name %>% str_to_lower() %>% 
                                str_replace('\\s+county\\s*$', ''))
# join texas county information with cases_TX_co dataframe on county
counties_TX <- counties_TX %>% left_join(cases_TX_co %>% 
                                           select(c(county, cases_per_1000, deaths_per_1000, spread_rate, fatality_rate)))

# plot spread rate in each county
ggplot(counties_TX, aes(long, lat, label = county)) + 
  geom_polygon(aes(group = group, fill = fatality_rate)) +
  coord_quickmap() + 
  scale_fill_gradient(low="blue", high="red") +
  labs(title = "Fatality Rate for Each County in Texas", 
       subtitle = "Fatality Rate = Deaths as of Jan 25, 2021 / Confirmed Cases as of Jan 25, 2021",
       x="Longitude", y="Latitude")
#===============================================================================

#================== First Confirmed Case on Texas Map ==========================

first_case <- subset(cases_TX, confirmed_cases > 0)
first_case <- first_case[match(unique(first_case$county_name), first_case$county_name),]
length(first_case$county_name)
first_case <- subset(first_case, select=c("county_name", "date")) 
colnames(first_case) <- c("county_name", "first_case_date")
first_case$county_name
first_case$county_name[first_case$county_name=="DeWitt County" | 
                         first_case$county_name=="Dewitt County" ] <- "De witt County"
first_case
sub <- merge(sub, first_case, by="county_name")
sub
# get all counties in US
co <- as_tibble(map_data("county"))
# get a subset of counties in Texas
counties_TX <- subset(co, region == "texas") %>% rename(c(county = subregion))
# create new dataframe of sub but change format of county name ("Dallas County" to "dallas")
cases_TX_co <- sub %>% mutate(county = county_name %>% str_to_lower() %>% 
                                str_replace('\\s+county\\s*$', ''))
# join texas county information with cases_TX_co dataframe on county
counties_TX <- counties_TX %>% left_join(cases_TX_co %>% 
                                           select(c(county, first_case_date)))

# plot spread rate in each county
ggplot(counties_TX, aes(long, lat, label = county)) + 
  geom_polygon(aes(group = group, fill = first_case_date)) +
  coord_quickmap() +
  scale_fill_gradient(low="blue", high="red", 
                      labels=as.Date(c("2020-03-05", "2020-07-11", "2020-11-17")), 
                      breaks=as.Date(c("2020-03-05", "2020-07-11", "2020-11-17")), 
                      limits=as.Date(c("2020-03-05","2020-11-17"))) +
  labs(title = "First Case Date for Each County in Texas", 
       subtitle = "Date of Each County's First Confirmed Case (Ranges from March 5, 2020 to November 17, 2020)",
       x="Longitude", y="Latitude") 


#===============================================================================

