#SEAK Dungeness crab harvest forecast model


source("./r/helper.R")

# global ---------
cur_yr = 2023
year <- 2023 # most recent year of data
fig_path <- paste0('figures/', year) # folder to hold all figs for a given year
dir.create(fig_path) # creates year subdirectory inside figures folder
output_path <- paste0('output/', year) # output and results
dir.create(output_path)

#load data-----

#Dungeness fishticket harvest pivot table from OceanAK
read.csv("data/fishery/dung fishticket summary 85-22.csv") %>%
  clean_names() -> harv_data

#fix season ref from character to numeric
harv_data %>%
  mutate(season_num = as.numeric((str_sub(season, -12, -9)))) -> harv_data1

#create table for first 7 days of harvest----

#anydate converts character date into a date variable
harv_data1$date_of_landing_mm_dd <- anydate(harv_data1$date_of_landing_mm_dd)

#Extract months into data column
harv_data1$month <- month(ymd(harv_data1$date_of_landing_mm_dd))

write.csv(harv_data1, paste0('./output/', cur_yr,'/dung_harvest_data.csv'))


#create date range to capture the first seven days of harvest
start_date <- ymd("1400-06-15")

end_date <- ymd("1400-06-22")

range <- seq(start_date, end_date, "days")

#check date range. year does not matter as the season dates remain the same for each year

print(range)

harv_data1 %>%
  filter(date_of_landing_mm_dd %in% range) -> dung_harv_sevendays

write.csv(dung_harv_sevendays, paste0('./output/', cur_yr,'/dung_harvest_sevendays.csv'))


harv_data1 %>%
  filter(!is.na(pounds),
         !is.na(amount_sum),
                date_of_landing_mm_dd %in% range) %>%
  group_by(season_num) %>%
  summarise(total_sevendays_lbs = sum(pounds),
            sevendays_number_of_permits = sum(permits),
            total_value = sum(amount_sum),
            price_lb = total_value/total_sevendays_lbs,
            total_landings = sum(landings),
            cpue = total_sevendays_lbs/total_landings) -> harv_sevendays

write.csv(harv_sevendays, paste0('./output/', cur_yr,'/dung_harvest_sevendays_summary.csv'))
           

#harvest summary for total season harvest (i.e. summer, fall, & winter)
harv_data1 %>%
  filter(!is.na(pounds),
         season_num != cur_yr) %>% #need to remove current year for harvest forecasting
  group_by(season_num) %>%
  summarise(total_lbs = sum(pounds)) -> harv_summary

#join tables to reflect first seven days of harvest to total harvest
dung_summary_84 <- data.frame(season_num = 1984,
                              total_sevendays_lbs = 128131,
                              sevendays_number_of_permits = 58,
                              total_lbs = 1838699,
                              harv_remaining = 1710568)

left_join(harv_sevendays, 
          harv_summary,
          by = "season_num") -> harv_join

bind_rows(dung_summary_84, harv_join) %>% 
  mutate(harv_remaining = total_lbs - total_sevendays_lbs,
         previous_year_proportion = c(lag(total_sevendays_lbs)/lag(total_lbs, n=1))) -> full_harv_summary  

#Remove 1984 due to NA value for prediction and current year as current year data is used to forecast
full_harv_summary %>%
  filter(season_num > 1984 & season_num < cur_yr) %>%
  dplyr::select(season_num, total_lbs, total_sevendays_lbs,
         previous_year_proportion, sevendays_number_of_permits, 
         total_landings, total_value,
         cpue, price_lb)-> harv_model_data

write.csv(harv_model_data, paste0('./output/', cur_yr,'/dung_model_data.csv'))


#Compare variables for correlation
harv_model_data %>%
  dplyr::select(-season_num) %>%
  ggpairs()

#Correlation matrix to compare variables----
harv_model_data %>%
  dplyr::select(-season_num) -> harv_model_data2
  
corrplot(cor(harv_model_data2), method = "number")


#Stepwise Linear Regression model selection----

#define intercept only model
intercept_only <-lm(total_lbs ~ 1, data = harv_model_data)

#define model with all predictors
harv_model_data %>%
  select(-season_num) -> harv_model_data2
all <- lm(total_lbs ~ ., data = harv_model_data2)

#perform forward and backwards stepwise regression
#if wanting to switch change formular to "forward" or "backward"
both <- step(all,
                direction = "both",
                scope = formula(all),
                trace = 0)
#view results
both$anova

#view final model
both$coefficients


#Create final model for forecasting----
#Model 23----
dung_model_23 <-lm(total_lbs ~ previous_year_proportion +
                     total_sevendays_lbs + cpue,
                   data = harv_model_data)

summary(dung_model_23)


#Grab current year info only to forecast based on best regression model
full_harv_summary %>%
  filter(season_num == cur_yr) %>%
  dplyr::select(previous_year_proportion,
         total_sevendays_lbs,
         cpue) -> harv_forecast_data

model_values <- data.frame(actual = harv_model_data$total_lbs,
                           predicted = predict(dung_model_23, interval = "confidence"),
                           x = harv_model_data$season_num)

model_values %>%
  gather(data_source,
         biomass,
         actual:predicted.fit,
         factor_key = TRUE) -> model_values

model_values %>%
  mutate(data_source = ifelse(data_source == "predicted.fit", 
                              "predicted", "actual")) -> model_values



forecast_values <- data.frame(x = 2022,
                              data_source = "predicted",
                              biomass = predict(dung_model_23,
                                                newdata = harv_forecast_data,
                                                interval = "confidence")) 
forecast_values %>%
  rename("biomass" = "biomass.fit",
         "predicted.lwr" = "biomass.lwr",
         "predicted.upr" = "biomass.upr") -> forecast_values


bind_rows(model_values, forecast_values) %>% 
  mutate(data_source = ifelse(data_source == "actual", "actual",
                              ifelse(data_source == "predicted", "23", "NONE"))) -> dung_forecast23

#Plot forecast model compared to observed model
dung_forecast23 %>%
  ggplot(aes(x, biomass, 
             color = data_source,
             fill = data_source)) +
  geom_line(lwd = 0.5) +
  geom_point(size = 3) +
  geom_hline(yintercept = 1500000,
             color = "red",
             lwd = 0.5) +
  geom_hline(yintercept = 2250000,
             color = "green",
             lwd = 0.5) +
  geom_ribbon(aes(ymin = predicted.lwr,
                  ymax = predicted.upr),
              alpha = 0.2,
              color = NA) +
  scale_x_continuous(breaks = seq(1985, 2022, 3), 
                     name = "Season") + 
  scale_y_continuous(breaks = seq(0, 10000000, 500000),
                     labels = scales::comma,
                     name = "Harvest (lbs)")

#Check assumptions and outliers of preferred model----

hist(residuals(dung_model_23))

plot(fitted(dung_model_23), 
     residuals(dung_model_23))

abline(h = 0, lty = 2)

autoplot(dung_model_23)

#identify influential outliers
cooksD <- cooks.distance(dung_model_23)

n <- nrow(harv_model_data)
plot(cooksD, main = "Cook's Distance for Influential Obs")
abline(h = 4/n, lty = 2, col = "steelblue")

influential_obs <- as.numeric(names(cooksD)[(cooksD > (4/n))])

outliers_removed <- harv_model_data[-influential_obs, ]

#Model 23a----
dung_model_23a <-lm(total_lbs ~ previous_year_proportion +
                      total_sevendays_lbs + cpue,
                    data = outliers_removed)

summary(dung_model_23a)

#Grab current year info only to forecast based on best regression model
full_harv_summary %>%
  filter(season_num == cur_yr) %>%
  select(previous_year_proportion,
         total_sevendays_lbs,
         cpue) -> harv_forecast_data

model_values <- data.frame(actual = outliers_removed$total_lbs,
                           predicted = predict(dung_model_23a, interval = "confidence"),
                           x = outliers_removed$season_num)

model_values %>%
  gather(data_source,
         biomass,
         actual:predicted.fit,
         factor_key = TRUE) -> model_values

model_values %>%
  mutate(data_source = ifelse(data_source == "predicted.fit", 
                              "predicted", "actual")) -> model_values



forecast_values <- data.frame(x = 2022,
                              data_source = "predicted",
                              biomass = predict(dung_model_23a,
                                                newdata = harv_forecast_data,
                                                interval = "confidence")) 
forecast_values %>%
  rename("biomass" = "biomass.fit",
         "predicted.lwr" = "biomass.lwr",
         "predicted.upr" = "biomass.upr") -> forecast_values


bind_rows(model_values, forecast_values) %>% 
  mutate(data_source = ifelse(data_source == "actual", "actual",
                                     ifelse(data_source == "predicted", "23a", "NONE"))) -> dung_forecast23a


#Plot forecast model compared to observed model
dung_forecast23a %>%
  ggplot(aes(x, biomass, 
             color = data_source,
             fill = data_source)) +
  geom_line(lwd = 0.5) +
  geom_point(size = 3) +
  geom_hline(yintercept = 1500000,
             color = "red",
             lwd = 0.5) +
  geom_hline(yintercept = 2250000,
             color = "green",
             lwd = 0.5) +
  geom_ribbon(aes(ymin = predicted.lwr,
                  ymax = predicted.upr),
              alpha = 0.2,
              color = NA) +
  scale_x_continuous(breaks = seq(1985, 2022, 3), 
                     name = "Season") + 
  scale_y_continuous(breaks = seq(0, 10000000, 500000),
                     labels = scales::comma,
                     name = "Harvest (lbs)")


#Check previous iterations of the model and compare to new preferred model

#Model 21 original----
#Create mutiple linear regression models with covariates
dung_model_21 <- lm(total_lbs ~ total_sevendays_lbs + 
     sevendays_number_of_permits + 
     previous_year_proportion,
   data = harv_model_data)

summary(dung_model_21) 


#Grab current year info only to forecast based on best regression model
full_harv_summary %>%
  filter(season_num == cur_yr) %>%
  select(previous_year_proportion,
         total_sevendays_lbs,
         sevendays_number_of_permits,
         cpue) -> harv_forecast_data

model_values <- data.frame(actual = harv_model_data$total_lbs,
                           predicted = predict(dung_model_21, interval = "confidence"),
                           x = harv_model_data$season_num)

model_values %>%
  gather(data_source,
         biomass,
         actual:predicted.fit,
         factor_key = TRUE) -> model_values

model_values %>%
  mutate(data_source = ifelse(data_source == "predicted.fit", 
                              "predicted", "actual")) -> model_values



forecast_values <- data.frame(x = 2022,
                              data_source = "predicted",
                              biomass = predict(dung_model_21,
                                                newdata = harv_forecast_data,
                                                interval = "confidence")) 
forecast_values %>%
  rename("biomass" = "biomass.fit",
         "predicted.lwr" = "biomass.lwr",
         "predicted.upr" = "biomass.upr") -> forecast_values


bind_rows(model_values, forecast_values) %>% 
  mutate(data_source = ifelse(data_source == "actual", "actual",
                              ifelse(data_source == "predicted", "21", "NONE"))) -> dung_forecast21


#Model 22 status quo----
#removes number of permit holders
dung_model_22 <- lm(total_lbs ~ total_sevendays_lbs +
                    previous_year_proportion,
                  data = harv_model_data)

summary(dung_model_22)


#Grab current year info only to forecast based on best regression model
full_harv_summary %>%
  filter(season_num == cur_yr) %>%
  select(previous_year_proportion,
         total_sevendays_lbs,
         cpue) -> harv_forecast_data

model_values <- data.frame(actual = harv_model_data$total_lbs,
                           predicted = predict(dung_model_22, interval = "confidence"),
                           x = harv_model_data$season_num)

model_values %>%
  gather(data_source,
         biomass,
         actual:predicted.fit,
         factor_key = TRUE) -> model_values

model_values %>%
  mutate(data_source = ifelse(data_source == "predicted.fit", 
                              "predicted", "actual")) -> model_values



forecast_values <- data.frame(x = 2022,
                              data_source = "predicted",
                              biomass = predict(dung_model_22,
                                                newdata = harv_forecast_data,
                                                interval = "confidence")) 
forecast_values %>%
  rename("biomass" = "biomass.fit",
         "predicted.lwr" = "biomass.lwr",
         "predicted.upr" = "biomass.upr") -> forecast_values


bind_rows(model_values, forecast_values) %>% 
  mutate(data_source = ifelse(data_source == "actual", "actual",
                              ifelse(data_source == "predicted", "22", "NONE"))) -> dung_forecast22


#Compare models using AIC----
#AIC tests
models <- list(dung_model_23,
               dung_model_23a,
               dung_model_21,
               dung_model_22)


model.names <- (c('model.23', 'model.23a', 'model.21', 'model.22'))

aictab(cand.set = models, modnames = model.names)


#AIC tests
models <- list(dung_model_22, 
               dung_model_22.1,
               dung_model_22.2,
               dung_model_23, 
               dung_model_23a, 
               dung_model_24)


model.names <- (c('model.22', 'model.22.1', 'model.22.2', 'model.23', 'model.23a', 'model.24'))

aictab(cand.set = models, modnames = model.names)

#Combine model forecast tables for graphing----
bind_rows(dung_forecast21, 
          dung_forecast22,
          dung_forecast23, 
          dung_forecast23a) -> forecast_summary

target <- c("actual", "23", "23a")


forecast_summary %>%
  filter(data_source %in% target) %>%
  ggplot(aes(x, biomass, 
             color = data_source,
             fill = data_source)) +
  geom_line(lwd = 0.5) +
  geom_point(size = 3) +
  geom_hline(yintercept = 1500000,
             color = "red",
             lwd = 0.5) +
  geom_hline(yintercept = 2250000,
             color = "green",
             lwd = 0.5) +
  geom_ribbon(aes(ymin = predicted.lwr,
                  ymax = predicted.upr),
              alpha = 0.1,
              color = NA) +
  scale_x_continuous(breaks = seq(1985, 2022, 3), 
                     name = "Season") + 
  scale_y_continuous(breaks = seq(0, 10000000, 500000),
                     labels = scales::comma,
                     name = "Harvest (lbs)") 


ggsave(paste0(fig_path, '/dung_forecast_models_85-22.png'), 
       width = 10, height = 5, units = "in", dpi = 200)


#Model 24 ----
#adds weighting to permit type
#standardize mgt data to reflect tiered pot system which went into effect in 1997

#fix season ref from character to numeric
seak_permits <- c("D9DA", "D9CA", "D9BA", "D9AA")

harv_data %>%
  mutate(season_num = as.numeric((str_sub(season, -12, -9))),
         wgt_permit = ifelse(cfec_fishery_code == "D9DA", 1/75,
                           ifelse(cfec_fishery_code == "D9CA", 1/150,
                                  ifelse(cfec_fishery_code == "D9BA", 1/225,
                                         ifelse(cfec_fishery_code == "D9AA", 1/300, NA))))) %>%
  filter(cfec_fishery_code %in% seak_permits,
         season_num >= 1997)-> harv_data1

#create table for first 7 days of harvest

#anydate converts character date into a date variable
harv_data1$date_of_landing_mm_dd <- anydate(harv_data1$date_of_landing_mm_dd)

#create date range to capture the first seven days of harvest
start_date <- ymd("1400-06-15")

end_date <- ymd("1400-06-22")

range <- seq(start_date, end_date, "days")

#check date range. year does not matter as the season dates remain the same for each year

print(range)


harv_data1 %>%
  filter(!is.na(pounds),
         !is.na(amount_sum),
         date_of_landing_mm_dd %in% range) %>%
  group_by(season_num) %>%
  summarise(total_sevendays_lbs = sum(pounds),
            sevendays_permits_wgt = sum(wgt_permit),
            total_value = sum(amount_sum),
            price_lb = total_value/total_sevendays_lbs,
            total_landings = sum(landings),
            cpue = total_sevendays_lbs/total_landings) -> harv_sevendays



#harvest summary for total season harvest (i.e. summer, fall, & winter)
harv_data1 %>%
  filter(!is.na(pounds),
         season_num != cur_yr) %>% #need to remove current year for harvest forecasting
  group_by(season_num) %>%
  summarise(total_lbs = sum(pounds)) -> harv_summary

#join tables to reflect first seven days of harvest to total harvest

left_join(harv_sevendays, 
          harv_summary,
          by = "season_num") -> harv_join

harv_join %>% 
  mutate(harv_remaining = total_lbs - total_sevendays_lbs,
         previous_year_proportion = c(lag(total_sevendays_lbs)/lag(total_lbs, n=1))) -> full_harv_summary  

#Remove 1996 due to NA value for prediction and current year as current year data is used to forecast
full_harv_summary %>%
  filter(season_num > 1997 & season_num < cur_yr) %>%
  select(season_num, total_lbs, total_sevendays_lbs,
         previous_year_proportion, cpue, price_lb, sevendays_permits_wgt)-> harv_model_data

#Correlation matrix to compare variables----
harv_model_data %>%
  select(-season_num) -> harv_model_data2

corrplot(cor(harv_model_data2), method = "number")


#Stepwise Linear Regression model selection----

#define intercept only model
intercept_only <-lm(total_lbs ~ 1, data = harv_model_data)

#define model with all predictors
harv_model_data %>%
  select(-season_num) -> harv_model_data2
all <- lm(total_lbs ~ ., data = harv_model_data2)

#perform forward and backwards stepwise regression
#if wanting to switch change formular to "forward" or "backward"
both <- step(all,
             direction = "both",
             scope = formula(all),
             trace = 0)
#view results
both$anova

#view final model
both$coefficients

dung_model_24 <- lm(total_lbs ~ total_sevendays_lbs +
                      previous_year_proportion +
                     cpue, 
                 data = harv_model_data)

summary(dung_model_24)    


full_harv_summary %>%
  filter(season_num == cur_yr) %>%
  select(previous_year_proportion,
         total_sevendays_lbs,
         cpue) -> harv_forecast_data

model_values <- data.frame(actual = harv_model_data$total_lbs,
                           predicted = predict(dung_model_24, interval = "confidence"),
                           x = harv_model_data$season_num)

model_values %>%
  gather(data_source,
         biomass,
         actual:predicted.fit,
         factor_key = TRUE) -> model_values

model_values %>%
  mutate(data_source = ifelse(data_source == "predicted.fit", 
                              "predicted", "actual")) -> model_values



forecast_values <- data.frame(x = 2022,
                              data_source = "predicted",
                              biomass = predict(dung_model_24,
                                                newdata = harv_forecast_data,
                                                interval = "confidence")) 
forecast_values %>%
  rename("biomass" = "biomass.fit",
         "predicted.lwr" = "biomass.lwr",
         "predicted.upr" = "biomass.upr") -> forecast_values


bind_rows(model_values, forecast_values) %>% 
  mutate(data_source = ifelse(data_source == "actual", "actual",
                              ifelse(data_source == "predicted", "24", "NONE"))) -> dung_forecast24

#Plot forecast model compared to observed model
dung_forecast24 %>%
  ggplot(aes(x, biomass, 
             color = data_source,
             fill = data_source)) +
  geom_line(lwd = 0.5) +
  geom_point(size = 3) +
  geom_hline(yintercept = 1500000,
             color = "red",
             lwd = 0.5) +
  geom_hline(yintercept = 2250000,
             color = "green",
             lwd = 0.5) +
  geom_ribbon(aes(ymin = predicted.lwr,
                  ymax = predicted.upr),
              alpha = 0.1,
              color = NA) +
  scale_x_continuous(breaks = seq(1985, 2022, 3), 
                     name = "Season") + 
  scale_y_continuous(breaks = seq(0, 10000000, 500000),
                     labels = scales::comma,
                     name = "Harvest (lbs)")



#Leslie depletion methods to estimate abundance-----
#Model 25----
harv_data1 %>%
  filter(!is.na(pounds),
         effort_sum > 0,
         date_of_landing_mm_dd %in% range,
         season_num == 2022) %>%
  group_by(season_num,
           date_of_landing_mm_dd) %>%
  summarise(total_sevendays_lbs = sum(pounds),
            total_effort = sum(effort_sum)) -> harv_sevendays
  

#Leslie Deption model per FSA package (https://fishr-core-team.github.io/FSA/reference/depletion.html)

l1 <- depletion(harv_sevendays$total_sevendays_lbs,
                harv_sevendays$total_effort,
                method = "Leslie",
                Ricker.mod = FALSE)

summary(l1)

summary(l1, verbose = TRUE)

summary(l1, parm = "No")

rSquared(l1)

rSquared(l1, digits = 1, percent = TRUE)

cbind(Est = coef(l1), confint(l1))

summary(l1, parm = "lm")

plot(l1)


#Dungeness Crab Forecasting-----
#Uses text by Elizabeth Holmes (https://fish-forecast.github.io/Fish-Forecast-Bookdown/index.html)

#Check model data frame
#Remove season
summary(harv_model_data)


#Correlation matrix of variables
corrplot(cor(harv_model_data))

corrplot(cor(harv_model_data), method = "number")

ggpairs(harv_model_data)

#Use variance inflation factors to check for collinearity
full <- lm(total_lbs ~., data = harv_model_data)
car::vif(full)


olsrr::ols_vif_tol(full)

#Condition indicies
model <- lm(total_lbs ~ ., data= harv_model_data)
round(olsrr::ols_eigen_cindex(model), digit=2)

#Check for variables that are redundant
a <- Hmisc::redun(~ ., data = harv_model_data[, -1], nk = 0)

a$out

#Relative importance metrics
#Groemping, U. (2006) Relative Importance for Linear Regression in R: 
#The Package relaimpo Journal of Statistical Software 17, Issue 1. 
#Downloadable at http://www.jstatsoft.org/v17/i01↩︎
library(relaimpo)

reli <- relaimpo::calc.relimp(total_lbs~.,data=harv_model_data)
plot(reli)

#Choose model w/o collinearity issues
harv_model_data %>% 
  dplyr::select(season_num, total_lbs, cpue) -> harv_model_data2

cpue_model <- lm(total_lbs ~ season_num + cpue, data = harv_model_data)

#Stepwise Linear Regression Model Selection

null <- lm(total_lbs ~ 1, data = harv_model_data2)

full <- lm(total_lbs ~ ., data = harv_model_data2)

step.full <- step(null,
                  scope = list(lower = null, upper = full),
                  direction = "both", trace = 0)

step.full

#olsrr package stepwise model selection with different metrics

ols_step_best_subset(full)

#Durbin-Watson test for autocorrelation
car::durbinWatsonTest(cpue_model)

#Breusch-Godfrey test to check residuals
forecast::checkresiduals(cpue_model)

#ARIMA models
#have to make data into a timeseries object
harv_model_data2 %>%
  dplyr::select(total_lbs, cpue) -> harv_model_data3

dung85ts <- ts(harv_model_data3,
               start = 1985)

fit <- forecast::auto.arima(dung85ts)

fit



#ARIMA model with covariates
xreg <- as.matrix(harv_model_data2[,c("cpue")])

forecast::auto.arima(harv_model_data2$total_lbs, xreg = xreg,
                     trace = TRUE)


#ACF plot of residuals
lm(total_lbs ~ cpue, data = harv_model_data2) %>%
  resid %>%
  acf
    
#AR(2,0,2) model
xreg <- as.matrix(harv_model_data2[,c("cpue")])

fit <- arima(harv_model_data2$total_lbs, xreg=xreg, order = c(2,0,2))

fit

checkresiduals(fit)

#Forecasting

fit<- forecast::auto.arima(harv_model_data2$total_lbs, test = "adf")

fr <- forecast::forecast(fit, h = 5)

fr

plot(fr)

#specify cross-validation method
ctrl <- trainControl(method = "cv", 
                     number = 5, 
                     repeats = 20)

#fit a regression model and use k-fold CV to evaluate performance
harv_model_data %>%
  select(-season_num) -> harv_model_data2

model <- train(total_lbs ~ ., data = harv_model_data, 
               method = "lm",
               trControl = ctrl)

#view summary of CV
print(model)

#view final model
model$finalModel

model$results

coef(model$finalModel, id = 4)

#another try
set.seed(123)

data <- harv_model_data[ , c("total_lbs", "total_sevendays_lbs", 
                           "previous_year_proportion", "sevendays_number_of_permits",
                           "total_landings", "total_value", "cpue", "price_lb")]

train_control <- trainControl(method = "repeatedcv", number = 10, repeats = 20)



model <- train(total_lbs ~ ., data = data,
            method = "leapSeq",
            trControl = train_control)
print(model)

model$results

coef(model$finalModel, id = 4)
