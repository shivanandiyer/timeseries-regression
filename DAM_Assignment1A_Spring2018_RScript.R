###############################################################################
## DAM Assignment 1a
#  Linear Regression on Time Series Data
# Author - Shivanand Iyer(13293283)
###############################################################################


#--------------------------------------------------------------------------------------------------
# Load Libraries
#--------------------------------------------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(lubridate)
library(Metrics)
library(tidyverse)
library(data.table)
library(broom)
#--------------------------------------------------------------------------------------------------
# if working directory not set, set working Directory
# setwd("~/Documents/UTS/02_Courses/36106_DAM/Assignment/Assignment_1")

# Load Data File
transactions<-read.csv("transactions.csv",header=TRUE,stringsAsFactors = FALSE)

#--------------------------------------------------------------------------------------------------
# Perform Exploratory Data Analysis
#--------------------------------------------------------------------------------------------------
glimpse(transactions)

# Convert date column from character to Date
transactions$date <- dmy(transactions$date)

# Extract Year and Month from Date and store them as Year and Month
transactions$year <-year(transactions$date)
transactions$month <-month(transactions$date)

# Glimpse the data set and check the new variables added
glimpse(transactions)


# Checking Data distributions. Plotting the dataset for Range, outlier detection and missing values

summary(transactions)

ggplot(transactions%>%group_by(date,year,month,industry,location)%>%summarise(mean_monthly_amount=mean(monthly_amount)),aes(x=as.factor(month),y=log(mean_monthly_amount)))+geom_boxplot() + facet_wrap(~year) +ggtitle("Monthly Distribution of Transaction Data")  + labs(x="Month", y="Average Monthly Amount(Represented on a Log scale")

ggplot(transactions%>%group_by(date,year,month,industry,location)%>%summarise(mean_monthly_amount=mean(monthly_amount)),aes(x=as.factor(industry),y=log(mean_monthly_amount)))+geom_boxplot() + facet_wrap(~year) +ggtitle("Distribution of transaction data by Industry and Amount") + labs(x="Industry", y="Average Monthly Amount(Represented on a Log scale")

ggplot(transactions%>%group_by(date,year,month,industry,location)%>%summarise(mean_monthly_amount=mean(monthly_amount)),aes(x=as.factor(location),y=log(mean_monthly_amount)))+geom_boxplot() + facet_wrap(~year) +ggtitle("Distribution of data by Location and Amount")  + labs(x="Location", y="Average Monthly Amount(Represented on a Log scale")

# Identifying irregularities in the time series data set

head(transactions%>%group_by(industry,location)%>%summarise(cnt_distinctyear=n_distinct(year),count_distinctmonth=n_distinct(month))%>%filter(cnt_distinctyear < 3 | count_distinctmonth < 12))


#--------------------------------------------------------------------------------------------------
# Data Preparation
#--------------------------------------------------------------------------------------------------

# Aggregate the transactions data by date,industry and location and compute the mean of Monthly Amount
transactions_agg <- transactions%>%filter(industry==1,location==1)%>%group_by(date,year,month,industry,location)%>%summarise(mean_monthly_amount=mean(monthly_amount))

ggplot(transactions_agg,aes(x=as.factor(year),y=mean_monthly_amount))+geom_boxplot() + ggtitle("Data distribution for Industry/Location=1")

ggplot(transactions_agg,aes(x=as.factor(month),y=mean_monthly_amount))+geom_boxplot() + ggtitle("Monthly Data distribution for Industry/Location=1")

plot(density(transactions_agg$mean_monthly_amount),main="Density Plot: Monthly Amount")

ggplot(transactions_agg,aes(x=date,y=mean_monthly_amount))+geom_line() + geom_smooth(method='lm', se=FALSE) +ggtitle("Trend and Seasonality in the data")


#--------------------------------------------------------------------------------------------------
# Partitions - Split the data set into train and test(80-20% split)
#--------------------------------------------------------------------------------------------------

train_data_basic<-rbind(as_data_frame(transactions_agg%>%filter(year<=2015)),slice(as_data_frame(transactions_agg%>%filter(year>2015)),1))

test_data_basic<-suppressMessages(anti_join(transactions_agg,train_data_basic))

summary(train_data_basic$date)
summary(test_data_basic$date)


#--------------------------------------------------------------------------------------------------
# Basic model Fitting - Model 1(No seasonality)
#--------------------------------------------------------------------------------------------------

# Training the regression model
fmla=as.formula(mean_monthly_amount~year+month)
train_model_basic_1<-lm(fmla,data=train_data_basic)

# Analyse the model output
tidy(summary(train_model_basic_1))

# Check the Output Statistics for Model 1

glance(summary(train_model_basic_1))

#--------------------------------------------------------------------------------------------------
# Basic model Fitting - Model 2(With seasonality)
#--------------------------------------------------------------------------------------------------


# Convert Month to a factor(Dummy Variable) for seasonality
train_data_basic$month<-factor(train_data_basic$month)

# Training the regression model for Model 2
fmla=as.formula(mean_monthly_amount~year+month)
train_model_basic_2<-lm(fmla,data=train_data_basic)


# Analyse Model output
summary(train_model_basic_2)

# Plot the results
par(mfrow = c(2,2))
plot(train_model_basic_2)

# Fitting the model on Test Data Partition
test_data_basic$month<-factor(test_data_basic$month)
prediction<-predict(train_model_basic_2,test_data_basic)

# Store the predicted values in the test data partition data frame
test_data_basic$prediction<-prediction


# Plot Actual vs Predicted output

ggplot(test_data_basic,aes(x=prediction,y=mean_monthly_amount))+geom_point() + geom_abline(color = "blue") + ggtitle("Actual vs Predicted") + labs(x="Predicted",y="Actuals(Mean Monthly Amount")

# Check the predictions data
test_data_basic%>%ungroup%>%select(year,month,actual=mean_monthly_amount,prediction)

# Identify missing data/irregular spaced data
head(transactions%>%group_by(industry,location)%>%summarise(cnt_distinctyear=n_distinct(year),count_distinctmonth=n_distinct(month))%>%filter(cnt_distinctyear < 3 | count_distinctmonth < 12))%>%select(industry,location)


#--------------------------------------------------------------------------------------------------
# Advanced model Fitting - Using Model 2 on all data set(With seasonality)
#--------------------------------------------------------------------------------------------------

# Get distinct of all industry and locations in a dataframe
industry_location_pair<-transactions%>%group_by(industry,location)%>%summarise(cnt_year=n_distinct(year),cnt_month=n_distinct(month))%>%filter(cnt_month==12 & cnt_year>=3)
industry_location_pair$.id<-seq.int(nrow(industry_location_pair))


# Create Empty Lists for storing model Output
train_results_AdvFit <-list()
statistics_AdvFit<-list()
prediction_AdvFit<-list()
rmse_AdvFit<-c()

# Store the regression equation in a R formula variable - fmla 
fmla=as.formula(mean_monthly_amount~year+month)

# Loop for each industry and location
for(row in 1:nrow(industry_location_pair))
{
  
  # Aggregate data for each industry and location
  transactions_agg_AdvFit <- transactions%>%filter(industry==industry_location_pair[row,]$industry,location==industry_location_pair[row,]$location)%>%group_by(date,year,month,industry,location)%>%summarise(mean_monthly_amount=mean(monthly_amount))
  
  #Convert month into a dummy variable
  transactions_agg_AdvFit$month<-factor(transactions_agg_AdvFit$month)
  

  # Partition - Split data for training and test. Exclude data which are not time series 

  train_data_AdvFit<-rbind(as_data_frame(transactions_agg_AdvFit%>%filter(year<=2015)),slice(as_data_frame(transactions_agg_AdvFit%>%filter(year>2015)),1))
  test_data_AdvFit<-suppressMessages(anti_join(transactions_agg_AdvFit,train_data_AdvFit))
  
  # Train the model 
  train_model_AdvFit<-lm(fmla,data=train_data_AdvFit)
  
  # Store the model output into lists
  train_results_AdvFit[[row]]<-tidy(train_model_AdvFit)
  statistics_AdvFit[[row]]<-glance(train_model_AdvFit)
  
  
  ind_loc<-cbind(industry=test_data_AdvFit$industry,location=test_data_AdvFit$location)
  
  # Fitting the model on test data. Store the predicted values in a lit
  prediction_AdvFit[[row]]<-cbind(ind_loc,predicted=predict(train_model_AdvFit,test_data_AdvFit))
  
  # Calculate Root Mean Square Error
  rmse_AdvFit[[row]]<-rmse(test_data_AdvFit$mean_monthly_amount,predict(train_model_AdvFit,test_data_AdvFit))
}  
# Loop Ends here

# Convert the model Output from a list to a data frame
final_results_AdvFit<-rbindlist(train_results_AdvFit,fill=TRUE,idcol=TRUE)
final_statistics_AdvFit<-rbindlist(statistics_AdvFit,fill=TRUE,idcol=TRUE)


# Evaluate the Model Output
regression_result<-final_statistics_AdvFit%>%inner_join(industry_location_pair,by=".id")%>%group_by(.id)%>%arrange(adj.r.squared,desc(p.value))%>%select(.id,industry,location,r.squared,adj.r.squared,p.value,statistic,AIC,BIC,deviance,df.residual)

head(regression_result)

# Plot Adjusted R Squared Values for the model 
hist(regression_result$adj.r.squared)

# Plot P Value
plot(regression_result$p.value)
title(main = "P Value of co-efficients observed")


# Out of Sample Predictions for Dec 2016
df_dec2016<-data.frame(year=2016)
df_dec2016$month<-as.factor("12")
predict_dec2016<-predict(train_model_basic_2,df_dec2016)
predict_dec2016

# Detailed Model Evaluation Summary for every industry and location. To check all output remove "Head" 

head(final_results_AdvFit%>%inner_join(industry_location_pair,by=".id")%>%select(industry,location,term,estimate,std.error,statistic,p.value))

# Store RMSE Results and Analyse them
rmse<-as_data_frame(rmse_AdvFit)
rmse_results<-rmse %>% mutate(.id = row_number())%>%inner_join(industry_location_pair,by=".id")%>%select(industry,location,rmse=value)%>%arrange(desc(rmse))
head(rmse_results)
