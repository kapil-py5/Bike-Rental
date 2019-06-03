
# Bike Rental Project



rm(list = ls()) #remove all the stored data
setwd("C:\\Users\\I B BHATT\\Desktop\\machine learning\\edwisor-projects") #set working directory
getwd()
#Read the Data
bike_rental = read.csv('day.csv',sep = ',',row.names = 'instant')

#Dimensions
dim(bike_rental)

head(bike_rental)

# Summary
summary(bike_rental)

#Datatypes
str(bike_rental)

#Changing categorical variables to factors
for (i in seq(2, 8, by=1)){
  bike_rental[,i] = as.factor(bike_rental[,i])
}

str(bike_rental)
#Denormalizing data to get better Understanding

bike_rental$Actualtemp = bike_rental$temp*47 - 8
bike_rental$Actualatemp = bike_rental$atemp*66 - 16
bike_rental$Humidity = bike_rental$hum*67
bike_rental$Actualwindspeed = bike_rental$windspeed*100
bike_rental$Season_abbr = factor(x = bike_rental$season, levels = c(1,2,3,4), labels = c('Spring','Summer','Fall','Winter'))
bike_rental$Year = factor(x = bike_rental$yr, levels = c(0,1), labels = c(2011,2012))
bike_rental$WeatherType = factor(x = bike_rental$weathersit, levels = c(1,2,3,4), labels = c('Clear','Mist','Light Snow','Heavy Rain'))

head(bike_rental)

#null value analysis
apply(bike_rental,2,function(x) sum(is.na(x))) # 0 null values

#Univariate Analysis 
#Libraries for Plots 
library(ggplot2)
library(gplots)
library(dplyr)

#histogram
hist_temp = ggplot(data=bike_rental, aes(bike_rental$Actualtemp)) + geom_histogram(breaks=seq(0, 40, by=2),col='black',fill = 'blue') +
  labs(title="Histogram for Temperature", x="Temp", y="Count")
hist_atemp = ggplot(data=bike_rental, aes(bike_rental$Actualatemp)) + geom_histogram(breaks=seq(0, 60, by=2),col='black',fill = 'blue') +
  labs(title="Histogram for feeling Temperature", x="aTemp", y="Count")
hist_hum =  ggplot(data=bike_rental, aes(bike_rental$Humidity)) + geom_histogram(breaks=seq(0, 70, by=2),col='black',fill = 'blue') +
  labs(title="Histogram for Humidity", x="Humidity", y="Count")
hist_ws =  ggplot(data=bike_rental, aes(bike_rental$Actualwindspeed)) + geom_histogram(breaks=seq(0, 60, by=2),col='black',fill = 'blue') +
  labs(title="Histogram for Windspeed", x="windspeed", y="Count")
#Together just like we did with sub plot in python
gridExtra::grid.arrange(hist_temp,hist_atemp,hist_hum,hist_ws,ncol=2)

#bar graph
 bar_season = ggplot(bike_rental, aes(Season_abbr,cnt)) + geom_col() + labs(title="bargraph for Season", x="Season", y="Number of rentals")
 bar_year = ggplot(bike_rental, aes(Year,cnt)) + geom_col() + labs(title="bargraph for Year", x="Year", y="Number of rentals")
 bar_weathertype = ggplot(bike_rental, aes(WeatherType,cnt)) + geom_col() + labs(title="bargraph for Weather type", x="Weather Type", y="Number of rentals")
 bar_hol = ggplot(bike_rental, aes(holiday,cnt)) + geom_col() + labs(title="bargraph for Holiday", x="Holiday", y="Number of rentals")
 bar_month = ggplot(bike_rental, aes(mnth,cnt)) + geom_col() + labs(title="bargraph for Month", x="Month", y="Number of rentals")
 bar_day = ggplot(bike_rental, aes(weekday,cnt)) + geom_col() + labs(title="bargraph for day", x="DAY", y="Number of rentals")
 gridExtra::grid.arrange(bar_season,bar_year,bar_weathertype,bar_hol,bar_month,bar_day,ncol=2)

 #box plot for outlier analysis
box_temp = ggplot(bike_rental,aes(y = Actualtemp,x=1))+geom_boxplot(outlier.color = 'red',outlier.size = 5,fill ='blue')+  labs(title="Boxplot for Temperature", y="Temp")
box_atemp = ggplot(bike_rental,aes(y = Actualatemp,x=1))+geom_boxplot(outlier.color = 'red',outlier.size = 5,fill ='blue')+  labs(title="Boxplot for feeling Temperature", y="feeling Temp")
box_hum = ggplot(bike_rental,aes(y = Humidity,x=1))+geom_boxplot(outlier.color = 'red',outlier.size = 5,fill ='blue')+  labs(title="Boxplot for Humidity", y="Humidity") 
box_ws = ggplot(bike_rental,aes(y = Actualwindspeed,x=1))+geom_boxplot(outlier.color = 'red',outlier.size = 5,fill ='blue')+  labs(title="Boxplot for Windspeed", y="Windspeed")
gridExtra::grid.arrange(box_temp,box_atemp,box_hum,box_ws,ncol=2)
#outlier removal

#removal of outliers from Humidity
bike_rental = bike_rental[which(!bike_rental$hum %in% boxplot.stats(bike_rental$hum)$out),]
#removal of outliers from windspeed

bike_rental = bike_rental[which(!bike_rental$windspeed %in% boxplot.stats(bike_rental$windspeed)$out),]


#box plot after outlier removal
ggplot(bike_rental,aes(y = Humidity,x=1))+geom_boxplot(outlier.color = 'red',outlier.size = 5,fill ='blue')+  labs(title="Boxplot for Humidity", y="Humidity") 
ggplot(bike_rental,aes(y = Actualwindspeed,x=1))+geom_boxplot(outlier.color = 'red',outlier.size = 5,fill ='blue')+  labs(title="Boxplot for Windspeed", y="Windspeed")

#scatter plot of numerical variables with dependent variable.

scatter_temp = ggplot(bike_rental,aes(Actualtemp,cnt))+geom_point(color='blue')+labs(title = 'Scatterplot of temp and count',x = 'Temp',y='Rental Count')
scatter_atemp = ggplot(bike_rental,aes(Actualatemp,cnt))+geom_point(color='blue')+labs(title = 'Scatterplot of feeling temp and count',x = 'feeling temp',y='Rental Count')
scatter_hum = ggplot(bike_rental,aes(Humidity,cnt))+geom_point(color='blue')+labs(title = 'Scatterplot of Humidity and count',x = 'Humidity',y='Rental Count')
scatter_wind = ggplot(bike_rental,aes(Actualwindspeed,cnt))+geom_point(color='blue')+labs(title = 'Scatterplot of windspeed and count',x = 'windspeed',y='Rental Count')
gridExtra::grid.arrange(scatter_temp,scatter_atemp,scatter_hum,scatter_wind,ncol=2)

#correlation analysis(matrix)
cor(bike_rental[,9:15])


#correlation between temp and atemp is high let's confirm it with scatterplot
ggplot(bike_rental,aes(temp,atemp))+geom_point(color='blue')+labs(title = 'Scatterplot of temp and atemp',x = 'temp',y='atemp')

#chi square test for categories -- just to check how it works.
factor_index = sapply(bike_rental,is.factor)
factor_data = bike_rental[,factor_index]
View(factor_data)
for (i in (2:8)){
  print(names(factor_data[i]))
  for (j in (2:8)){
    print (names(factor_data[j]))
    print (chisq.test(factor_data[,i],factor_data[,j]))
  }
}  #holiday is least independent variable between all
#remove earlier created variable as we use normalised variables(feature scaling) given in data for better results
bike_rental =  subset(bike_rental, select = -c(atemp,holiday,casual,registered,Actualatemp,Actualtemp,Actualwindspeed,Season_abbr,Humidity,Year,WeatherType,dteday))
head(bike_rental)


#Modeling
#Linear Regression
#splitting in Test and train
library(caTools)
set.seed(0)
split<-sample.split(bike_rental$cnt,SplitRatio = 0.8)
train<-subset(bike_rental,split == TRUE)
test<-subset(bike_rental,split== FALSE)

#train_index = sample(1:nrow(bike_rental), 0.8 * nrow(bike_rental))
#train = bike_rental[train_index,]
#test = bike_rental[-train_index,]

model_lm = lm(cnt~.,data=train)
summary(model_lm)

#predictions
predict_lm = predict(model_lm,test[,-10])
#MAPE
library(DMwR)
regr.eval(test[,10],predict_lm,stats = {"mape"})*100
#R square - 0.8453
#MAPE -16.30

#Decision Tree Regressor
library(rpart)
#set.seed(0)
model_dtr = rpart(cnt~.,data= train,method = "anova")
summary(model_dtr)
#plotting the decision tree
plot(model_dtr)
text(model_dtr)
#predictions
predict_dtr = predict(model_dtr,test[,-10])
#Mape
regr.eval(test[,10],predict_dtr,stats = {"mape"})*100
Rsquare = (cor(test[,10],predict_dtr))^2
Rsquare
#23.07mape
#0.80 Rsquare
#Random Forest Regressor
library(randomForest)
str(train)

model_rfr = randomForest(cnt~.,data = train,ntree = 500)

predict_rfr =  predict(model_rfr,test[,-10])
#Mape
regr.eval(test[,10],predict_rfr,stats = {"mape"})*100
Rsquare = (cor(test[,10],predict_dtr))^2
Rsquare

#15.94 Mape
#0.805 Rsquare

