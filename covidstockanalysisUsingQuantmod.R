#loading required R libraries 
library(quantmod)
library(plyr)
library(dplyr)
library(data.table)
library(corrgram)
library(corrplot)
defaultW <- getOption("warn") 
options(warn = -1) 

stocklist <- c("AAPL","GOOG","IBM","AMZN","AMD","FORD")
getSymbols(stocklist, from='2020-02-01',to='2020-06-01')
# To assign it from yahoo instead of Google
#getSymbols(stocklist,src="yahoo", auto.assign=FALSE, from='2020-02-01',to='2020-06-01')
options("getSymbols.warning4.0"=FALSE)

#Merge the stock Details to form a portfolio
ibm<-as.data.table(Ad(IBM))
aapl<-as.data.table(Ad(AAPL))
goog<-as.data.table(Ad(GOOG))
amzn<-as.data.table(Ad(AMZN))
amd<-as.data.table(Ad(AMD))
ford<-as.data.table(Ad(FORD))
portfolio<-join_all(list(ibm,aapl,goog,amzn,amd,ford), by='index', type='left')
colnames(portfolio)<-c("Date","IBM","AAPL","GOOG","AMZN","AMD","FORD")
head(portfolio)


#Read Covid Data
dfcovid <- read.csv(file = 'data/Covid19_6June/covid_19_data.csv', header = TRUE)
dfcovid <- subset(dfcovid, select =c(Date,Confirmed,Deaths,Recovered))
dfcovid <- dfcovid %>% group_by(Date) %>% summarise_all(sum)
dfcovid$Date<-as.Date(dfcovid$Date)
dfcovid <- subset(dfcovid, Date >= as.Date("2020-02-01") & Date<= as.Date("2020-06-01"))
tail(dfcovid)


#Merge Dataset of covid and Portfolio
dfcovidportfolio<-merge(portfolio,dfcovid, by = c("Date"))
df<-subset(dfcovidportfolio, select =-c(Date))
tail(dfcovidportfolio)

portfolio_scaled<-scale(subset(portfolio, select =-c(Date)))
head(portfolio_scaled)

#Correlation Coefficient among variables
cor(df)

#Correlation Matrix Plot
pairs(df, pch = 21)

#Heatmap using Corrplot 
corrplot(corrgram(df))

# Heatmap with heirchical clustering of portfolio
heatmap(as.matrix(portfolio_scaled), symm=FALSE)

heatmap(corrgram(portfolio_scaled), symm=FALSE)
