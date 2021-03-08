library(forecast)
library(TTR)

library(imputeTS)
library(dtw)
library(cluster)

# Read data
data= read.csv("https://covid.ourworldindata.org/data/owid-covid-data.csv")
# add data to dataframe
data$RDate <- as.Date(data$date, "%Y-%m-%d")

data$new_cases_per_new_tests = data$new_cases/data$new_tests
data$new_deaths_per_new_cases = data$new_deaths/data$new_cases
data$infestation = data$total_cases/data$population
# create list of countries with maximal observations



oecd = c("Austria", "Belgium", "Czechia", "Denmark", 
         "Estonia", "Finland", "France", "Germany", "Greece", 
         "Hungary", "Iceland", "Ireland", "Italy", "Latvia", 
         "Lithuania", "Luxembourg", "Netherlands", "Norway", 
         "Poland", "Portugal", "Slovakia", "Slovenia", 
         "Spain", "Sweden", "Switzerland", "United Kingdom",
         "Canada", "Chile", "Colombia", "Mexico", "United States",
         "Australia", "Japan", "South Korea", "New Zealand", "Israel", "Turkey")

complete <- oecd
completedata = subset(data, data$location %in% complete)

# Subset Data for creating Timeseries
TSData = subset(data[,c(3:18,(length(data)-2):length(data))],location %in%complete)
# Group data by country
TSData = split(TSData,completedata$location)


# Create Timeseries
TimeSeries <- lapply( TSData, function(x) ts(x,start=min(data$RDate), end=max(data$RDate)))

# Combine Timeseries across countries by specified variable

all=vector(mode="list",length=length(colnames(TimeSeries[[1]])))
colnames=colnames(TimeSeries[[1]])

for(i in 1:length(colnames(TimeSeries[[1]]))){
  all[[i]]=lapply(TimeSeries,function(x) x[,colnames[i]])
}
names(all)=colnames

TimeSeries=lapply(all,function(x) do.call(cbind,x))
TimeSeries=TimeSeries[c(3:length(TimeSeries))]

originalvars=names(TimeSeries)[-c(13:14)]


# calculate cluster
TimeSeriesList = lapply(TimeSeries[c("new_cases_smoothed_per_million","new_deaths_smoothed_per_million")],as.list)
TimeSeriesList = lapply(TimeSeriesList,function(x) lapply(x,is.na))
TimeSeriesList = lapply(TimeSeriesList,function(x) lapply(x,na_interpolation))
#test=as.list(TimeSeries$total_cases_smoothed)
#test=lapply(test,na_interpolation)
#typeof(TimeSeries$total_cases_smoothed[,1])

dd = lapply(TimeSeriesList,dist,method="DTW")

hc3 = lapply(dd,hclust,method="mcquitty")

clusters=lapply(hc3,cutree, 5)
