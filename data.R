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
tab <- table(data$location)
complete <- names(tab)[tab==max(tab)]
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

smoothTS = TimeSeries[-c(9:12)]
smoothTS[-c(7:13)] = lapply(smoothTS[-c(7:13)],function(x) apply(x,2,na.interp))
smoothTS[-c(7:13)] = lapply(smoothTS[-c(7:13)],function(x) apply(x,2,SMA,n=10))
smoothTS[-c(7:13)] = lapply(smoothTS[-c(7:13)],function(x) ts(x,start=min(data$RDate), end=max(data$RDate)))


names(smoothTS)[-c(9:12)]=lapply(names(smoothTS)[-c(9:12)],function(x) paste0(x,"_smoothed"))

RVal <- function(ts){
  R=rep(NA, length(ts)) 
  for (t in 11:length(ts)) { 
    R[t-1] <- sum(ts[t-0:6]) / sum(ts[t-4:10]) 
  }
  R=ts(R, start=min(data$RDate), end=max(data$RDate))
  return(R)
} 



RWert = lapply(smoothTS$new_cases_smoothed, RVal )
RMulti <- do.call(cbind, RWert)

smoothTS[[length(smoothTS)+1]]=RMulti

# doubing time
doublingTime = function(RValue){
  return(log(2)/log(RValue))
}

doubleVal = lapply(RWert,doublingTime)
dobuleMulti = do.call(cbind, doubleVal)

smoothTS[[length(smoothTS)+1]]=dobuleMulti

names(smoothTS)[names(smoothTS)==""]=c("R_Value","doubling_Times")

smoothvars=names(smoothTS)[-c(11:12)]

originalvars=names(TimeSeries)[-c(13:14)]

TimeSeries=c(smoothTS,TimeSeries)


# calculate cluster
TimeSeriesList = lapply(TimeSeries[c(5:8)],as.list)
TimeSeriesList = lapply(TimeSeriesList,function(x) lapply(x,is.na))
TimeSeriesList = lapply(TimeSeriesList,function(x) lapply(x,na_interpolation))
#test=as.list(TimeSeries$total_cases_smoothed)
#test=lapply(test,na_interpolation)
#typeof(TimeSeries$total_cases_smoothed[,1])

dd = lapply(TimeSeriesList,dist,method="DTW")

hc3 = lapply(dd,hclust,method="mcquitty")


clusters=lapply(hc3,cutree, 5)