##############################################################################################################
############# Testing Section ################################################################################
##############################################################################################################

# loads TimeFrame.R (if currently in same directory as this script)
source(paste(dirname(rstudioapi::getSourceEditorContext()$path),"/TimeFrame.R",sep=""))


#date test
#data for testing purposes
testdate = "03.04.2017"
testdates = c("03.04.2017","05.06.2018","23.09.2019")
testdatesS = c("03.04.17","05.06.18","23.09.19") #functions shoud automatically add the 20 in front of the year
testdatesR = c("2017/04/03","2018/06/05","2019/09/23") #functions need specification regarding separator and order of y m d 

split.date(testdate)
split.date(testdates)
split.date(testdatesS)
split.date(testdatesR,sep="/",format=c("y","m","d"))
split.date("2015/03/01",sep="/",format=c("y","m","d"))

reformat.date(testdate)
reformat.date(testdates)
reformat.date(testdatesS)
reformat.date(testdatesR,sep="/",format=c("y","m","d"),to.sep=".",to.format=c("d","m","y"))
reformat.date("2015/03/01",sep="/",format=c("y","m","d"),to.sep=".",to.format=c("d","m","y"))
reformat.date("15/03/01",sep="/",format=c("y","m","d"),to.sep=".",to.format=c("d","m","y"))

days.year.passed(testdate)
days.year.passed(testdates)
days.year.passed(testdatesS)
days.year.passed(testdatesR,sep="/",format=c("y","m","d"))
days.year.passed("2015/03/01",sep="/",format=c("y","m","d"))

days.year.coming(testdate)
days.year.coming(testdates)
days.year.coming(testdatesS)
days.year.coming(testdatesR,sep="/",format=c("y","m","d"))
days.year.coming("2015/03/01",sep="/",format=c("y","m","d"))

days.since(testdate,reference="01.01.2015")
days.since(testdates,reference="01.01.2015")
days.since(testdates,reference=c("01.01.2015","01.01.2016","01.01.2017"))
days.since(testdatesS,reference="01.01.2015")
days.since(testdatesR,reference="2015/01/01",sep="/",format=c("y","m","d"))
days.since("2015/03/01",reference="2015/01/01",sep="/",format=c("y","m","d"))


# time test
#data for testing purposes
testtime = c("14:37:11")
testtimes = c("14:37:11","19:28:56","3:10:13")
testtimesS = c("14:37","19:28","03:10")

split.time(testtime)
split.time(testtimes)
split.time(testtimesS,format=c("m","s"))
split.time(testtimes,merge=T)
split.time(testtimesS,format=c("h","m"),merge=T)

to.hour(testtime)
to.hour(testtimes)
to.hour(testtimesS,format=c("h","m"))

to.minute(testtime)
to.minute(testtimes)
to.minute(testtimesS,format=c("h","m"))

to.second(testtime)
to.second(testtimes)
to.second(testtimesS,format=c("h","m"))

to.time(to.second(testtimes),type="s")
to.time(to.second(testtimes),type="s",with.sec=F)
to.time(to.minute(testtimes),type="m")
to.time(to.hour(testtimes),type="h",sep="")


# how to group into pseudo days:
# - calculate the days passed between the date of each registration and a common reference date -> days.since(VectorWithDates,reference=CommonReferenceDate,sep=...,format=...)
#     -> reference date should be set to at least 1 day before first registration
# - calculate the seconds passed since midnight for every time -> to.second(VectorWithTimes)
#     -> compare with cut-off times (should also be given in "seconds since midnight") to remove or associate to current, or next/previous "day since x" (calculated in first step)
#         -> pseudo-day! 

# generate random fake data to try
data = matrix(NA,ncol=3,nrow=0)
for(i in 1:100)
{
  tempT = to.time(runif(round(runif(1,1,20)),0,24),type="h")
  tempTs = split.time(as.vector(tempT),merge=T)
  tempT = tempT[order(as.numeric(as.vector(tempTs)))]
  tempD = rep(every.date.year(2020)[i],length(tempT))
  tempE = sample(c(1,-1),length(tempT),replace=T)
  data = rbind(data,cbind(tempD,apply(split.time(tempT),1,function(x)paste(x,collapse=":")),tempE))
}
data = data[-1,]
data = as.data.frame(data)
names(data) = c("date","time","event")
head(data)
# assume data as dataset for analysis

# distribute datapoints to pseudodates to represent groupings better; refday used as first step
data$refday = days.since(as.vector(data$date),reference="31.12.2019")
data$seconds = to.second(as.vector(data$time))
data$pseudoday = NA

morning = "06:00:00"
evening = "18:00:00"

for(i in 1:nrow(data))
{
  if(data$seconds[i] < to.second(morning)) data$pseudoday[i] = data$refday[i] - 1
  else if(data$seconds[i] > to.second(morning) & data$seconds[i] < to.second(evening)) data$pseudoday[i] = -1
  else data$pseudoday[i] = data$refday[i]
}

dataF = subset(data,data$pseudoday!=-1)
cat(nrow(data)-nrow(dataF)," datapoints removed from dataset\n")
head(dataF)
