source("read_dt.R")
test1<-read_dt(112802)

library(xts)
###prova 1 --> works WELL!!
partim<-as.character(c(test1$datetime))
partim1<-as.POSIXct(partim)
str(partim1)
andale<-data.frame((date = partim1),
                   year = as.numeric(format(partim1, format = "%Y")),
                   month = as.numeric(format(partim1, format = "%m")),
                   day = as.numeric(format(partim1, format = "%d")),
                   hour = as.numeric(format(partim1, format = "%H")))

### prova 
test1$hour<-andale$hour
test1$day<-andale$day
test1$month<-andale$month
test1$year<-andale$year
test1<-as.xts(test1,test1$datetime)
str(test1)

###@@@@@@@@ HUMAN HAND 1 @@@@@@####### PUT THE DATES CORRECTS
mon1<-test1['2014::2015']
mon2<-data.frame(datetime=index(mon1), coredata(mon1))
time_index <- as.data.frame(mon2$datetime)
names(time_index)<-"dates"
str(mon2)
power<-as.numeric(as.character(mon2$power))
hour<-as.data.frame(mon2$hour)
dayn<-as.data.frame(mon2$day)
month<-as.data.frame(mon2$month)
year<-as.data.frame(mon2$year)


mon3<- cbind(time_index,hour,dayn,month,year,power)
mon3$day <- weekdays(mon3$dates)
names(mon3)<-c("dates" , "hour","day","month","year","power"  ,"dayname")
str(mon3)
#OK

####Divided by months to print facets###################################
hourly_data<-mon3
str(hourly_data)
months1<-paste(hourly_data$year,hourly_data$month,sep='-')
months2<-as.yearmon(months1)
months3<-as.data.frame(months2)
months3$months2 <- factor(months3$months2, levels = c("gen 2014" , "feb 2014" , "març 2014" ,"abr 2014" , "maig 2014" ,"juny 2014" ,"jul 2014" , "ag 2014"  , "set 2014" , "oct 2014" , "nov 2014" , "des 2014","gen 2015" , "feb 2015" , "març 2015" ))
str(months3)

hourly_data1<-cbind(hourly_data,months3)
str(hourly_data1)
names(hourly_data1)<-c("dates" , "hour", "day","month","year","power"  ,"dayname","monthname")
hourly_data1$dayname <- factor(hourly_data1$dayname, levels = c("dilluns","dimarts","dimecres","dijous","divendres","dissabte","diumenge"))
#######################################################################
#############################################################


#as.xts
library(xts)
mon4<-hourly_data1
mon4<-as.xts(mon4,mon4$dates)

###weekdays
weekdays<-mon4[.indexwday(mon4) %in% 1:5] #labels=c("Monday","Tuesday","Wednesday", "Thursday", "Friday")
str(weekdays)
w_days<-data.frame(dates=index(weekdays), coredata(weekdays))
w_days$power<-as.numeric(as.character(w_days$power))
w_days$dayname <- factor(w_days$dayname, levels = c("dilluns","dimarts","dimecres","dijous","divendres"))
w_days$monthname<-factor(w_days$monthname,levels = c("gen 2014" , "feb 2014" , "març 2014" ,"abr 2014" , "maig 2014" ,"juny 2014" ,"jul 2014" , "ag 2014"  , "set 2014" , "oct 2014" , "nov 2014" , "des 2014","gen 2015" , "feb 2015" , "març 2015" ))
str(w_days)

###weekends
weekends<-mon4[!.indexwday(mon4) %in% 1:5]
w_ends<-data.frame(dates=index(weekends), coredata(weekends))
w_ends$power<-as.numeric(as.character(w_ends$power))
w_ends$dayname<- factor(w_ends$dayname, levels = c("dissabte","diumenge"))
w_ends$monthname<-factor(w_ends$monthname, levels = c("gen 2014" , "feb 2014" , "març 2014" ,"abr 2014" , "maig 2014" ,"juny 2014" ,"jul 2014" , "ag 2014"  , "set 2014" , "oct 2014" , "nov 2014" , "des 2014","gen 2015" , "feb 2015" , "març 2015" ))
str(w_ends)

## joint weekdays and weekends
penu1<-w_days
penu1$wd_we<-as.character("week day")
penu2<-w_ends
penu2$wd_we<-as.character("week end")
ultim<-rbind(penu1,penu2)

###########PLOTTING
#####PLOTTING BOTHS WEEKDAYS-WEEKENDS
library(ggplot2)
ggplot(hourly_data1,aes(hour, power,color=hour))+ geom_boxplot()
ggplot(hourly_data1,aes(hour, power))+ geom_boxplot(aes(fill=monthname),outlier.shape=NA) + facet_wrap(~monthname) #+ scale_x_discrete(labels=c(0:23))#+ylim(10,3000)
ggplot(hourly_data1,aes(hour, power))+ geom_boxplot(aes(fill=dayname),outlier.shape=NA) + facet_wrap(~dayname) + geom_smooth(aes(group=1))#+scale_x_discrete(labels=c(0:23))#+ylim(10,3000)

#####PLOTTING WEEKDAYS
library(ggplot2)
w_days_plot<-ggplot(w_days,aes(hour, power,color=hour))+ geom_boxplot()
ggplot(w_days,aes(hour, power))+ geom_boxplot(aes(fill=monthname),outlier.shape=NA) + facet_wrap(~monthname)# + ylim(10,3000)
ggplot(w_days,aes(hour, power))+ geom_boxplot(aes(fill=dayname),outlier.shape=NA) + facet_wrap(~dayname)
#####PLOTTING WEEKENDS
library(ggplot2)
w_ends_plot<-ggplot(w_ends,aes(hour, power,color=hour))+ geom_boxplot()
ggplot(w_ends,aes(hour, power))+ geom_boxplot(aes(fill=monthname),outlier.shape=NA) + facet_wrap(~monthname) 
ggplot(w_ends,aes(hour, power))+ geom_boxplot(aes(fill=dayname),outlier.shape=NA) + facet_wrap(~dayname) 
#####PLOTTING WEEKDAYS AND WEEKENDS
library(ggplot2)
ggplot(ultim,aes(hour, power))+ geom_boxplot(aes(fill=wd_we)) + facet_wrap(~wd_we)#+geom_smooth(aes(group=1))#+ ylim(1,3000)
ggplot(ultim,aes(hour, power))+ geom_boxplot(aes(fill=wd_we),outlier.shape=NA) + facet_wrap(~wd_we)#+geom_smooth(aes(color=wd_we,group=1))#+ylim(1,3000)


