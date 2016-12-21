source("read_dt_w.R")
rubi_net<-read_dt_w(90713)

########  Reshaping

library(reshape2)
##reshape 24 columns to 1
dats<-melt(rubi_net, id.vars=c("idmeter", "date"))
names(dats)<-c("idmeter","date","time","power")
##DateTime<-as.POSIXct(paste(segon$Date, segon$Time),format="%d/%m/%Y %H:%M:%S")
## convert date and time from Character to Time objects -->maybe use POSIXlt to stora days of the week?
dats$time<-as.character(dats$time)
dats$datetime<-as.POSIXct(paste(dats$date, dats$time),format="%d/%m/%Y %H:%M")
dats$date<-NULL
dats$time<-NULL
##columns right place id,date,power
dat3<-cbind(dats[1],dats[3], dats[2])
dat3$power<-dat3$power/1000
##order by date
test1<-dat3[order(as.numeric(dat3$idmeter),dat3$datetime),]


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
mon1<-test1['2014-11-03::2014-11-10']
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
months3$months2 <- factor(months3$months2, levels = c("Jan 2014" , "Feb 2014" , "Mar 2014" ,"Apr 2014" , "May 2014" ,"Jun 2014" ,"Jul 2014" , "Aug 2014"  , "Sep 2014" , "Oct 2014" , "Nov 2014" , "Dec 2014","Jan 2015" , "Feb 2015" , "Mar 2015" ))
str(months3)

hourly_data1<-cbind(hourly_data,months3)
str(hourly_data1)
names(hourly_data1)<-c("dates" , "hour", "day","month","year","power"  ,"dayname","monthname")
hourly_data1$dayname <- factor(hourly_data1$dayname, levels = c("Monday","Tuesday","Wednesday", "Thursday", "Friday","Saturday","Sunday"))
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
w_days$dayname <- factor(w_days$dayname, levels = c("Monday","Tuesday","Wednesday", "Thursday", "Friday"))
w_days$monthname<-factor(w_days$monthname,levels = c("Jan 2014" , "Feb 2014" , "Mar 2014" ,"Apr 2014" , "May 2014" ,"Jun 2014" ,"Jul 2014" , "Aug 2014"  , "Sep 2014" , "Oct 2014" , "Nov 2014" , "Dec 2014","Jan 2015" , "Feb 2015" , "Mar 2015" ))
str(w_days)

###weekends
weekends<-mon4[!.indexwday(mon4) %in% 1:5]
w_ends<-data.frame(dates=index(weekends), coredata(weekends))
w_ends$power<-as.numeric(as.character(w_ends$power))
w_ends$dayname<- factor(w_ends$dayname, levels = c("Saturday","Sunday"))
w_ends$monthname<-factor(w_ends$monthname, levels = c("Jan 2014" , "Feb 2014" , "Mar 2014" ,"Apr 2014" , "May 2014" ,"Jun 2014" ,"Jul 2014" , "Aug 2014"  , "Sep 2014" , "Oct 2014" , "Nov 2014" , "Dec 2014","Jan 2015" , "Feb 2015" , "Mar 2015" ))
str(w_ends)

## joint weekdays and weekends
penu1<-w_days
penu1$wd_we<-as.character("week day")
penu2<-w_ends
penu2$wd_we<-as.character("week end")
ultim<-rbind(penu1,penu2)

###########PLOTTING

##PLOTTING ALL JOINT In the same graph, all weekdays vs weekends
library(ggplot2)
ggplot(ultim,aes(dates, power,color=wd_we))+ geom_bar(width=0.9,stat = "identity")+
  labs(title = "User:90713  Period consumption profile [kWh]",x = "Dates",y="Consumption [kWh]")+ 
  scale_y_continuous(breaks=c(seq(0,10,by=0.5)))+
  theme_set(theme_gray(base_size = 26))

