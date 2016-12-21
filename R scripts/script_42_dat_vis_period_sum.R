
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
mon1<-test1['2014-04::2015-02']
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
hourly_data1$dayname <- factor(hourly_data1$dayname, levels = c("dilluns","dimarts","dimecres", "dijous", "divendres","dissabte","diumenge"))
#######################################################################
#############################################################


#Agregate date per hour 00:00,01:00,02:00

mon4<-hourly_data1
holo<-aggregate( cbind( power ) ~ day+month+year , data = mon4 , FUN = sum )
holo$dates<-as.POSIXct(paste(holo$year, holo$month, holo$day,sep='-'))

holo$dates<-as.Date(holo$dates)
#####PLOTTING BOTHS --> bar, amb + coord_polar() 
library(ggplot2)
library(scales)

#daily aggregation
ggplot(holo,aes(dates, power))+ geom_bar(aes(fill=power),stat = "identity")+ 
  labs(title = "User:90713  Daily aggregation consumption [kWh]",x = "Dates",y="Daily Consumption [kWh]")

ggplot(holo,aes(dates, power))+ geom_bar(aes(fill=power),stat = "identity")+ 
  labs(title = "Daily aggregation",x = "Dates",y="Daily Consumption [kWh]")+
  scale_x_date(breaks = date_breaks("4 weeks"),labels = date_format("%b%Y"))

#monthy aggregation
mon4<-hourly_data1
monthly_ag<-aggregate( cbind( power ) ~ monthname , data = mon4 , FUN = sum )

#plot monthly aggregation
ggplot(monthly_ag,aes(monthname, power))+ geom_bar(aes(fill=power),stat = "identity")+
  labs(title = "User:90713  Monthly aggregation consumption [kWh]",x = "Dates",y="Monthly Consumption [kWh]")+ # +scale_fill_gradientn(colours=c("green","orange","red"))
theme_set(theme_gray(base_size = 26))



