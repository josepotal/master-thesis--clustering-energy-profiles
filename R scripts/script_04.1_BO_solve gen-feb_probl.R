setwd("~/SELECT ERASMUS MUNDUS/ENERBYTE- Thesis&internship/THESIS - Enerbyte/Chapter 2. Case study Data analysis/R for Tesi/test_01_tesi")
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
dayn<-as.data.frame(mon2$day)
month<-as.data.frame(mon2$month)
year<-as.data.frame(mon2$year)

mon3<- cbind(time_index,dayn,month,year,power)
mon3$day <- weekdays(mon3$dates)
names(mon3)<-c("dates" , "day","month","year","power"  ,"dayname")
str(mon3)
#OK

####Divided by months to print facets###################################
hourly_data<-mon3
months<-as.data.frame(hourly_data$dates)
names(months)<-"month"
str(hourly_data)
months1<-paste(hourly_data$year,hourly_data$month,sep='-')
months2<-as.yearmon(months1)
months3<-as.data.frame(months2)
unique(months3)
months3$months2 <- factor(months3$months2, levels = c("gen 2014" , "feb 2014" , "març 2014" ,"abr 2014" , "maig 2014" ,"juny 2014" ,"jul 2014" , "ag 2014"  , "set 2014" , "oct 2014" , "nov 2014" , "des 2014","gen 2015" , "feb 2015" , "març 2015" ))
str(months3)
####PROBLEM!!!!!!

hourly_data1<-cbind(hourly_data,months3)
str(hourly_data1)
names(hourly_data1)<-c("dates" , "day","month","year","power"  ,"dayname","months2")
hourly_data1$dayname <- factor(hourly_data1$dayname, levels = c("dilluns","dimarts","dimecres","dijous","divendres","dissabte","diumenge"))
#######################################################################

#Agregate date per hour 00:00,01:00,02:00

mon4<-hourly_data1
holo<-aggregate( cbind( power ) ~ day + month + year , data = mon4 , FUN = sum )
holo$dates<-as.POSIXct(paste(holo$year, holo$month, holo$day,sep='-'))

#####PLOTTING BOTHS --> bar, amb + coord_polar() 
library(ggplot2)
ggplot(holo,aes(dates, power))+ geom_point()+geom_line()+geom_smooth()#+ facet_wrap(~month)
ggplot(mon4,aes(dates, power))+ geom_point()+geom_line()#+geom_smooth()#+ facet_wrap(~month)
#acumulat periode seleccionat
ggplot(holo,aes(dates, power,group=1))+ geom_point()#+ geom_line()
ggplot(holo,aes(dates, power,color=power))+ geom_point(shape=19)+ geom_line()+scale_colour_gradientn(colours=c("green","orange","red"))
ggplot(holo,aes(dates, power))+ geom_bar(aes(fill=power),stat = "identity")+scale_fill_gradientn(colours=c("green","orange","red"))
#separat per mesos--> FALTA POSAR EL NOM DELS MESOS!!!
ggplot(holo,aes(day, power,color=month))+ geom_point()+facet_wrap(~month)
##amb coord_polar
ggplot(holo,aes(dates, power))+ geom_bar(aes(fill=power),stat = "identity")+ coord_polar()
ggplot(holo,aes(dates, power))+ geom_bar(aes(fill=power),stat = "identity")+scale_fill_gradientn(colours=c("green","orange","red"))+ coord_polar()
