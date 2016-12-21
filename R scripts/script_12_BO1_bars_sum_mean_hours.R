source("read_dt.R")
test1<-read_dt(112980)

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

###SUM!!!   Agregate date per hour 00:00,01:00,02:00
mon4<-hourly_data1
holo<-aggregate( cbind( power ) ~ hour , data = mon4 , FUN = sum )
holo$dates<-as.POSIXct(paste(holo$year, holo$month, holo$day,sep='-'))
holo1<-aggregate( cbind( power ) ~ hour + monthname , data = mon4 , FUN = sum)
holo1$monthname <- factor(holo1$month, levels = c("gen 2014" , "feb 2014" , "març 2014" ,"abr 2014" , "maig 2014" ,"juny 2014" ,"jul 2014" , "ag 2014"  , "set 2014" , "oct 2014" , "nov 2014" , "des 2014","gen 2015" , "feb 2015" , "març 2015" ))
#OK

###MEAN!!
holo2<-aggregate( cbind( power ) ~ hour , data = mon4 , FUN = mean )
holo2$dates<-as.POSIXct(paste(holo$year, holo2$month, holo2$day,sep='-'))
holo3<-aggregate( cbind( power ) ~ hour + monthname , data = mon4 , FUN = mean)
holo3$monthname <- factor(holo3$month, levels = c("gen 2014" , "feb 2014" , "març 2014" ,"abr 2014" , "maig 2014" ,"juny 2014" ,"jul 2014" , "ag 2014"  , "set 2014" , "oct 2014" , "nov 2014" , "des 2014","gen 2015" , "feb 2015" , "març 2015" ))
#OK


#####PLOTTING SUM!!!! --> bar, amb + coord_polar() 
library(ggplot2)
#acumulat periode seleccionat
ggplot(holo,aes(hour, power,group=1))+ geom_point()#+ geom_line()
ggplot(holo,aes(hour, power,color=power))+ geom_point(shape=19)+ geom_line()+scale_colour_gradientn(colours=c("green","orange","red"))
ggplot(holo,aes(hour, power))+ geom_bar(aes(fill=power),stat = "identity")
ggplot(holo,aes(hour, power))+ geom_bar(aes(fill=power),stat = "identity")+coord_polar()
ggplot(holo,aes(hour, power))+ geom_bar(width=0.9,aes(fill=power),stat = "identity")+scale_fill_gradientn(colours=c("green","orange","red"))
ggplot(holo,aes(hour, power))+ geom_bar(width=0.9,aes(fill=power),stat = "identity")+scale_fill_gradientn(colours=c("green","orange","red"))+coord_polar()
#separat per mesos
ggplot(holo1,aes(hour, power))+ geom_bar(width=0.9,aes(fill=power),stat = "identity")+facet_wrap(~monthname)+scale_fill_gradientn(colours=c("green","orange","red"))
ggplot(holo1,aes(hour, power))+ geom_bar(width=0.9,aes(fill=power),stat = "identity")+facet_wrap(~monthname)+scale_fill_gradientn(colours=c("green","orange","red"))+coord_polar()
#separat per hores
ggplot(holo1,aes(monthname, power))+ geom_bar(width=0.9,aes(fill=power),stat = "identity")+facet_wrap(~hour)+scale_fill_gradientn(colours=c("green","orange","red"))
ggplot(holo1,aes(monthname, power))+ geom_bar(width=0.9,aes(fill=power),stat = "identity")+facet_wrap(~hour)+scale_fill_gradientn(colours=c("green","orange","red"))+coord_polar()

#####PLOTTING MEAN!!!! --> bar, amb + coord_polar() 
library(ggplot2)
#acumulat periode seleccionat
ggplot(holo2,aes(hour, power,group=1))+ geom_point()+ geom_line()+scale_y_continuous(breaks=c(seq(0,10000,by=500)))+ggtitle("load profile")
ggplot(holo2,aes(hour, power,color=power))+ geom_point(shape=19)+ geom_line()+scale_colour_gradientn(colours=c("green","orange","red"))
ggplot(holo2,aes(hour, power))+ geom_bar(aes(fill=power),stat = "identity")
ggplot(holo2,aes(hour, power))+ geom_bar(aes(fill=power),stat = "identity")+coord_polar()
ggplot(holo2,aes(hour, power))+ geom_bar(width=0.9,aes(fill=power),stat = "identity")+scale_fill_gradientn(colours=c("green","orange","red"))
ggplot(holo2,aes(hour, power))+ geom_bar(width=0.9,aes(fill=power),stat = "identity")+scale_fill_gradientn(colours=c("green","orange","red"))+coord_polar()
#separat per mesos
ggplot(holo3,aes(hour, power))+ geom_bar(width=0.9,aes(fill=power),stat = "identity")+facet_wrap(~monthname)+scale_fill_gradientn(colours=c("green","orange","red"))
ggplot(holo3,aes(hour, power))+ geom_bar(width=0.9,aes(fill=power),stat = "identity")+facet_wrap(~monthname)+scale_fill_gradientn(colours=c("green","orange","red")) + coord_polar()
#separat per hores
ggplot(holo3,aes(monthname, power))+ geom_bar(width=0.9,aes(fill=power),stat = "identity")+facet_wrap(~hour)+scale_fill_gradientn(colours=c("green","orange","red"))
ggplot(holo3,aes(monthname, power))+ geom_bar(width=0.9,aes(fill=power),stat = "identity")+facet_wrap(~hour)+scale_fill_gradientn(colours=c("green","orange","red")) + coord_polar()
