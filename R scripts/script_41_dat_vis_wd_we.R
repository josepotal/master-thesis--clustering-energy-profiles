
source("read_dt_w.R")
rubi_net<-read_dt_w(112838)


#Percentage calcualtions  

###1. Get the sum of each row 
rubi_sum<-rubi_net[3:26]
row_sum<-as.matrix(rowSums(rubi_sum))
names(row_sum)<-"sum"

###2. Division to get the percentages per hour  
division<-as.data.frame(rubi_sum/row_sum)
division_id<-cbind(rubi_net$idmeter,rubi_net$date,division)
names(division_id)<-c("date","00:00","01:00","02:00","03:00","04:00","05:00","06:00","07:00","08:00","09:00","10:00","11:00","12:00","13:00","14:00","15:00","16:00","17:00","18:00","19:00","20:00","21:00","22:00","23:00")  

rubi_net_perc<-division_id

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
mon1<-test1['2014-04::2015-03']
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


#as.xts
library(xts)
mon4<-hourly_data1
mon4<-as.xts(mon4,mon4$dates)

###weekdays
weekdays<-mon4[.indexwday(mon4) %in% 1:5] #labels=c("Monday","Tuesday","Wednesday", "Thursday", "Friday")
str(weekdays)
w_days<-data.frame(dates=index(weekdays), coredata(weekdays))
w_days$power<-as.numeric(as.character(w_days$power))
w_days$dayname <- factor(w_days$dayname, levels = c("dilluns","dimarts","dimecres", "dijous", "divendres")
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
                         
 ###MEAN!!
holo_wdays<-aggregate( cbind(power) ~ hour , data = penu1 , FUN = mean )
holo_wdays$dates<-as.POSIXct(paste(holo_wdays$year, holo_wdays$month, holo_wdays$day,sep='-'))
holo_wdays1<-aggregate( cbind( power ) ~ hour + monthname , data = penu1 , FUN = mean)
holo_wdays1$monthname <- factor(holo_wdays1$month, levels = c("gen 2014" , "feb 2014" , "març 2014" ,"abr 2014" , "maig 2014" ,"juny 2014" ,"jul 2014" , "ag 2014"  , "set 2014" , "oct 2014" , "nov 2014" , "des 2014","gen 2015" , "feb 2015" , "març 2015" ))
                         
holo_wends<-aggregate( cbind( power ) ~ hour , data = penu2 , FUN = mean )
holo_wends$dates<-as.POSIXct(paste(holo_wends$year, holo_wends$month, holo_wends$day,sep='-'))
holo_wends1<-aggregate( cbind( power ) ~ hour + monthname , data = penu1 , FUN = mean)
holo_wends1$monthname <- factor(holo_wends1$month, levels = c("gen 2014" , "feb 2014" , "març 2014" ,"abr 2014" , "maig 2014" ,"juny 2014" ,"jul 2014" , "ag 2014"  , "set 2014" , "oct 2014" , "nov 2014" , "des 2014","gen 2015" , "feb 2015" , "març 2015" ))
                         
                         
holo_ultim0<-aggregate( cbind( power ) ~ hour, data = ultim , FUN = mean)
holo_ultim<-aggregate( cbind( power ) ~ hour+ wd_we , data = ultim , FUN = mean)
holo_ultim$monthname <- factor(holo_ultim$month, levels = c("gen 2014" , "feb 2014" , "març 2014" ,"abr 2014" , "maig 2014" ,"juny 2014" ,"jul 2014" , "ag 2014"  , "set 2014" , "oct 2014" , "nov 2014" , "des 2014","gen 2015" , "feb 2015" , "març 2015" ))
holo_ultim1<-aggregate( cbind( power ) ~ hour + monthname + wd_we , data = ultim , FUN = mean)
holo_ultim1$monthname <- factor(holo_ultim1$month, levels = c("gen 2014" , "feb 2014" , "març 2014" ,"abr 2014" , "maig 2014" ,"juny 2014" ,"jul 2014" , "ag 2014"  , "set 2014" , "oct 2014" , "nov 2014" , "des 2014","gen 2015" , "feb 2015" , "març 2015" ))
holo_ultim2<-aggregate( cbind( power ) ~ hour + dayname , data = ultim , FUN = mean)
holo_ultim2$dayname <- factor(holo_ultim2$dayname, levels = c("dilluns","dimarts","dimecres", "dijous", "divendres","dissabte", "diumenge"))
   ##PLOTTING ALL JOINT In the same graph, all weekdays vs weekends
 library(ggplot2)
                         
##weekdays
a<-ggplot(holo_wdays,aes(hour, power))+ geom_bar(width=0.9,aes(fill=power),stat = "identity")+scale_fill_gradientn(colours=c("green","orange","red"))
ggplot(holo_wdays1,aes(hour, power))+ geom_bar(width=0.9,aes(fill=power),stat = "identity")+facet_wrap(~monthname)+scale_fill_gradientn(colours=c("green","orange","red"))
                         
#weekends
b<-ggplot(holo_wends,aes(hour, power))+ geom_bar(width=0.9,aes(fill=power),stat = "identity")+scale_fill_gradientn(colours=c("green","orange","red"))
ggplot(holo_wends1,aes(hour, power))+ geom_bar(width=0.9,aes(fill=power),stat = "identity")+facet_wrap(~monthname)+scale_fill_gradientn(colours=c("green","orange","red"))

source("multiplot_function.R")
multiplot(a,b, cols=2)


 #weekdays+weekends
## together wd_we
ggplot(holo_ultim0,aes(hour, power,group=1))+ geom_bar(width=0.9,aes(fill=power),stat = "identity")+
  labs(title = "User:112992  Load profile [kWh]",x = "Hour",y="Consumption [kWh]")+scale_fill_gradientn(colours=c("green","orange","red"))
  
## differentiate wd_we
ggplot(holo_ultim,aes(hour, power,group=1))+ geom_bar(width=0.9,aes(fill=power),stat = "identity")+
  facet_wrap(~wd_we)+labs(title = "User:112992  Load profiles [kWh]",x = "Hour",y="Consumption [kWh]")+scale_fill_gradientn(colours=c("green","orange","red"))

##differentiate wd_we dodge
ggplot(holo_ultim,aes(hour, power,color=wd_we,fill=wd_we))+ geom_bar(stat = "identity",position="dodge")+
  labs(title = "User:112992  Load profiles [kWh]",x = "Hour",y="Consumption [kWh]")

##differentiate wd_we per facette per month
ggplot(holo_ultim1,aes(hour, power,color=wd_we,fill=wd_we))+ geom_bar(stat = "identity",position="dodge")+facet_wrap(~monthname)+
  labs(title = "User:112838  Monthly Load Profiles [kWh]",x = "Hour",y="Consumption [kWh]")

##differentiate per day of the week
ggplot(holo_ultim2,aes(hour, power,fill=dayname))+ geom_bar(width=0.9,stat = "identity")+facet_wrap(~dayname,ncol = 4)+
  labs(title = "User:112992 Days Load Profiles [kWh]",x = "Hour",y="Consumption [kWh]")+
  theme_set(theme_gray(base_size = 26))


                         



