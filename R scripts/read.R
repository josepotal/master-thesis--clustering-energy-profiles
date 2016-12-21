##function script to read each idmeter by specifying the idmeter number

read<-function(idmeter){
## set working directory
  setwd("~/SELECT ERASMUS MUNDUS/ENERBYTE- Thesis&internship/THESIS - Enerbyte/Part 3. Data analysis/R for Tesi/test_01_tesi")
## Import dataset --> not choose "strings as factors"  
  meter_rubi_01 <- read.csv("~/SELECT ERASMUS MUNDUS/ENERBYTE- Thesis&internship/THESIS - Enerbyte/Part 3. Data analysis/R for Tesi/test_01_tesi/meter_rubi_01.csv", sep=";", stringsAsFactors=FALSE)
  current_costs<-meter_rubi_01[(meter_rubi_01$source == "CURRENTCOST"),]
  current_costs$source <-NULL
  names(current_costs)<- c("idmeter","date","00:00","01:00","02:00","03:00","04:00","05:00","06:00","07:00","08:00","09:00","10:00","11:00","12:00","13:00","14:00","15:00","16:00","17:00","18:00","19:00","20:00","21:00","22:00","23:00")
library(reshape2)
##reshare 24 columns to 1
  dats<-melt(current_costs, id.vars=c("idmeter", "date"))
  names(dats)<-c("idmeter","date","time","power")
#dats$datetime<-as.POSIXct(paste(dats$date, dats$time),format="%d/%m/%Y %H:%M:%S")
## convert date and time from Character to Time objects -->maybe use POSIXlt to stora days of the week?
  dats$time<-as.character(dats$time)
  dats$date<-as.Date(dats$date,format="%d/%m/%Y")
##order by date
  clean<-dats[order(as.numeric(dats$idmeter),dats$date),]
## choose the idmeter
test1<-clean[clean$idmeter == idmeter ,]
}
##As example
#test1<-read(90713)
#write.csv(test1,file="id_90713.csv")
#library(ggplot2)
#ggplot(test1,aes(date,power,color=idmeter))+geom_point()+geom_smooth()
