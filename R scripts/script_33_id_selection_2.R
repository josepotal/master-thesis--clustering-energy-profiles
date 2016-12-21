
#merge or reshape
setwd("~/SELECT ERASMUS MUNDUS/ENERBYTE- Thesis&internship/THESIS - Enerbyte/Part 3. Data analysis/R for Tesi/test_01_tesi")
## Import dataset --> not choose "strings as factors"  
meter_rubi_01 <- read.csv("~/SELECT ERASMUS MUNDUS/ENERBYTE- Thesis&internship/THESIS - Enerbyte/Part 3. Data analysis/R for Tesi/test_01_tesi/all_clean.csv", sep=";", stringsAsFactors=FALSE)
current_costs<-meter_rubi_01[(meter_rubi_01$source == "CURRENTCOST"),]
current_costs$source <-NULL
names(current_costs)<- c("idmeter","date","00:00","01:00","02:00","03:00","04:00","05:00","06:00","07:00","08:00","09:00","10:00","11:00","12:00","13:00","14:00","15:00","16:00","17:00","18:00","19:00","20:00","21:00","22:00","23:00")
library(reshape2)
##reshare 24 columns to 1
dats<-melt(current_costs, id.vars=c("idmeter", "date"))
names(dats)<-c("idmeter","date","time","power")
##DateTime<-as.POSIXct(paste(segon$Date, segon$Time),format="%d/%m/%Y %H:%M:%S")
## convert date and time from Character to Time objects -->maybe use POSIXlt to stora days of the week?
dats$time<-as.character(dats$time)
dats$datetime<-as.POSIXlt(paste(dats$date, dats$time),format="%d/%m/%Y %H:%M")
dats$date<-NULL
dats$time<-NULL
##columns right place id,date,power
dat3<-cbind(dats[1],dats[3], dats[2])
##order by date
all_clean<-dat3[order(as.numeric(dat3$idmeter),dat3$datetime),]

#################
##count most repeated values per each id

as<-split(all_clean,all_clean$idmeter)
apply(split(all_clean,all_clean$idmeter,sort(table(all_clean$power))))
ret<-all_clean[all_clean$idmeter==112769,]
test2<-data.frame(count=sort(table(ret$power), decreasing=TRUE))
View(head(test2))
ash<- c(unique(all_clean$idmeter))

mill<-data.frame()
for (i in ash){
  ret<-all_clean[all_clean$idmeter==i,]
  test2<-data.frame(count=sort(table(ret$power), decreasing=TRUE))
  mill<-rbind(mill,head(test2))
}
#
re78<-all_clean[all_clean$idmeter==112696,]
test7<-data.frame(count=sort(table(re78$power), decreasing=TRUE))
View(head(test7))






test1<-all[all$idmeter==90713,]
s_63<-as.data.frame(veure(x))

rbind(s_61,s_62,s_63)

mill<-data.frame()
for(i in seq_along(temp1)){
  test1<-read_dt(i)
  s_1000<-as.data.frame(veure(x))
  mill<-rbind(mill,s_1000)
}

veure<-function (x){
  ##histogram
  #hist(test1$power)
  ## know how many 0
  num_zeros <- length(which(test1$power == 0))
  
  ## percentage 0 respect to total
  #total measures
  tot<-length(test1$power)
  if (tot-num_zeros>8000){
    s<-print("YES")
  } else {
    s<-print("NO")
  }
  #percentage 0 to total  
  percen_zeros<- num_zeros/tot
  resum<-c(num_zeros,tot,s,percen_zeros)
  resum<-as.data.frame(resum)
  resum<-t(resum)
  View(resum)
  }
## know how many consecutive numbers --> frozens
test2<-data.frame(count=sort(table(test1$power), decreasing=TRUE))
View(head(test2))

test1[which(test1$power == 212),]





mill<-data.frame()
for (i in seq_along(temp1)){
  test1<-as.data.frame(st[i])
  num_zeros <- length(which(st$power == 0))
  tot<-length(st$power)
  percen_zeros<- num_zeros/tot
  resum<-c(num_zeros,tot,percen_zeros)
  resum<-as.data.frame(resum)
  resum<-t(resum)
  mill<-rbind(mill,resum[i])
}

mill<-lapply(temp1,veure1)

veure1<-function (x){
  ## know how many 0
  num_zeros <- length(which(st$power == 0))
  
  ## percentage 0 respect to total
  #total measures
  tot<-length(st$power)
  if (tot-num_zeros>8000){
    s<-print("YES")
  } else {
    s<-print("NO")
  }
  #percentage 0 to total  
  percen_zeros<- num_zeros/tot
  resum<-c(num_zeros,tot,s,percen_zeros)
  resum<-as.data.frame(resum)
  resum<-t(resum)
  ## know how many consecutive numbers --> frozens
  test2<-data.frame(count=sort(table(st$power), decreasing=TRUE))
 }




#write.csv(test1,file="id_103433.csv")
veure(x)






      


