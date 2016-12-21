
######  Import database 
meter_rubi_01 <- read.csv("~/SELECT ERASMUS MUNDUS/ENERBYTE- Thesis&internship/THESIS - Enerbyte/Chapter 2. Case study Data analysis/R for Tesi/test_01_tesi/meter_rubi_wd_we_bo.csv", sep=";", stringsAsFactors=FALSE)
rubi<-meter_rubi_01[(meter_rubi_01$source == "CURRENTCOST"),]
rubi$source <-NULL
names(rubi)<- c("idmeter","date","00:00","01:00","02:00","03:00","04:00","05:00","06:00","07:00","08:00","09:00","10:00","11:00","12:00","13:00","14:00","15:00","16:00","17:00","18:00","19:00","20:00","21:00","22:00","23:00")
rubi$date<-as.POSIXct((rubi$date), format="%d/%m/%Y")
rubi1<-rubi[order(as.numeric(rubi$idmeter),rubi$date),]
#rubi$date <-NULL


###### Clean database

###1. Get the sum of each row 
rubi_cut<-rubi1[3:26]
rubi_zero<-rubi[apply(rubi_cut==0, 1, sum)<=0,]

### 2. frozens
x<-rubi_zero[3:26]
y<-x[-1]
diff <- y-x[1:length(x)-1]
rubi_net<-rubi_zero[apply(diff==0,1,sum)<=2,]               ## more than six 0 per row, remove the row

names(rubi_net)<- c("idmeter","date","00:00","01:00","02:00","03:00","04:00","05:00","06:00","07:00","08:00","09:00","10:00","11:00","12:00","13:00","14:00","15:00","16:00","17:00","18:00","19:00","20:00","21:00","22:00","23:00")

rubi_net2<-rubi_net[3:26]
all_row_sum<-as.data.frame(rowSums(rubi_net2))
all_all<-as.data.frame(colSums(all_row_sum))
all_kwh<-all_all/1000


####Separate Weekday and Weekends
library(xts)
rubi2<-as.xts(rubi_net,rubi_net$date)

## Weekdays
weekdays<-rubi2[.indexwday(rubi2) %in% 1:5] #labels=c("Monday","Tuesday","Wednesday", "Thursday", "Friday")
w_days<-as.data.frame(dates=index(weekdays), coredata(weekdays))
w_days$dates<-NULL
names(w_days)<- c("idmeter","date",c("wd0","wd1","wd2","wd3","wd4","wd5","wd6","wd7","wd8","wd9","wd10","wd11","wd12","wd13","wd14","wd15","wd16","wd17","wd18","wd19","wd20","wd21","wd22","wd23"))
w_days[,c(3:26)] <- lapply(w_days[,c(3:26)], as.character)
w_days[,c(3:26)] <- lapply(w_days[,c(3:26)], as.numeric)
w_days$idmeter<-as.numeric(as.character(w_days$idmeter))
w_days$date<-as.POSIXct(w_days$date)
w_days<-w_days[order(w_days$idmeter,w_days$date),]
str(w_days)



### WEEKDAYSSS!!
rubi_net3<-w_days[3:26]
wd_row_sum<-as.data.frame(rowSums(rubi_net3))
wd_all<-as.data.frame(colSums(wd_row_sum))
wd_kwh<-wd_all/1000

w_days_net<-w_days[3:26]
row_sum<-as.matrix(rowSums(w_days_net))
names(row_sum)<-"sum"