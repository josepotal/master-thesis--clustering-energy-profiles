####  Import database
setwd("~/SELECT ERASMUS MUNDUS/ENERBYTE- Thesis&internship/THESIS - Enerbyte/Chapter 2. Case study Data analysis/R for Tesi/test_01_tesi")
rubi <- read.csv("~/SELECT ERASMUS MUNDUS/ENERBYTE- Thesis&internship/THESIS - Enerbyte/Chapter 2. Case study Data analysis/R for Tesi/test_01_tesi/data_id_selection.csv", sep=";", stringsAsFactors=FALSE)
rubi<-rubi[(rubi$source == "CURRENTCOST"),]
rubi$source <-NULL
rubi$idmeter<-as.numeric(rubi$idmeter)
#rubi$date <-NULL

a<-as.data.frame(unique(rubi$idmeter))
names(a)<-"idmeters selected"

###1. Get the sum of each row 
rubi_net<-rubi[3:26]
  rubi_zero<-rubi[apply(rubi_net==0, 1, sum)<=0,]

## test if 0 code works well
hop_zero<-as.data.frame(rowSums(rubi_net==0)<=0)                ## more than four 0 per row, remove the row
  names(hop_zero)<-"ji"
    rubi_ze<-rubi[hop_zero$ji==FALSE,]


### 2. frozens
x<-rubi_zero[3:26]
y<-x[-1]
diff <- y-x[1:length(x)-1]
rubi_frozen<-rubi_zero[apply(diff==0,1,sum)<=2,]               ## more than six 0 per row, remove the row

names(rubi_frozen)<- c("idmeter","date","00:00","01:00","02:00","03:00","04:00","05:00","06:00","07:00","08:00","09:00","10:00","11:00","12:00","13:00","14:00","15:00","16:00","17:00","18:00","19:00","20:00","21:00","22:00","23:00")

b<-as.data.frame(unique(rubi_frozen$idmeter))
## test if frozen code works well
ho<-as.data.frame(rowSums(diff==0)<=2)                ## more than four 0 per row, remove the row
names(ho)<-"ji"
rubi_fr<-rubi_zero[ho$ji==FALSE,]
