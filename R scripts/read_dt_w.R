
read_dt_w<-function(idmeter){
  ## set working directory
  setwd("~/SELECT ERASMUS MUNDUS/ENERBYTE- Thesis&internship/THESIS - Enerbyte/Chapter 2. Case study Data analysis/R for Tesi/test_01_tesi")
  ## Import dataset --> not choose "strings as factors"  
  meter_rubi_01 <- read.csv("~/SELECT ERASMUS MUNDUS/ENERBYTE- Thesis&internship/THESIS - Enerbyte/Chapter 2. Case study Data analysis/R for Tesi/test_01_tesi/meter_rubi_wd_we.csv", sep=";", stringsAsFactors=FALSE)
  current_costs<-meter_rubi_01[(meter_rubi_01$source == "CURRENTCOST"),]
  current_costs$source <-NULL
  names(current_costs)<- c("idmeter","date","00:00","01:00","02:00","03:00","04:00","05:00","06:00","07:00","08:00","09:00","10:00","11:00","12:00","13:00","14:00","15:00","16:00","17:00","18:00","19:00","20:00","21:00","22:00","23:00")
  rubi<-current_costs[current_costs$idmeter == idmeter ,]
  
  
  ###Cleaning data: eliminating bad data
  ###1. Get the sum of each row 
  rubi_cut<-rubi[3:26]
  rubi_zero<-rubi[apply(rubi_cut==0, 1, sum)<=0,]
  
  ### 2. frozens
  x<-rubi_zero[3:26]
  y<-x[-1]
  diff <- y-x[1:length(x)-1]
  rubi_net<-rubi_zero[apply(diff==0,1,sum)<=2,]               ## more than six 0 per row, remove the row
  names(rubi_net)<- c("idmeter","date","00:00","01:00","02:00","03:00","04:00","05:00","06:00","07:00","08:00","09:00","10:00","11:00","12:00","13:00","14:00","15:00","16:00","17:00","18:00","19:00","20:00","21:00","22:00","23:00")
  rubi_net
}
