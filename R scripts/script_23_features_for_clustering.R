
setwd("~/SELECT ERASMUS MUNDUS/ENERBYTE- Thesis&internship/THESIS - Enerbyte/Part 3. Data analysis/R for Tesi/test_01_tesi")
    rubi <- read.csv("~/SELECT ERASMUS MUNDUS/ENERBYTE- Thesis&internship/THESIS - Enerbyte/Part 3. Data analysis/R for Tesi/test_01_tesi/meter_rubi_wd_we.csv", sep=";", stringsAsFactors=FALSE)
      rubi<-rubi[(rubi$source == "CURRENTCOST"),]
        rubi$source <-NULL
          rubi$idmeter<-as.numeric(rubi$idmeter)
            rubi$date <-NULL

##FEATURE 1,2,3: Daily mean, min, max
  cast<-as.data.frame(lapply(split(rubi, rubi$idmeter),colMeans))
    cast1<-as.data.frame(t(cast))
      cast1$idmeter<-NULL
        cast1<-round(cast1, digits = 0)
      
      c_daily<-as.data.frame(rowMeans(cast1))
    names(c_daily)<-"c_daily"

      c_daily<-round(c_daily, digits = 0)

      c_max<-as.data.frame(apply(cast1, 1, max))
          names(c_max)<-"c_max"

      c_min<-as.data.frame(apply(cast1, 1, min))
          names(c_min)<-"c_min"

##FEATURE 4,5,6,7: morning(6-11am), noon(12-17h), evening(18-23), night(0-5am)
cast2<-cast1
names(cast2)<-c(0:23)
        
            c_night<-cast2[,1:6]
            c_night<-as.data.frame(rowMeans(c_night))
            names(c_night)<-"c_night"
      c_night<-round(c_night, digits = 0)

              c_morning<-cast2[,7:12]
              c_morning<-as.data.frame(rowMeans(c_morning))
              names(c_morning)<-"c_morning"
      c_morning<-round(c_morning, digits = 0)

              c_noon<-cast2[,13:18]
              c_noon<-as.data.frame(rowMeans(c_noon))
              names(c_noon)<-"c_noon"
      c_noon<-round(c_noon, digits = 0)
        
              c_evening<-cast2[,19:24]
              c_evening<-as.data.frame(rowMeans(c_evening))
              names(c_evening)<-"c_evening"
      c_evening<-round(c_evening, digits = 0)



##FEATURE 8,9: Daily Mean weekday, daily mean weekend

## Import dataset --> not choose "strings as factors"  
  meter_rubi_01 <- read.csv("~/SELECT ERASMUS MUNDUS/ENERBYTE- Thesis&internship/THESIS - Enerbyte/Part 3. Data analysis/R for Tesi/test_01_tesi/meter_rubi_wd_we.csv", sep=";", stringsAsFactors=FALSE)
      current<-meter_rubi_01[(meter_rubi_01$source == "CURRENTCOST"),]
        current$source <-NULL
          names(current)<- c("idmeter","date","00:00","01:00","02:00","03:00","04:00","05:00","06:00","07:00","08:00","09:00","10:00","11:00","12:00","13:00","14:00","15:00","16:00","17:00","18:00","19:00","20:00","21:00","22:00","23:00")
            current$date<-as.POSIXct((current$date), format="%d/%m/%Y")

current1<-current[order(as.numeric(current$idmeter),current$date),]

##extract weekday
library(xts)
current2<-as.xts(current1,current1$date)

###weekdays
weekdays<-current2[.indexwday(current2) %in% 1:5] #labels=c("Monday","Tuesday","Wednesday", "Thursday", "Friday")
    w_days<-as.data.frame(dates=index(weekdays), coredata(weekdays))
      w_days$dates<-NULL
        names(w_days)<- c("idmeter","date",c("wd0","wd1","wd2","wd3","wd4","wd5","wd6","wd7","wd8","wd9","wd10","wd11","wd12","wd13","wd14","wd15","wd16","wd17","wd18","wd19","wd20","wd21","wd22","wd23"))
            w_days[,c(3:26)] <- lapply(w_days[,c(3:26)], as.character)
              w_days[,c(3:26)] <- lapply(w_days[,c(3:26)], as.numeric)
          w_days$idmeter<-as.numeric(as.character(w_days$idmeter))
        w_days$date<-as.POSIXct(w_days$date)
      w_days<-w_days[order(w_days$idmeter,w_days$date),]
    str(w_days)

###weekends
    weekends<-current2[!.indexwday(current2) %in% 1:5]
        w_ends<-data.frame(dates=index(weekends), coredata(weekends))
          w_ends$dates<-NULL
            names(w_ends)<- c("idmeter","date",c("we0","we1","we2","we3","we4","we5","we6","we7","we8","we9","we10","we11","we12","we13","we14","we15","we16","we17","we18","we19","we20","we21","we22","we23"))
              w_ends[,c(3:26)] <- lapply(w_ends[,c(3:26)], as.character)
                w_ends[,c(3:26)] <- lapply(w_ends[,c(3:26)], as.numeric)
              w_ends$idmeter<-as.numeric(as.character(w_ends$idmeter))
          w_ends$date<-as.POSIXct(w_ends$date)
      w_ends<-w_ends[order(w_ends$idmeter,w_ends$date),]
str(w_ends)

###### Data frame are correctly separated by weekday/weekend


##Caclulate the mean for each hour (0 to 23)
  ##weekdays_mean
    w_days$date<-NULL
      weekdays_mean<-as.data.frame(lapply(split(w_days, w_days$idmeter),colMeans))
        weekdays_mean<-as.data.frame(t(weekdays_mean))
          weekdays_mean$idmeter<-NULL
            weekdays_mean<-round(weekdays_mean, digits = 0)
              weekdays_mean

    c_weekday<-as.data.frame(rowMeans(weekdays_mean))
    names(c_weekday)<-"c_weekday"
    c_weekday<-round(c_weekday, digits = 0)

  ##weekends_mean
      w_ends$date<-NULL
          weekends_mean<-as.data.frame(lapply(split(w_ends, w_ends$idmeter),colMeans))
              weekends_mean<-as.data.frame(t(weekends_mean))
                weekends_mean$idmeter<-NULL
                  weekends_mean<-round(weekends_mean, digits = 0)
                    weekends_mean

      c_weekend<-as.data.frame(rowMeans(weekends_mean))
      names(c_weekend)<-"c_weekend"
      c_weekend<-round(c_weekend, digits = 0)



## Recap features 1 to 9
c_daily
c_max
c_min
c_morning
c_night
c_evening
c_noon
c_weekday
c_weekend


##############RATIOS FIGURES ########

r_mean_max<-c_daily/c_max
names(r_mean_max)<-"r_mean_max"

r_min_mean<-c_min/c_daily
names(r_min_mean)<-"r_min_mean"

r_min_max<-c_min/c_max
names(r_min_max)<-"r_min_max"

r_night_day<-c_night/c_daily
names(r_night_day)<-"r_night_day"

r_morning_noon<-c_morning/c_noon
names(r_morning_noon)<-"r_morning_noon"
  
r_evening_noon<- c_evening/c_noon
names(r_evening_noon)<-"r_evening_noon"


c_figures<-cbind(c_daily,c_max,c_min, c_morning, c_night, c_evening,c_noon, c_weekday,c_weekend)
r_figures<-cbind(r_mean_max,r_min_mean,r_min_max,r_night_day,r_morning_noon,r_evening_noon)
str(r_figures)


######TEMPORAL PROPERTIES ########




