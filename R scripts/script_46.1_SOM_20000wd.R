
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

####  PERCENTAGES (OPOWER) OPTION 2: CURVES ARCHETYPES
## RowSUms, Division, ColMeans --> MORE ACCURATE!

### WEEKDAYSSS!!
#1. Get the sum of each row 
w_days_net<-w_days[3:26]
row_sum<-as.matrix(rowSums(w_days_net))
names(row_sum)<-"sum"


#2. Division to get the percentages per hour
division<-as.data.frame(w_days_net/row_sum)
division_id<-cbind(w_days$idmeter,division)
names(division_id)<-c("idmeter",c("wd0","wd1","wd2","wd3","wd4","wd5","wd6","wd7","wd8","wd9","wd10","wd11","wd12","wd13","wd14","wd15","wd16","wd17","wd18","wd19","wd20","wd21","wd22","wd23"))
division_id_wd<-as.data.frame(division_id)


#3. Column means
cast_99_wd<-as.data.frame(lapply(split(division_id, division_id$idmeter),colMeans))
cast100_wd<-as.data.frame(t(cast_99_wd))
division_id_wd$idmeter<-NULL

perc_wd<-division_id_wd


## 4. call libraries
library(som)
library (kohonen)

##5.1 som cluster no scaling
cast3<-as.matrix(perc_wd)
hol<-som(cast3, grid = somgrid(5, 5, "hexagonal"))
plot(hol)


## 5.2. som cluster scaling data
cast4<-as.matrix (scale(perc_wd))
holy<-som(cast4, grid = somgrid(10, 10, "hexagonal"))
plot(holy)

##5.3. som clustering dividing row per each maximum
som_model<-som(cast4, 
               grid=somgrid(10,10,"hexagonal"), 
               rlen=1000, 
               alpha=c(0.05,0.001),
               #toroidal=TRUE,
               keep.data = TRUE,
               n.hood="circular")
plot(som_model)



##6. Plotting som
summary(som_model) 
str(som_model)
plot(som_model)
plot(som_model, type="codes")    
plot(som_model, type="changes")
plot(som_model, type="quality")
plot(som_model, type="dist.neighbours")
plot(som_model, type="mapping", labels= row.names(cast4))
plot(som_model, type="count")

plot(som_model, type = "property", property = som_model$codes[,4], main=names(som_model$data)[4])

bn<-som_model$codes

plot(som_model, type = "property", property = som_model$codes, main=names(som_model$data))

## Hierarchical clustering to cluster the codebook vectors
str(som_model)
a<-as.matrix(dist(som_model$codes))
plot(hclust(dist(som_model$codes)))
som_cluster <- cutree(hclust(dist(som_model$codes)), 7)
som_cluster

# plot these results:
plot(som_model, type="mapping", bgcol = c("purple","red","gray", "pink", "blue","green", "yellow","black")[som_cluster], main = "Clusters") 
add.cluster.boundaries(som_model, som_cluster)
