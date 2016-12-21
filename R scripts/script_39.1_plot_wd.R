

######  Import database 
meter_rubi_01 <- read.csv("~/SELECT ERASMUS MUNDUS/ENERBYTE- Thesis&internship/THESIS - Enerbyte/Chapter 2. Case study Data analysis/R for Tesi/test_01_tesi/meter_rubi_wd_we_bo.csv", sep=";", stringsAsFactors=FALSE)
rubi<-meter_rubi_01[(meter_rubi_01$source == "CURRENTCOST"),]
rubi$source <-NULL
names(rubi)<- c("idmeter","date","00:00","01:00","02:00","03:00","04:00","05:00","06:00","07:00","08:00","09:00","10:00","11:00","12:00","13:00","14:00","15:00","16:00","17:00","18:00","19:00","20:00","21:00","22:00","23:00")
rubi$date<-as.POSIXct((rubi$date), format="%d/%m/%Y")
rubi1<-rubi[order(as.numeric(rubi$idmeter),rubi$date),]
#rubi$date <-NULL
str(rubi1)

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
str(rubi_net)
####  PERCENTAGES (OPOWER) OPTION 2: CURVES ARCHETYPES
## RowSUms, Division, ColMeans --> MORE ACCURATE!

###1. Get the sum of each row 
rubi_sum<-rubi_net[3:26]
row_sum<-as.matrix(rowSums(rubi_sum))
names(row_sum)<-"sum"

###2. Division to get the percentages per hour  
division<-as.data.frame(rubi_sum/row_sum)
#division$idmeter<-NULL
division_id<-cbind(rubi_net$idmeter,division)
names(division_id)<-c("idmeter","00:00","01:00","02:00","03:00","04:00","05:00","06:00","07:00","08:00","09:00","10:00","11:00","12:00","13:00","14:00","15:00","16:00","17:00","18:00","19:00","20:00","21:00","22:00","23:00")  
division_id<-as.data.frame(division_id)
#test<-as.matrix(rowSums(division_id[2:25]))
division_id$idmeter<-NULL
###3. Column means
#cast_99<-as.data.frame(lapply(split(division_id, division_id$idmeter),colMeans))
#cast100<-as.data.frame(t(cast_99))
#cast100$idmeter<-NULL


hourly_percent<-division_id
hourly_percent1<-cbind(rubi_net$date,hourly_percent)
names(hourly_percent1)<-c("date","00:00","01:00","02:00","03:00","04:00","05:00","06:00","07:00","08:00","09:00","10:00","11:00","12:00","13:00","14:00","15:00","16:00","17:00","18:00","19:00","20:00","21:00","22:00","23:00")  
str(hourly_percent1)


####Separate Weekday and Weekends
library(xts)
rubi2<-as.xts(hourly_percent1,hourly_percent1$date)

## Weekdays
weekdays<-rubi2[.indexwday(rubi2) %in% 1:5] #labels=c("Monday","Tuesday","Wednesday", "Thursday", "Friday")
w_days<-as.data.frame(dates=index(weekdays), coredata(weekdays))
w_days$dates<-NULL
names(w_days)<- c("date",c("wd0","wd1","wd2","wd3","wd4","wd5","wd6","wd7","wd8","wd9","wd10","wd11","wd12","wd13","wd14","wd15","wd16","wd17","wd18","wd19","wd20","wd21","wd22","wd23"))
w_days[,c(2:25)] <- lapply(w_days[,c(2:25)], as.character)
w_days[,c(2:25)] <- lapply(w_days[,c(2:25)], as.numeric)
w_days$date<-as.POSIXct(w_days$date)
w_days<-w_days[order(w_days$date),]
str(w_days)

perc_wd<-w_days[2:25]
perc_wd<-perc_wd*100
str(perc_wd)
#### Hierarchial clustering matrix data

##Distances: distance is choosen in dist() can be euclidean, manhattan etc...
p<-dist(perc_wd,method="euclidean")
memory.limit(size=8000)
##Linkages: linkage is chosen in hclust() can be ward.D, ward.D2, single, complete, average, median,...
p1<-hclust(p, method="ward.D2")                   #also equal to complete, single, ward.D, ward.D2


# Dendrogram
library(ggplot2)
library(ggdendro)  
ggdendrogram(p1, rotate = F, size = 3, labels=T)     ##  http://cran.r-project.org/web/packages/ggdendro/vignettes/ggdendro.html       


## Cluster by cutree()
clus <- as.data.frame(cutree((p1), k=5))  ## k indicates number of clusters      
names(clus)<- "clus_num"
clus
table(clus)    # to know how many are in each cluster

library(dendroextras)
d5<-colour_clusters(p1,5)
plot(d5)
d5g<-colour_clusters(p1,5,groupLabels=TRUE)
plot(d5g)



############PLOTTING
## if uses "rel_wdays_wends" print from 0 to 1; if uses "wdays_wends" print real power

p2<-as.data.frame(cbind(clus$clus_num, perc_wd)) 
names(p2)<-c("clus_num",c("wd0","wd1","wd2","wd3","wd4","wd5","wd6","wd7","wd8","wd9","wd10","wd11","wd12","wd13","wd14","wd15","wd16","wd17","wd18","wd19","wd20","wd21","wd22","wd23"))



cluster1<-p2[p2$clus_num==1,]
cluster2<-p2[p2$clus_num==2,]
cluster3<-p2[p2$clus_num==3,]
cluster4<-p2[p2$clus_num==4,]
cluster5<-p2[p2$clus_num==5,]

str(cluster1)


#### TESTING clusters by plotting       
library(ggplot2)
library(reshape2)
hour<-c(0:23)

##1. WEEKDAYS PLOT clusters 

#cluster1 
c_1wd<-as.data.frame(t(cluster1[,c(2:25)]))
c_1wd<-cbind(hour,c_1wd)
c_11wd<-melt(c_1wd, id.vars="hour")
cp1wd<-ggplot(c_11wd, aes(hour,value)) + geom_line(aes(colour = variable))+geom_smooth()+ggtitle("cluster1")#+ylim(0,0.15)
str(c_11wd)
#cluster2
c_2wd<-as.data.frame(t(cluster2[,c(2:25)]))
c_2wd<-cbind(hour,c_2wd)
c_21wd<-melt(c_2wd, id.vars="hour")
cp2wd<-ggplot(c_21wd, aes(hour,value)) + geom_line(aes(colour = variable))+geom_smooth()+ggtitle("cluster2")#+ylim(0,0.15)

#cluster3 
c_3wd<-as.data.frame(t(cluster3[,c(2:25)]))
c_3wd<-cbind(hour,c_3wd)
c_31wd<-melt(c_3wd, id.vars="hour")
cp3wd<-ggplot(c_31wd, aes(hour,value)) + geom_line(aes(colour = variable))+geom_smooth()+ggtitle("cluster3")#+ylim(0,0.15)

#cluster4 
c_4wd<-as.data.frame(t(cluster4[,c(2:25)]))
c_4wd<-cbind(hour,c_4wd)
c_41wd<-melt(c_4wd, id.vars="hour")
cp4wd<-ggplot(c_41wd, aes(hour,value)) + geom_line(aes(colour = variable))+geom_smooth()+ggtitle("cluster4") #+ylim(0,0.15)

#cluster5 
c_5wd<-as.data.frame(t(cluster5[,c(2:25)]))
c_5wd<-cbind(hour,c_5wd)
c_51wd<-melt(c_5wd, id.vars="hour")
cp5wd<-ggplot(c_51wd, aes(hour,value)) + geom_line(aes(colour = variable))+geom_smooth()+ggtitle("cluster5")#+ylim(0,0.15)

source("multiplot_function.R")
multiplot(cp1wd, cp2wd, cp3wd, cp4wd,cp5wd, cols=2)



#CLUSTERS MEAN WEEKDAYS
cluster1_mean<-as.data.frame(colMeans(cluster1[2:25]))
names(cluster1_mean)<-"mean_clus1"
cluster1_mean<-cbind(hour,cluster1_mean)
ggplot()+
  geom_line(data=c_11wd, aes(hour,value, colour=variable))+
  geom_line(data=cluster1_mean, aes(hour,mean_clus1),colour= "red", size = 1)


cluster2_mean<-as.data.frame(colMeans(cluster2[2:25]))
names(cluster2_mean)<-"mean_clus2"
cluster2_mean<-cbind(hour,cluster2_mean)
ggplot()+
  geom_line(data=c_21wd, aes(hour,value, colour=variable))+
  geom_line(data=cluster2_mean, aes(hour,mean_clus2),colour= "blue", size = 1)

cluster3_mean<-as.data.frame(colMeans(cluster3[2:25]))
names(cluster3_mean)<-"mean_clus3"
cluster3_mean<-cbind(hour,cluster3_mean)
ggplot()+
  geom_line(data=c_31wd, aes(hour,value, colour=variable))+
  geom_line(data=cluster3_mean, aes(hour,mean_clus3),colour= "orange", size = 1)


cluster4_mean<-as.data.frame(colMeans(cluster4[2:25]))
names(cluster4_mean)<-"mean_clus4"
cluster4_mean<-cbind(hour,cluster4_mean)
ggplot()+
  geom_line(data=c_41wd, aes(hour,value, colour=variable))+
  geom_line(data=cluster4_mean, aes(hour,mean_clus4),colour= "green", size = 1)


cluster5_mean<-as.data.frame(colMeans(cluster5[2:25]))
names(cluster5_mean)<-"mean_clus5"
cluster5_mean<-cbind(hour,cluster5_mean)
ggplot()+
  geom_line(data=c_51wd, aes(hour,value, colour=variable))+
  geom_line(data=cluster5_mean, aes(hour,mean_clus5),colour= "black", size = 1)


ggplot()+
  geom_line(data=cluster1_mean, aes(hour,mean_clus1),colour= "red", size = 1)+
  geom_line(data=cluster2_mean, aes(hour,mean_clus2),colour= "blue", size = 1)+
  geom_line(data=cluster3_mean, aes(hour,mean_clus3),colour= "orange", size = 1)+
  geom_line(data=cluster4_mean, aes(hour,mean_clus4),colour= "green", size = 1)+
  geom_line(data=cluster5_mean, aes(hour,mean_clus5),colour= "black", size = 1)



#CLUSTERS MEAN WEEKENDS
cluster1_mean_we<-as.data.frame(colMeans(cluster1[26:49]))
names(cluster1_mean_we)<-"mean_clus1_we"
cluster1_mean_we<-cbind(hour,cluster1_mean_we)
ggplot()+
  geom_line(data=c_11we, aes(hour,value, colour=variable))+
  geom_line(data=cluster1_mean_we, aes(hour,mean_clus1_we),colour= "red", size = 1)


cluster2_mean_we<-as.data.frame(colMeans(cluster2[26:49]))
names(cluster2_mean_we)<-"mean_clus2_we"
cluster2_mean_we<-cbind(hour,cluster2_mean_we)
ggplot()+
  geom_line(data=c_21we, aes(hour,value, colour=variable))+
  geom_line(data=cluster2_mean_we, aes(hour,mean_clus2_we),colour= "blue", size = 1)

cluster3_mean_we<-as.data.frame(colMeans(cluster3[26:49]))
names(cluster3_mean_we)<-"mean_clus3_we"
cluster3_mean_we<-cbind(hour,cluster3_mean_we)
ggplot()+
  geom_line(data=c_31we, aes(hour,value, colour=variable))+
  geom_line(data=cluster3_mean_we, aes(hour,mean_clus3_we),colour= "orange", size = 1)


cluster4_mean_we<-as.data.frame(colMeans(cluster4[26:49]))
names(cluster4_mean_we)<-"mean_clus4_we"
cluster4_mean_we<-cbind(hour,cluster4_mean_we)
ggplot()+
  geom_line(data=c_41we, aes(hour,value, colour=variable))+
  geom_line(data=cluster4_mean_we, aes(hour,mean_clus4_we),colour= "green", size = 1)


cluster5_mean_we<-as.data.frame(colMeans(cluster5[26:49]))
names(cluster5_mean_we)<-"mean_clus5_we"
cluster5_mean_we<-cbind(hour,cluster5_mean_we)
ggplot()+
  geom_line(data=c_51we, aes(hour,value, colour=variable))+
  geom_line(data=cluster5_mean_we, aes(hour,mean_clus5_we),colour= "black", size = 1)

all_we<-
  ggplot()+
  geom_line(data=cluster1_mean_we, aes(hour,mean_clus1_we),colour= "red", size = 1)+
  geom_line(data=cluster2_mean_we, aes(hour,mean_clus2_we),colour= "blue", size = 1)+
  geom_line(data=cluster3_mean_we, aes(hour,mean_clus3_we),colour= "orange", size = 1)+
  geom_line(data=cluster4_mean_we, aes(hour,mean_clus4_we),colour= "green", size = 1)+
  geom_line(data=cluster5_mean_we, aes(hour,mean_clus5_we),colour= "black", size = 1)

source("multiplot_function.R")
multiplot(all_wd,all_we)

