
####  Import database
setwd("~/SELECT ERASMUS MUNDUS/ENERBYTE- Thesis&internship/THESIS - Enerbyte/Chapter 2. Case study Data analysis/R for Tesi/test_01_tesi")
rubi <- read.csv("~/SELECT ERASMUS MUNDUS/ENERBYTE- Thesis&internship/THESIS - Enerbyte/Chapter 2. Case study Data analysis/R for Tesi/test_01_tesi/data_id_selection.csv", sep=";", stringsAsFactors=FALSE)
#rubi<-rubi[(rubi$source == "CURRENTCOST"),]
rubi$source <-NULL
rubi$idmeter<-as.numeric(rubi$idmeter)
rubi$date<-as.POSIXct((rubi$date), format="%d/%m/%Y")
rubi1<-rubi[order(as.numeric(rubi$idmeter),rubi$date),]
str(rubi1)
a<-as.data.frame(unique(rubi1$idmeter))

###1. Get the sum of each row 
rubi_cut<-rubi[3:26]
rubi_zero<-rubi[apply(rubi_cut==0, 1, sum)<=0,]

### 2. frozens
x<-rubi_zero[3:26]
y<-x[-1]
diff <- y-x[1:length(x)-1]
rubi_net<-rubi_zero[apply(diff==0,1,sum)<=2,]               ## more than six 0 per row, remove the row

names(rubi_net)<- c("idmeter","date","00:00","01:00","02:00","03:00","04:00","05:00","06:00","07:00","08:00","09:00","10:00","11:00","12:00","13:00","14:00","15:00","16:00","17:00","18:00","19:00","20:00","21:00","22:00","23:00")
str(rubi_net)
b<-as.data.frame(unique(rubi_net$idmeter))

####Separate Weekday and Weekends
library(xts)
rubi2<-as.xts(rubi_net,rubi_net$date)

## Weekdays
weekdays<-rubi2[.indexwday(rubi2) %in% 1:5] #labels=c("Monday","Tuesday","Wednesday", "Thursday", "Friday")
w_days<-as.data.frame(dates=index(weekdays), coredata(weekdays))
w_days$date<-NULL
names(w_days)<- c("idmeter",c("wd0","wd1","wd2","wd3","wd4","wd5","wd6","wd7","wd8","wd9","wd10","wd11","wd12","wd13","wd14","wd15","wd16","wd17","wd18","wd19","wd20","wd21","wd22","wd23"))
w_days[,c(2:25)] <- lapply(w_days[,c(2:25)], as.character)
w_days[,c(2:25)] <- lapply(w_days[,c(2:25)], as.numeric)
#w_days$date<-as.POSIXct(w_days$date)
w_days$idmeter<-as.numeric(as.character(w_days$idmeter))
w_days<-w_days[order(w_days$idmeter),]
str(w_days)
c<-as.data.frame(unique(w_days$idmeter))


####  PERCENTAGES (OPOWER) OPTION 2: CURVES ARCHETYPES
## RowSUms, Division, ColMeans --> MORE ACCURATE!

###1. Get the sum of each row 
rubi_sum<-w_days[2:25]
row_sum<-as.matrix(rowSums(rubi_sum))
names(row_sum)<-"sum"

###2. Division to get the percentages per hour  
division<-as.data.frame(rubi_sum/row_sum)
#division$idmeter<-NULL
division_id<-cbind(w_days$idmeter,division)
names(division_id)<-c("idmeter","00:00","01:00","02:00","03:00","04:00","05:00","06:00","07:00","08:00","09:00","10:00","11:00","12:00","13:00","14:00","15:00","16:00","17:00","18:00","19:00","20:00","21:00","22:00","23:00")  
division_id<-as.data.frame(division_id)
#test<-as.matrix(rowSums(division_id[2:25]))

###3. Column means
cast_99<-as.data.frame(lapply(split(division_id, division_id$idmeter),colMeans))
cast100<-as.data.frame(t(cast_99))
cast100$idmeter<-NULL


hour_percent<-cast100

#perc_wd<-w_days[2:25]
#perc_wd<-perc_wd*100
#str(perc_wd)


##Distances: distance is choosen in dist() can be euclidean, manhattan etc...
p<-dist(hour_percent,method="manhattan")


##Linkages: linkage is chosen in hclust() can be ward.D, ward.D2, single, complete, average, median,...
p1<-hclust(p, method="ward.D")                   #also equal to complete, single, ward.D, ward.D2


## Dendrogram
library(ggplot2)
library(ggdendro)  
ggdendrogram(p1, rotate = F, size = 3, labels=T)     ##  http://cran.r-project.org/web/packages/ggdendro/vignettes/ggdendro.html       


## Cluster by cutree()
clus <- as.data.frame(cutree((p1), k=7))  ## k indicates number of clusters      
names(clus)<- "clus_num"
clus
table(clus)    # to know how many are in each cluster


library(dendroextras)
d5<-colour_clusters(p1,7)
plot(d5)
d5g<-colour_clusters(p1,7,groupLabels=TRUE)
plot(d5g)

## circular plotting
library(ape)
plot(as.phylo(p1), type = "fan")

## Heatmap--> need to improve
q<-heatmap(as.matrix(hour_percent))




#### Plotting clusters lines

p2<-as.data.frame(cbind(clus$clus_num,hour_percent)) 
names(p2)<-c("clus_num",c(0:23))

cluster1<-p2[p2$clus_num==1,]
cluster2<-p2[p2$clus_num==2,]
cluster3<-p2[p2$clus_num==3,]
cluster4<-p2[p2$clus_num==4,]
cluster5<-p2[p2$clus_num==5,]
cluster6<-p2[p2$clus_num==6,]
cluster7<-p2[p2$clus_num==7,]



## TESTING clusters by plotting       
library(reshape2)
hour<-c(0:23)

#cluster1 
c_1<-as.data.frame(t(cluster1[,c(2:25)]))
c_1<-cbind(hour,c_1)
c_11<-melt(c_1, id.vars="hour")
cp1<-
  ggplot(c_11, aes(hour,value)) + geom_line(aes(colour = variable))+ggtitle("cluster1")+ylim(0,0.15)
theme_set(theme_gray(base_size = 26))
#cluster2 
c_2<-as.data.frame(t(cluster2[,c(2:25)]))
c_2<-cbind(hour,c_2)
c_21<-melt(c_2, id.vars="hour")
cp2<-
  ggplot(c_21, aes(hour,value)) + geom_line(aes(colour = variable))+ggtitle("cluster2")+ylim(0,0.15)

#cluster3 
c_3<-as.data.frame(t(cluster3[,c(2:25)]))
c_3<-cbind(hour,c_3)
c_31<-melt(c_3, id.vars="hour")
cp3<-
  ggplot(c_31, aes(hour,value)) + geom_line(aes(colour = variable))+ggtitle("cluster3")+ylim(0,0.15)

#cluster4 
c_4<-as.data.frame(t(cluster4[,c(2:25)]))
c_4<-cbind(hour,c_4)
c_41<-melt(c_4, id.vars="hour")
cp4<-
  ggplot(c_41, aes(hour,value)) + geom_line(aes(colour = variable))+ggtitle("cluster4") +ylim(0,0.15)

#cluster5
c_5<-as.data.frame(t(cluster5[,c(2:25)]))
c_5<-cbind(hour,c_5)
c_51<-melt(c_5, id.vars="hour")
cp5<-
  ggplot(c_51, aes(hour,value)) + geom_line(aes(colour = variable))+ggtitle("cluster5")+ylim(0,0.15)

#cluster6
c_6<-as.data.frame(t(cluster6[,c(2:25)]))
c_6<-cbind(hour,c_6)
c_61<-melt(c_6, id.vars="hour")
cp6<-
  ggplot(c_61, aes(hour,value)) + geom_line(aes(colour = variable))+ggtitle("cluster6")+ylim(0,0.15)

#cluster7
c_7<-as.data.frame(t(cluster7[,c(2:25)]))
c_7<-cbind(hour,c_7)
c_71<-melt(c_7, id.vars="hour")
cp7<-
  ggplot(c_71, aes(hour,value)) + geom_line(aes(colour = variable))+ggtitle("cluster7")+ylim(0,0.15)

source("multiplot_function.R")
multiplot(cp1, cp2, cp3, cp4,cp5,cp6,cp7, cols=3)



## Plotting the clusters MEAN

#cluster1   
cluster1_mean<-as.data.frame(colMeans(cluster1[2:25]))
names(cluster1_mean)<-"mean_clus1"
c1_mean<-as.data.frame(t(cluster1_mean))

cluster1_mean<-cbind(hour,cluster1_mean)
ggplot()+
  geom_line(data=c_11, aes(hour,value, colour=variable))+
  geom_line(data=cluster1_mean, aes(hour,mean_clus1),colour= "red", size = 1)

#cluster2 
cluster2_mean<-as.data.frame(colMeans(cluster2[2:25]))
names(cluster2_mean)<-"mean_clus2"
c2_mean<-as.data.frame(t(cluster2_mean))
cluster2_mean<-cbind(hour,cluster2_mean)
ggplot()+
  geom_line(data=c_21, aes(hour,value, colour=variable))+
  geom_line(data=cluster2_mean, aes(hour,mean_clus2),colour= "blue", size = 1)

#cluster 3
cluster3_mean<-as.data.frame(colMeans(cluster3[2:25]))
names(cluster3_mean)<-"mean_clus3"
c3_mean<-as.data.frame(t(cluster3_mean))
cluster3_mean<-cbind(hour,cluster3_mean)
ggplot()+
  geom_line(data=c_31, aes(hour,value, colour=variable))+
  geom_line(data=cluster3_mean, aes(hour,mean_clus3),colour= "orange", size = 1)

# cluster4
cluster4_mean<-as.data.frame(colMeans(cluster4[2:25]))
names(cluster4_mean)<-"mean_clus4"
c4_mean<-as.data.frame(t(cluster4_mean))
cluster4_mean<-cbind(hour,cluster4_mean)
ggplot()+
  geom_line(data=c_41, aes(hour,value, colour=variable))+
  geom_line(data=cluster4_mean, aes(hour,mean_clus4),colour= "green", size = 1)

# cluster5
cluster5_mean<-as.data.frame(colMeans(cluster5[2:25]))
names(cluster5_mean)<-"mean_clus5"
c5_mean<-as.data.frame(t(cluster5_mean))
cluster5_mean<-cbind(hour,cluster5_mean)
ggplot()+
  geom_line(data=c_51, aes(hour,value, colour=variable))+
  geom_line(data=cluster5_mean, aes(hour,mean_clus5),colour= "black", size = 1)

# cluster6
cluster6_mean<-as.data.frame(colMeans(cluster6[2:25]))
names(cluster6_mean)<-"mean_clus6"
c6_mean<-as.data.frame(t(cluster6_mean))
cluster6_mean<-cbind(hour,cluster6_mean)
ggplot()+
  geom_line(data=c_61, aes(hour,value, colour=variable))+
  geom_line(data=cluster6_mean, aes(hour,mean_clus6),colour= "purple", size = 1)

# cluster7
cluster7_mean<-as.data.frame(colMeans(cluster7[2:25]))
names(cluster7_mean)<-"mean_clus7"
c7_mean<-as.data.frame(t(cluster7_mean))
cluster7_mean<-cbind(hour,cluster7_mean)
ggplot()+
  geom_line(data=c_71, aes(hour,value, colour=variable))+
  geom_line(data=cluster7_mean, aes(hour,mean_clus7),colour= "black", size = 1)
##all clusters



ggplot()+
  geom_line(data=cluster1_mean, aes(hour,mean_clus1),colour= "red", size = 1)+
  geom_line(data=cluster2_mean, aes(hour,mean_clus2),colour= "blue", size = 1)+
  geom_line(data=cluster3_mean, aes(hour,mean_clus3),colour= "orange", size = 1)+
  geom_line(data=cluster4_mean, aes(hour,mean_clus4),colour= "green", size = 1)+
  geom_line(data=cluster5_mean, aes(hour,mean_clus5),colour= "black", size = 1)+
  geom_line(data=cluster6_mean, aes(hour,mean_clus6),colour= "purple", size = 1)+
  geom_line(data=cluster7_mean, aes(hour,mean_clus7),colour= "yellow", size = 1)


##merging cluster to the same dataframe

cluster1_mean<-cbind(cluster1_mean,rep(c(1)))
names(cluster1_mean)<-(c("hour","mean","clus_num"))

cluster2_mean<-cbind(cluster2_mean,rep(c(2)))
names(cluster2_mean)<-(c("hour","mean","clus_num"))

cluster3_mean<-cbind(cluster3_mean,rep(c(3)))
names(cluster3_mean)<-(c("hour","mean","clus_num"))

cluster4_mean<-cbind(cluster4_mean,rep(c(4)))
names(cluster4_mean)<-(c("hour","mean","clus_num"))

cluster5_mean<-cbind(cluster5_mean,rep(c(5)))
names(cluster5_mean)<-(c("hour","mean","clus_num"))

cluster6_mean<-cbind(cluster6_mean,rep(c(6)))
names(cluster6_mean)<-(c("hour","mean","clus_num"))

cluster7_mean<-cbind(cluster7_mean,rep(c(7)))
names(cluster7_mean)<-(c("hour","mean","clus_num"))

by_clus_mean<-rbind(cluster1_mean,cluster2_mean,cluster3_mean,cluster4_mean,cluster5_mean,cluster6_mean,cluster7_mean)
##2. plot the 5 different cluster in 5 wrap facets
library(ggplot2)
ggplot(by_clus_mean,aes(hour,mean))+geom_line(aes(colour=clus_num))+facet_wrap(~clus_num)+
  ylim(0,0.10)


##residus per cluster
all_means<-rbind(c1_mean,c2_mean,c3_mean,c4_mean,c5_mean,c6_mean,c7_mean)
#1
c1_res<-as.data.frame(cluster1[,2:25])
ax1<-data.frame()
id1<-1:nrow(cluster1)
for (i in id1){
  ax1 <- rbind(ax1,(c1_res[i,] - c1_mean))
}
ax1<-abs(ax1)
ax1_rs<-as.data.frame(rowSums(ax1))
ax1_m<-as.data.frame(colMeans(ax1_rs))
names(ax1_m)<-"dist"

#2
c2_res<-as.data.frame(cluster2[,2:25])
ax2<-data.frame()
id2<-1:nrow(cluster2)
for (i in id2){
  ax2 <- rbind(ax2,(c2_res[i,] - c2_mean))
}
ax2<-abs(ax2)
ax2_rs<-as.data.frame(rowSums(ax2))
ax2_m<-as.data.frame(colMeans(ax2_rs))
names(ax2_m)<-"dist"
#3
c3_res<-as.data.frame(cluster3[,2:25])
ax3<-data.frame()
id3<-1:nrow(cluster3)
for (i in id3){
  ax3 <- rbind(ax3,(c3_res[i,] - c3_mean))
}
ax3<-abs(ax3)
ax3_rs<-as.data.frame(rowSums(ax3))
ax3_m<-as.data.frame(colMeans(ax3_rs))
names(ax3_m)<-"dist"
#4
c4_res<-as.data.frame(cluster4[,2:25])
ax4<-data.frame()
id4<-1:nrow(cluster4)
for (i in id4){
  ax4 <- rbind(ax4,(c4_res[i,] - c4_mean))
}
ax4<-abs(ax4)
ax4_rs<-as.data.frame(rowSums(ax4))
ax4_m<-as.data.frame(colMeans(ax4_rs))
names(ax4_m)<-"dist"
#5
c5_res<-as.data.frame(cluster5[,2:25])
ax5<-data.frame()
id5<-1:nrow(cluster5)
for (i in id5){
  ax5 <- rbind(ax5,(c5_res[i,] - c5_mean))
}
ax5<-abs(ax5)
ax5_rs<-as.data.frame(rowSums(ax5))
ax5_m<-as.data.frame(colMeans(ax5_rs))
names(ax5_m)<-"dist"
#6
c6_res<-as.data.frame(cluster6[,2:25])
ax6<-data.frame()
id6<-1:nrow(cluster6)
for (i in id6){
  ax6 <- rbind(ax6,(c6_res[i,] - c6_mean))
}
ax6<-abs(ax6)
ax6_rs<-as.data.frame(rowSums(ax6))
ax6_m<-as.data.frame(colMeans(ax6_rs))
names(ax6_m)<-"dist"

#7
c7_res<-as.data.frame(cluster7[,2:25])
ax7<-data.frame()
id7<-1:nrow(cluster7)
for (i in id7){
  ax7 <- rbind(ax7,(c7_res[i,] - c7_mean))
}
ax7<-abs(ax7)
ax7_rs<-as.data.frame(rowSums(ax7))
ax7_m<-as.data.frame(colMeans(ax7_rs))
names(ax7_m)<-"dist"


alls_m<-as.data.frame(rbind(ax1_m,ax2_m,ax3_m,ax4_m,ax5_m,ax6_m,ax7_m))



