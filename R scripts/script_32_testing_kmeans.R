####  Import database
setwd("~/SELECT ERASMUS MUNDUS/ENERBYTE- Thesis&internship/THESIS - Enerbyte/Part 3. Data analysis/R for Tesi/test_01_tesi")
rubi <- read.csv("~/SELECT ERASMUS MUNDUS/ENERBYTE- Thesis&internship/THESIS - Enerbyte/Part 3. Data analysis/R for Tesi/test_01_tesi/meter_rubi_wd_we.csv", sep=";", stringsAsFactors=FALSE)
rubi<-rubi[(rubi$source == "CURRENTCOST"),]
rubi$source <-NULL
rubi$idmeter<-as.numeric(rubi$idmeter)
rubi$date <-NULL


####  PERCENTAGES (OPOWER) OPTION 2: CURVES ARCHETYPES
## RowSUms, Division, ColMeans --> MORE ACCURATE!

###1. Get the sum of each row 
rubi_net<-rubi[2:25]
row_sub <- apply(rubi_net, 1, function(row) all(row !=0 )) #Go through each row and determine if a value is zero
rubi_net<- rubi_net[row_sub,]   #Subset as usual
row_sum<-as.matrix(rowSums(rubi_net))
names(row_sum)<-"sum"
rubi<-rubi[row_sub,]

###2. Division to get the percentages per hor  
division<-as.data.frame(rubi_net/row_sum)
division$idmeter<-NULL
division_id<-cbind(rubi$idmeter,division)
names(division_id)<-c("idmeter", c(0:23))   
division_id<-as.data.frame(division_id)
test<-as.matrix(rowSums(division_id[2:25]))

###3. Column means
cast_99<-as.data.frame(lapply(split(division_id, division_id$idmeter),colMeans))
cast100<-as.data.frame(t(cast_99))
cast100$idmeter<-NULL


hour_percent<-cast100



#### K-means clustering

  ##plot to determine the distance sum of square within the groups!
wssplot <- function(data, nc, seed){
          wss <- (nrow(data)-1)*sum(apply(data,2,var))
          for (i in 2:nc){
              set.seed(seed)
              wss[i] <- sum(kmeans(data, centers=i)$withinss)}
        plot(1:nc, wss, type="b", xlab="Number of Clusters",
        ylab="Within groups sum of squares")}

wssplot(hour_percent,15,1234)

hp<-scale(hour_percent[-1]) 

library(NbClust)
  set.seed(1234)
  nc <- NbClust(hp, method="kmeans")
  table(nc$Best.n[1,])

barplot(table(nc$Best.n[1,]),
        xlab="Numer of Clusters", ylab="Number of Criteria",
        main="Number of Clusters Chosen by 26 Criteria")

set.seed(1234)
fit.km <- kmeans(hour_percent, 3, nstart=25)                           
fit.km$size
fit.km$centers

aggregate(hour_percent[-1], by=list(cluster=fit.km$cluster), mean)


## no distances between points is needed to calculate, as kmeans is based centroid mininimu square 

fit <- kmeans(hour_percent, 9,iter.max=100,nstart=143) # 5 cluster solution
fit
clus_num<-fit$cluster
p1<-as.data.frame(clus_num)
p2<-cbind(p1,hour_percent)
names(p2)<-c("clus_num",c(0:23))




cluster1<-p2[p2$clus_num==1,]
cluster2<-p2[p2$clus_num==2,]
cluster3<-p2[p2$clus_num==3,]
cluster4<-p2[p2$clus_num==4,]
cluster5<-p2[p2$clus_num==5,]
cluster6<-p2[p2$clus_num==6,]
cluster7<-p2[p2$clus_num==7,]
cluster8<-p2[p2$clus_num==8,]
cluster9<-p2[p2$clus_num==9,]

##TESTING clusters by plotting 
library(ggplot2)
#cluster1 són 32 samples
c_1<-as.data.frame(t(cluster1[,c(2:25)]))
hour<-c(0:23)
c_1<-cbind(hour,c_1)
library(reshape2)
c_11<-melt(c_1, id.vars="hour")
cp1<-ggplot(c_11, aes(hour,value)) + geom_line(aes(colour = variable))+geom_smooth()+ggtitle("cluster1")

#cluster2 són 42 samples
c_2<-as.data.frame(t(cluster2[,c(2:25)]))
hour<-c(0:23)
c_2<-cbind(hour,c_2)
library(reshape2)
c_21<-melt(c_2, id.vars="hour")
cp2<-ggplot(c_21, aes(hour,value)) + geom_line(aes(colour = variable))+geom_smooth()+ggtitle("cluster2")

#cluster3 són 53 samples
c_3<-as.data.frame(t(cluster3[,c(2:25)]))
hour<-c(0:23)
c_3<-cbind(hour,c_3)
library(reshape2)
c_31<-melt(c_3, id.vars="hour")
cp3<-ggplot(c_31, aes(hour,value)) + geom_line(aes(colour = variable))+geom_smooth()+ggtitle("cluster3")

#cluster4 són 6 samples
c_4<-as.data.frame(t(cluster4[,c(2:25)]))
hour<-c(0:23)
c_4<-cbind(hour,c_4)
library(reshape2)
c_41<-melt(c_4, id.vars="hour")
cp4<-ggplot(c_41, aes(hour,value)) + geom_line(aes(colour = variable))+geom_smooth()+ggtitle("cluster4")      

#cluster5 són 11 samples
c_5<-as.data.frame(t(cluster5[,c(2:25)]))
hour<-c(0:23)
c_5<-cbind(hour,c_5)
library(reshape2)
c_51<-melt(c_5, id.vars="hour")
cp5<-ggplot(c_51, aes(hour,value)) + geom_line(aes(colour = variable))+geom_smooth()+ggtitle("cluster5")

#cluster6 són 11 samples
c_6<-as.data.frame(t(cluster6[,c(2:25)]))
hour<-c(0:23)
c_6<-cbind(hour,c_6)
library(reshape2)
c_61<-melt(c_6, id.vars="hour")
cp6<-ggplot(c_61, aes(hour,value)) + geom_line(aes(colour = variable))+geom_smooth()+ggtitle("cluster6")

#cluster7 són 11 samples
c_7<-as.data.frame(t(cluster7[,c(2:25)]))
hour<-c(0:23)
c_7<-cbind(hour,c_7)
library(reshape2)
c_71<-melt(c_7, id.vars="hour")
cp7<-ggplot(c_71, aes(hour,value)) + geom_line(aes(colour = variable))+geom_smooth()+ggtitle("cluster7")

#cluster8 són 11 samples
c_8<-as.data.frame(t(cluster8[,c(2:25)]))
hour<-c(0:23)
c_8<-cbind(hour,c_8)
library(reshape2)
c_81<-melt(c_8, id.vars="hour")
cp8<-ggplot(c_81, aes(hour,value)) + geom_line(aes(colour = variable))+geom_smooth()+ggtitle("cluster8")

#cluster8 són 11 samples
c_9<-as.data.frame(t(cluster9[,c(2:25)]))
hour<-c(0:23)
c_9<-cbind(hour,c_9)
library(reshape2)
c_91<-melt(c_9, id.vars="hour")
cp9<-ggplot(c_91, aes(hour,value)) + geom_line(aes(colour = variable))+geom_smooth()+ggtitle("cluster9")



source("multiplot_function.R")
multiplot(cp1, cp2, cp3, cp4,cp5,cp6,cp7,cp8,cp9, cols=3)