

####  Import database
setwd("~/SELECT ERASMUS MUNDUS/ENERBYTE- Thesis&internship/THESIS - Enerbyte/Part 3. Data analysis/R for Tesi/test_01_tesi")
rubi <- read.csv("~/SELECT ERASMUS MUNDUS/ENERBYTE- Thesis&internship/THESIS - Enerbyte/Part 3. Data analysis/R for Tesi/test_01_tesi/data_id_selection2.csv", sep=";", stringsAsFactors=FALSE)
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

## no distances between points is needed to calculate, as kmeans is based centroid mininimu square 

fit <- kmeans(hour_percent, 7,iter.max=100,nstart=100) # 5 cluster solution
fit
clus_num<-fit$cluster
p1<-as.data.frame(clus_num)
p2<-cbind(p1,hour_percent)
names(p2)<-c("clus_num",c(0:23))


#### Plotting clusters lines


cluster1<-p2[p2$clus_num==1,]
cluster2<-p2[p2$clus_num==2,]
cluster3<-p2[p2$clus_num==3,]
cluster4<-p2[p2$clus_num==4,]
cluster5<-p2[p2$clus_num==5,]

cluster6<-p2[p2$clus_num==6,]
cluster7<-p2[p2$clus_num==7,]
cluster8<-p2[p2$clus_num==8,]
cluster9<-p2[p2$clus_num==9,]

## TESTING clusters by plotting       
library(reshape2)
hour<-c(0:23)

#cluster1 
c_1<-as.data.frame(t(cluster1[,c(2:25)]))
c_1<-cbind(hour,c_1)
c_11<-melt(c_1, id.vars="hour")
cp1<-ggplot(c_11, aes(hour,value)) + geom_line(aes(colour = variable))+geom_smooth()+ggtitle("cluster1")+ylim(0,0.125)

#cluster2 
c_2<-as.data.frame(t(cluster2[,c(2:25)]))
c_2<-cbind(hour,c_2)
c_21<-melt(c_2, id.vars="hour")
cp2<-ggplot(c_21, aes(hour,value)) + geom_line(aes(colour = variable))+geom_smooth()+ggtitle("cluster2")+ylim(0,0.125)

#cluster3 
c_3<-as.data.frame(t(cluster3[,c(2:25)]))
c_3<-cbind(hour,c_3)
c_31<-melt(c_3, id.vars="hour")
cp3<-ggplot(c_31, aes(hour,value)) + geom_line(aes(colour = variable))+geom_smooth()+ggtitle("cluster3")+ylim(0,0.125)

#cluster4 
c_4<-as.data.frame(t(cluster4[,c(2:25)]))
c_4<-cbind(hour,c_4)
c_41<-melt(c_4, id.vars="hour")
cp4<-ggplot(c_41, aes(hour,value)) + geom_line(aes(colour = variable))+geom_smooth()+ggtitle("cluster4") +ylim(0,0.125)

#cluster5
c_5<-as.data.frame(t(cluster5[,c(2:25)]))
c_5<-cbind(hour,c_5)
c_51<-melt(c_5, id.vars="hour")
cp5<-ggplot(c_51, aes(hour,value)) + geom_line(aes(colour = variable))+geom_smooth()+ggtitle("cluster5")+ylim(0,0.125)

#cluster6
c_6<-as.data.frame(t(cluster6[,c(2:25)]))
c_6<-cbind(hour,c_6)
c_61<-melt(c_6, id.vars="hour")
cp6<-ggplot(c_61, aes(hour,value)) + geom_line(aes(colour = variable))+geom_smooth()+ggtitle("cluster6")+ylim(0,0.125)

#cluster7
c_7<-as.data.frame(t(cluster7[,c(2:25)]))
c_7<-cbind(hour,c_7)
c_71<-melt(c_7, id.vars="hour")
cp7<-ggplot(c_71, aes(hour,value)) + geom_line(aes(colour = variable))+geom_smooth()+ggtitle("cluster7")+ylim(0,0.125)

#cluster8
c_8<-as.data.frame(t(cluster8[,c(2:25)]))
c_8<-cbind(hour,c_8)
c_81<-melt(c_8, id.vars="hour")
cp8<-ggplot(c_81, aes(hour,value)) + geom_line(aes(colour = variable))+geom_smooth()+ggtitle("cluster8")+ylim(0,0.125)

#cluster9
c_9<-as.data.frame(t(cluster9[,c(2:25)]))
c_9<-cbind(hour,c_9)
c_91<-melt(c_9, id.vars="hour")
cp9<-ggplot(c_91, aes(hour,value)) + geom_line(aes(colour = variable))+geom_smooth()+ggtitle("cluster9")+ylim(0,0.125)


source("multiplot_function.R")
multiplot(cp1, cp2, cp3, cp4,cp5,cp6,cp7, cols=3)
multiplot(cp1, cp2, cp3, cols=2)


## Plotting the clusters MEAN

#cluster1   
cluster1_mean<-as.data.frame(colMeans(cluster1[2:25]))
names(cluster1_mean)<-"mean_clus1"
cluster1_mean<-cbind(hour,cluster1_mean)
ggplot()+
  geom_line(data=c_11, aes(hour,value, colour=variable))+
  geom_line(data=cluster1_mean, aes(hour,mean_clus1),colour= "red", size = 1)

#cluster2 
cluster2_mean<-as.data.frame(colMeans(cluster2[2:25]))
names(cluster2_mean)<-"mean_clus2"
cluster2_mean<-cbind(hour,cluster2_mean)
ggplot()+
  geom_line(data=c_21, aes(hour,value, colour=variable))+
  geom_line(data=cluster2_mean, aes(hour,mean_clus2),colour= "blue", size = 1)

#cluster 3
cluster3_mean<-as.data.frame(colMeans(cluster3[2:25]))
names(cluster3_mean)<-"mean_clus3"
cluster3_mean<-cbind(hour,cluster3_mean)
ggplot()+
  geom_line(data=c_31, aes(hour,value, colour=variable))+
  geom_line(data=cluster3_mean, aes(hour,mean_clus3),colour= "orange", size = 1)

# cluster4
cluster4_mean<-as.data.frame(colMeans(cluster4[2:25]))
names(cluster4_mean)<-"mean_clus4"
cluster4_mean<-cbind(hour,cluster4_mean)
ggplot()+
  geom_line(data=c_41, aes(hour,value, colour=variable))+
  geom_line(data=cluster4_mean, aes(hour,mean_clus4),colour= "green", size = 1)

# cluster5
cluster5_mean<-as.data.frame(colMeans(cluster5[2:25]))
names(cluster5_mean)<-"mean_clus5"
cluster5_mean<-cbind(hour,cluster5_mean)
ggplot()+
  geom_line(data=c_51, aes(hour,value, colour=variable))+
  geom_line(data=cluster5_mean, aes(hour,mean_clus5),colour= "black", size = 1)

# cluster6
cluster6_mean<-as.data.frame(colMeans(cluster6[2:25]))
names(cluster6_mean)<-"mean_clus6"
cluster6_mean<-cbind(hour,cluster6_mean)
ggplot()+
  geom_line(data=c_61, aes(hour,value, colour=variable))+
  geom_line(data=cluster6_mean, aes(hour,mean_clus6),colour= "purple", size = 1)

# cluster7
cluster7_mean<-as.data.frame(colMeans(cluster7[2:25]))
names(cluster7_mean)<-"mean_clus7"
cluster7_mean<-cbind(hour,cluster7_mean)
ggplot()+
  geom_line(data=c_71, aes(hour,value, colour=variable))+
  geom_line(data=cluster7_mean, aes(hour,mean_clus7),colour= "brown", size = 1)


# cluster8
cluster8_mean<-as.data.frame(colMeans(cluster8[2:25]))
names(cluster8_mean)<-"mean_clus8"
cluster8_mean<-cbind(hour,cluster8_mean)
ggplot()+
  geom_line(data=c_81, aes(hour,value, colour=variable))+
  geom_line(data=cluster8_mean, aes(hour,mean_clus8),colour= "grey", size = 1)


# cluster9
cluster9_mean<-as.data.frame(colMeans(cluster9[2:25]))
names(cluster9_mean)<-"mean_clus9"
cluster9_mean<-cbind(hour,cluster9_mean)
ggplot()+
  geom_line(data=c_91, aes(hour,value, colour=variable))+
  geom_line(data=cluster9_mean, aes(hour,mean_clus9),colour= "yellow", size = 1)

##all clusters

ggplot()+
  geom_line(data=cluster1_mean, aes(hour,mean_clus1),colour= "red", size = 1)+
  geom_line(data=cluster2_mean, aes(hour,mean_clus2),colour= "blue", size = 1)+
  geom_line(data=cluster3_mean, aes(hour,mean_clus3),colour= "orange", size = 1)+
  geom_line(data=cluster4_mean, aes(hour,mean_clus4),colour= "green", size = 1)+
  geom_line(data=cluster5_mean, aes(hour,mean_clus5),colour= "black", size = 1)+
  geom_line(data=cluster6_mean, aes(hour,mean_clus6),colour= "purple", size = 1)+
  geom_line(data=cluster7_mean, aes(hour,mean_clus7),colour= "brown", size = 1)+
  geom_line(data=cluster8_mean, aes(hour,mean_clus8),colour= "grey", size = 1)+
  geom_line(data=cluster9_mean, aes(hour,mean_clus9),colour= "yellow", size = 1)


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

cluster8_mean<-cbind(cluster8_mean,rep(c(8)))
names(cluster8_mean)<-(c("hour","mean","clus_num"))

cluster9_mean<-cbind(cluster9_mean,rep(c(9)))
names(cluster9_mean)<-(c("hour","mean","clus_num"))

by_clus_mean<-rbind(cluster1_mean,cluster2_mean,cluster3_mean,cluster4_mean,cluster5_mean,cluster6_mean,cluster7_mean)
##2. plot the 5 different cluster in 5 wrap facets
library(ggplot2)
ggplot(by_clus_mean,aes(hour,mean))+geom_line(aes(colour=clus_num))+facet_wrap(~clus_num)#+scale_colour_gradientn(colours=n(6))
