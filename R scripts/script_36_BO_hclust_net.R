
####  Import database
setwd("~/SELECT ERASMUS MUNDUS/ENERBYTE- Thesis&internship/THESIS - Enerbyte/Chapter 2. Case study Data analysis/R for Tesi/test_01_tesi")
  rubi <- read.csv("~/SELECT ERASMUS MUNDUS/ENERBYTE- Thesis&internship/THESIS - Enerbyte/Chapter 2. Case study Data analysis/R for Tesi/test_01_tesi/data_id_selection.csv", sep=";", stringsAsFactors=FALSE)
    #rubi<-rubi[(rubi$source == "CURRENTCOST"),]
      rubi$source <-NULL
        rubi$idmeter<-as.numeric(rubi$idmeter)
          #rubi$date <-NULL
a<-as.data.frame(unique(rubi$idmeter))

###1. Get the sum of each row 
rubi_cut<-rubi[3:26]
  rubi_zero<-rubi[apply(rubi_cut==0, 1, sum)<=0,]

### 2. frozens
x<-rubi_zero[3:26]
y<-x[-1]
  diff <- y-x[1:length(x)-1]
  rubi_net<-rubi_zero[apply(diff==0,1,sum)<=2,]               ## more than six 0 per row, remove the row

names(rubi_net)<- c("idmeter","date","00:00","01:00","02:00","03:00","04:00","05:00","06:00","07:00","08:00","09:00","10:00","11:00","12:00","13:00","14:00","15:00","16:00","17:00","18:00","19:00","20:00","21:00","22:00","23:00")


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

###3. Column means
cast_99<-as.data.frame(lapply(split(division_id, division_id$idmeter),colMeans))
    cast100<-as.data.frame(t(cast_99))
        cast100$idmeter<-NULL


hour_percent<-cast100



##FEATURE 1,2,3: Median mean, min, max

c_median<-as.data.frame(apply(hour_percent, 1, median))
names(c_median)<-"c_median"

c_max<-as.data.frame(apply(hour_percent, 1, max))
names(c_max)<-"c_max"

c_min<-as.data.frame(apply(hour_percent, 1, min))
names(c_min)<-"c_min"


##FEATURE 4,5,6,7: morning(6-11am), noon(12-17h), evening(18-23), night(0-5am)
hour_percent2<-hour_percent
names(hour_percent2)<-c(0:23)

c_night<-hour_percent2[,1:6]
c_night<-as.data.frame(rowMeans(c_night))
names(c_night)<-"c_night"

c_morning<-hour_percent2[,7:12]
c_morning<-as.data.frame(rowMeans(c_morning))
names(c_morning)<-"c_morning"

c_noon<-hour_percent2[,13:18]
c_noon<-as.data.frame(rowMeans(c_noon))
names(c_noon)<-"c_noon"

c_evening<-hour_percent2[,19:24]
c_evening<-as.data.frame(rowMeans(c_evening))
names(c_evening)<-"c_evening"



##############RATIOS FIGURES ########

r_mean_max<-c_median/c_max
names(r_mean_max)<-"r_mean_max"

r_min_mean<-c_min/c_median
names(r_min_mean)<-"r_min_mean"

r_min_max<-c_min/c_max
names(r_min_max)<-"r_min_max"

r_night_evening<-c_night/c_evening
names(r_night_evening)<-"r_night_evening"

r_morning_evening<-c_morning/c_evening
names(r_morning_noon)<-"r_morning_noon"

r_night_noon<- c_night/c_morning
names(r_night_noon)<-"r_night_noon"

##variance and Standard deviation
c_var<-as.data.frame(apply(hour_percent, 1, var))
names(c_var)<-"c_var"
c_sd<-as.data.frame(apply(hour_percent, 1, sd))
names(c_sd)<-"c_sd"


c_figures<-cbind(c_median,c_max,c_min, c_morning, c_night, c_evening,c_noon)
r_figures<-cbind(r_mean_max,r_min_mean,r_min_max,r_night_evening,r_morning_evening,r_night_noon)
s_figures<-cbind(c_var,c_sd)


all_figures<-cbind(c_figures,r_figures,s_figures)

total<-cbind(hour_percent,all_figures)

subi<-cbind(hour_percent,c_figures)  ## only min, max, periods



#### Hierarchial clustering matrix data

##Distances: distance is choosen in dist() can be euclidean, manhattan etc...
p<-dist(hour_percent,method="euclidean")

#p<-dist(subi,method="euclidean")

#p<-dist(all_figures,method="euclidean")


##Linkages: linkage is chosen in hclust() can be ward.D, ward.D2, single, complete, average, median,...
p1<-hclust(p, method="ward.D2")                   #also equal to complete, single, ward.D, ward.D2


## Dendrogram
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



## TESTING clusters by plotting       
library(reshape2)
hour<-c(0:23)

#cluster1 
c_1<-as.data.frame(t(cluster1[,c(2:25)]))
c_1<-cbind(hour,c_1)
c_11<-melt(c_1, id.vars="hour")
cp1<-ggplot(c_11, aes(hour,value)) + geom_line(aes(colour = variable))+ggtitle("cluster1")+ylim(0,0.13)#+geom_smooth()

#cluster2 
c_2<-as.data.frame(t(cluster2[,c(2:25)]))
c_2<-cbind(hour,c_2)
c_21<-melt(c_2, id.vars="hour")
cp2<-ggplot(c_21, aes(hour,value)) + geom_line(aes(colour = variable))+ggtitle("cluster2")+ylim(0,0.13)

#cluster3 
c_3<-as.data.frame(t(cluster3[,c(2:25)]))
c_3<-cbind(hour,c_3)
c_31<-melt(c_3, id.vars="hour")
cp3<-ggplot(c_31, aes(hour,value)) + geom_line(aes(colour = variable))+ggtitle("cluster3")+ylim(0,0.13)

#cluster4 
c_4<-as.data.frame(t(cluster4[,c(2:25)]))
c_4<-cbind(hour,c_4)
c_41<-melt(c_4, id.vars="hour")
cp4<-ggplot(c_41, aes(hour,value)) + geom_line(aes(colour = variable))+ggtitle("cluster4") +ylim(0,0.13)

#cluster5
c_5<-as.data.frame(t(cluster5[,c(2:25)]))
c_5<-cbind(hour,c_5)
c_51<-melt(c_5, id.vars="hour")
cp5<-ggplot(c_51, aes(hour,value)) + geom_line(aes(colour = variable))+ggtitle("cluster5")+ylim(0,0.13)

source("multiplot_function.R")
multiplot(cp1, cp2, cp3, cp4,cp5, cols=2)



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

##all clusters

ggplot()+
  geom_line(data=cluster1_mean, aes(hour,mean_clus1),colour= "red", size = 1)+
  geom_line(data=cluster2_mean, aes(hour,mean_clus2),colour= "blue", size = 1)+
  geom_line(data=cluster3_mean, aes(hour,mean_clus3),colour= "orange", size = 1)+
  geom_line(data=cluster4_mean, aes(hour,mean_clus4),colour= "green", size = 1)+
  geom_line(data=cluster5_mean, aes(hour,mean_clus5),colour= "black", size = 1)



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

by_clus_mean<-rbind(cluster1_mean,cluster2_mean,cluster3_mean,cluster4_mean,cluster5_mean)
##2. plot the 5 different cluster in 5 wrap facets
library(ggplot2)
ggplot(by_clus_mean,aes(hour,mean))+geom_line(aes(colour=clus_num))+facet_wrap(~clus_num)#+scale_colour_gradientn(colours=n(6))


