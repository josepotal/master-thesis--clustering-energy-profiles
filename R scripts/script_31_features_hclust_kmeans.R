
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

###2. Division to get the percentages per hour  
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




#### Hierarchial clustering matrix data

##Distances: distance is choosen in dist() can be euclidean, manhattan etc...
p<-dist(total,method="euclidean")

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




#### K-means clustering

## no distances between points is needed to calculate, as kmeans is based centroid mininimu square 

fit <- kmeans(total, 5,iter.max=100,nstart=100) # 5 cluster solution
fit
clus_num<-fit$cluster
p1<-as.data.frame(clus_num)
p2<-cbind(p1,hour_percent)
names(p2)<-c("clus_num",c(0:23))
