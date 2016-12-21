##each cluster clustering
cluster51<-cluster5[2:25]
p_c4<-dist(cluster51,method="euclidean")

##Linkages: linkage is chosen in hclust() can be ward.D, ward.D2, single, complete, average, median,...
p1_c4<-hclust(p_c4, method="ward.D") 

library(ggplot2)
library(ggdendro)  
ggdendrogram(p1_c4, rotate = F, size = 3, labels=T)     ##  http://cran.r-project.org/web/packages/ggdendro/vignettes/ggdendro.html       


## Cluster by cutree()
clus_c4 <- as.data.frame(cutree((p1_c4), k=7))  ## k indicates number of clusters      
names(clus_c4)<- "clus_num"
clus_c4
table(clus_c4)    # to know how many are in each cluster



p2_c4<-as.data.frame(cbind(clus_c4$clus_num,cluster51)) 
names(p2_c4)<-c("clus_num",c(0:23))

c1_c4<-p2_c4[p2_c4$clus_num==1,]
c2_c4<-p2_c4[p2_c4$clus_num==2,]
c3_c4<-p2_c4[p2_c4$clus_num==3,]
c4_c4<-p2_c4[p2_c4$clus_num==4,]
c5_c4<-p2_c4[p2_c4$clus_num==5,]
c6_c4<-p2_c4[p2_c4$clus_num==6,]
c7_c4<-p2_c4[p2_c4$clus_num==7,]



## TESTING clusters by plotting       
library(reshape2)
hour<-c(0:23)

#cluster1 
c_1_c4<-as.data.frame(t(c1_c4[,c(2:25)]))
c_1_c4<-cbind(hour,c_1_c4)
c_11_c4<-melt(c_1_c4, id.vars="hour")
cp1_c4<-
  ggplot(c_11_c4, aes(hour,value)) + geom_line(aes(colour = variable))+ggtitle("cluster1")+ylim(0,0.15)
theme_set(theme_gray(base_size = 12))
#cluster2 
c_2_c4<-as.data.frame(t(c2_c4[,c(2:25)]))
c_2_c4<-cbind(hour,c_2_c4)
c_21_c4<-melt(c_2_c4, id.vars="hour")
cp2_c4<-
  ggplot(c_21_c4, aes(hour,value)) + geom_line(aes(colour = variable))+ggtitle("cluster2")+ylim(0,0.15)

#cluster3 
c_3_c4<-as.data.frame(t(c3_c4[,c(2:25)]))
c_3_c4<-cbind(hour,c_3_c4)
c_31_c4<-melt(c_3_c4, id.vars="hour")
cp3_c4<-
  ggplot(c_31_c4, aes(hour,value)) + geom_line(aes(colour = variable))+ggtitle("cluster3")+ylim(0,0.15)

#cluster4 
c_4_c4<-as.data.frame(t(c4_c4[,c(2:25)]))
c_4_c4<-cbind(hour,c_4_c4)
c_41_c4<-melt(c_4_c4, id.vars="hour")
cp4_c4<-
  ggplot(c_41_c4, aes(hour,value)) + geom_line(aes(colour = variable))+ggtitle("cluster4") +ylim(0,0.15)

#cluster5
c_5_c4<-as.data.frame(t(c5_c4[,c(2:25)]))
c_5_c4<-cbind(hour,c_5_c4)
c_51_c4<-melt(c_5_c4, id.vars="hour")
cp5_c4<-
  ggplot(c_51_c4, aes(hour,value)) + geom_line(aes(colour = variable))+ggtitle("cluster5")+ylim(0,0.15)

#cluster6
c_6_c4<-as.data.frame(t(c6_c4[,c(2:25)]))
c_6_c4<-cbind(hour,c_6_c4)
c_61_c4<-melt(c_6_c4, id.vars="hour")
cp6_c4<-
  ggplot(c_61_c4, aes(hour,value)) + geom_line(aes(colour = variable))+ggtitle("cluster6")+ylim(0,0.15)

#cluster7
c_7_c4<-as.data.frame(t(c7_c4[,c(2:25)]))
c_7_c4<-cbind(hour,c_7_c4)
c_71_c4<-melt(c_7_c4, id.vars="hour")
cp7_c4<-
  ggplot(c_71_c4, aes(hour,value)) + geom_line(aes(colour = variable))+ggtitle("cluster7")+ylim(0,0.15)

source("multiplot_function.R")
multiplot(cp1_c4, cp2_c4, cp3_c4, cp4_c4,cp5_c4,cp6_c4,cp7_c4, cols=3)
