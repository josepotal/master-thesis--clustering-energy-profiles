

####  Import database 
    meter_rubi_01 <- read.csv("~/SELECT ERASMUS MUNDUS/ENERBYTE- Thesis&internship/THESIS - Enerbyte/Part 3. Data analysis/R for Tesi/test_01_tesi/meter_rubi_wd_we.csv", sep=";", stringsAsFactors=FALSE)
        rubi<-meter_rubi_01[(meter_rubi_01$source == "CURRENTCOST"),]
          rubi$source <-NULL
        names(rubi)<- c("idmeter","date","00:00","01:00","02:00","03:00","04:00","05:00","06:00","07:00","08:00","09:00","10:00","11:00","12:00","13:00","14:00","15:00","16:00","17:00","18:00","19:00","20:00","21:00","22:00","23:00")
      rubi$date<-as.POSIXct((rubi$date), format="%d/%m/%Y")
  rubi1<-rubi[order(as.numeric(rubi$idmeter),rubi$date),]


####Seprate Weekday and Weekends
library(xts)
rubi2<-as.xts(rubi1,rubi1$date)

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

## Weekends
  weekends<-rubi2[!.indexwday(rubi2) %in% 1:5]
      w_ends<-data.frame(dates=index(weekends), coredata(weekends))
        w_ends$dates<-NULL
            names(w_ends)<- c("idmeter","date",c("we0","we1","we2","we3","we4","we5","we6","we7","we8","we9","we10","we11","we12","we13","we14","we15","we16","we17","we18","we19","we20","we21","we22","we23"))
              w_ends[,c(3:26)] <- lapply(w_ends[,c(3:26)], as.character)
            w_ends[,c(3:26)] <- lapply(w_ends[,c(3:26)], as.numeric)
          w_ends$idmeter<-as.numeric(as.character(w_ends$idmeter))
        w_ends$date<-as.POSIXct(w_ends$date)
      w_ends<-w_ends[order(w_ends$idmeter,w_ends$date),]
    str(w_ends)



####  PERCENTAGES (OPOWER) OPTION 2: CURVES ARCHETYPES
    ## RowSUms, Division, ColMeans --> MORE ACCURATE!
  
  ### WEEKDAYSSS!!
    #1. Get the sum of each row 
    w_days_net<-w_days[3:26]
        row_sub <- apply(w_days_net, 1, function(row) all(row !=0 )) #Go through each row and determine if a value is zero
           w_days_net<- w_days_net[row_sub,]   #Subset as usual
              row_sum<-as.matrix(rowSums(w_days_net))
            names(row_sum)<-"sum"
        w_days<-w_days[row_sub,]
    
    #2. Division to get the percentages per hour
    division<-as.data.frame(w_days_net/row_sum)
        division_id<-cbind(w_days$idmeter,division)
              names(division_id)<-c("idmeter",c("wd0","wd1","wd2","wd3","wd4","wd5","wd6","wd7","wd8","wd9","wd10","wd11","wd12","wd13","wd14","wd15","wd16","wd17","wd18","wd19","wd20","wd21","wd22","wd23"))
          division_id<-as.data.frame(division_id)
      test<-as.matrix(rowSums(division_id[2:25]))

    #3. Column means
    cast_99_wd<-as.data.frame(lapply(split(division_id, division_id$idmeter),colMeans))
        cast100_wd<-as.data.frame(t(cast_99_wd))
            cast100_wd$idmeter<-NULL
        cast100_wd<-cast100_wd[-c(143), ]

    perc_wd<-cast100_wd

##WEEKENDSS!!
    #1. Get the sum of each row 
    w_ends_net<-w_ends[3:26]
        row_sub1 <- apply(w_ends_net, 1, function(row) all(row !=0 )) #Go through each row and determine if a value is zero
            w_ends_net<- w_ends_net[row_sub1,]   #Subset as usual
              row_sum_we<-as.matrix(rowSums(w_ends_net))
          names(row_sum_we)<-"sum"
        w_ends<-w_ends[row_sub1,]
  
    #2. Division to get the percentages per hour
    division_we<-as.data.frame(w_ends_net/row_sum_we)
        division_id_we<-cbind(w_ends$idmeter,division_we)
            names(division_id_we)<-c("idmeter",c("we0","we1","we2","we3","we4","we5","we6","we7","we8","we9","we10","we11","we12","we13","we14","we15","we16","we17","we18","we19","we20","we21","we22","we23"))
          division_id_we<-as.data.frame(division_id_we)
              test_we<-as.matrix(rowSums(division_id_we[2:25]))

    #3. Column means
    cast_99_we<-as.data.frame(lapply(split(division_id_we, division_id_we$idmeter),colMeans))
          cast100_we<-as.data.frame(t(cast_99_we))
                  cast100_we$idmeter<-NULL
                     

perc_we<-cast100_we


## Data casted weekdays and weekends
perc_wd_we<-cbind(perc_wd,perc_we)   ## matrix real power data casted



    #### Hierarchial clustering matrix data
    
    ##Distances: distance is choosen in dist() can be euclidean, manhattan etc...
          p<-dist(perc_wd_we,method="euclidean")
    
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



############PLOTTING
## if uses "rel_wdays_wends" print from 0 to 1; if uses "wdays_wends" print real power
      
      p2<-as.data.frame(cbind(clus$clus_num, perc_wd_we)) 
          names(p2)<-c("clus_num",c("wd0","wd1","wd2","wd3","wd4","wd5","wd6","wd7","wd8","wd9","wd10","wd11","wd12","wd13","wd14","wd15","wd16","wd17","wd18","wd19","wd20","wd21","wd22","wd23","we0","we1","we2","we3","we4","we5","we6","we7","we8","we9","we10","we11","we12","we13","we14","we15","we16","we17","we18","we19","we20","we21","we22","we23"))
          


cluster1<-p2[p2$clus_num==1,]
cluster2<-p2[p2$clus_num==2,]
cluster3<-p2[p2$clus_num==3,]
cluster4<-p2[p2$clus_num==4,]
cluster5<-p2[p2$clus_num==5,]




#### TESTING clusters by plotting       
    library(reshape2)
    hour<-c(0:23)

##1. WEEKDAYS PLOT clusters 

    #cluster1 
    c_1wd<-as.data.frame(t(cluster1[,c(2:25)]))
        c_1wd<-cbind(hour,c_1wd)
          c_11wd<-melt(c_1wd, id.vars="hour")
    cp1wd<-ggplot(c_11wd, aes(hour,value)) + geom_line(aes(colour = variable))+geom_smooth()+ggtitle("cluster1")

    #cluster2
    c_2wd<-as.data.frame(t(cluster2[,c(2:25)]))
        c_2wd<-cbind(hour,c_2wd)
          c_21wd<-melt(c_2wd, id.vars="hour")
    cp2wd<-ggplot(c_21wd, aes(hour,value)) + geom_line(aes(colour = variable))+geom_smooth()+ggtitle("cluster2")

    #cluster3 
    c_3wd<-as.data.frame(t(cluster3[,c(2:25)]))
        c_3wd<-cbind(hour,c_3wd)
          c_31wd<-melt(c_3wd, id.vars="hour")
    cp3wd<-ggplot(c_31wd, aes(hour,value)) + geom_line(aes(colour = variable))+geom_smooth()+ggtitle("cluster3")

    #cluster4 
    c_4wd<-as.data.frame(t(cluster4[,c(2:25)]))
        c_4wd<-cbind(hour,c_4wd)
          c_41wd<-melt(c_4wd, id.vars="hour")
    cp4wd<-ggplot(c_41wd, aes(hour,value)) + geom_line(aes(colour = variable))+geom_smooth()+ggtitle("cluster4")# +ylim(0,1)#+scale_y_log10()     

    #cluster5 
    c_5wd<-as.data.frame(t(cluster5[,c(2:25)]))
      c_5wd<-cbind(hour,c_5wd)
          c_51wd<-melt(c_5wd, id.vars="hour")
    cp5wd<-ggplot(c_51wd, aes(hour,value)) + geom_line(aes(colour = variable))+geom_smooth()+ggtitle("cluster5")

    source("multiplot_function.R")
    multiplot(cp1wd, cp2wd, cp3wd, cp4wd,cp5wd, cols=2)



####2. WEEKENDS PLOT clusters 
    #cluster1 
    c_1we<-as.data.frame(t(cluster1[,c(26:49)]))
      c_1we<-cbind(hour,c_1we)
          c_11we<-melt(c_1we, id.vars="hour")
    cp1we<-ggplot(c_11we, aes(hour,value)) + geom_line(aes(colour = variable))+geom_smooth()+ggtitle("cluster1")

    #cluster2 
    c_2we<-as.data.frame(t(cluster2[,c(26:49)]))
        c_2we<-cbind(hour,c_2we)
          c_21we<-melt(c_2we, id.vars="hour")
    cp2we<-ggplot(c_21we, aes(hour,value)) + geom_line(aes(colour = variable))+geom_smooth()+ggtitle("cluster2")

    #cluster3 
    c_3we<-as.data.frame(t(cluster3[,c(26:49)]))
        c_3we<-cbind(hour,c_3we)
          c_31we<-melt(c_3we, id.vars="hour")
    cp3we<-ggplot(c_31we, aes(hour,value)) + geom_line(aes(colour = variable))+geom_smooth()+ggtitle("cluster3")

    #cluster4
    c_4we<-as.data.frame(t(cluster4[,c(26:49)]))
        c_4we<-cbind(hour,c_4we)
          c_41we<-melt(c_4we, id.vars="hour")
    cp4we<-ggplot(c_41we, aes(hour,value)) + geom_line(aes(colour = variable))+geom_smooth()+ggtitle("cluster4") #+ylim(0,1)#+scale_y_log10()     

    #cluster5 
    c_5we<-as.data.frame(t(cluster5[,c(26:49)]))
        c_5we<-cbind(hour,c_5we)
          c_51we<-melt(c_5we, id.vars="hour")
    cp5we<-ggplot(c_51we, aes(hour,value)) + geom_line(aes(colour = variable))+geom_smooth()+ggtitle("cluster5")

source("multiplot_function.R")
multiplot(cp1we, cp2we, cp3we, cp4we, cp5we, cols=2)

multiplot(cp1wd, cp2wd, cp3wd, cp4wd, cp5wd,cp1we, cp2we, cp3we, cp4we, cp5we, cols=2)



#CLUSTERS MEAN WEEKDAYS
cluster1_mean<-as.data.frame(colMeans(cluster1[2:25]))
names(cluster1_mean)<-"mean_clus1"
cluster1_mean<-cbind(hour,cluster1_mean)
ggplot()+
  geom_line(data=c_11, aes(hour,value, colour=variable))+
  geom_line(data=cluster1_mean, aes(hour,mean_clus1),colour= "red", size = 1)


cluster2_mean<-as.data.frame(colMeans(cluster2[2:25]))
names(cluster2_mean)<-"mean_clus2"
cluster2_mean<-cbind(hour,cluster2_mean)
ggplot()+
  geom_line(data=c_21, aes(hour,value, colour=variable))+
  geom_line(data=cluster2_mean, aes(hour,mean_clus2),colour= "blue", size = 1)

cluster3_mean<-as.data.frame(colMeans(cluster3[2:25]))
names(cluster3_mean)<-"mean_clus3"
cluster3_mean<-cbind(hour,cluster3_mean)
ggplot()+
  geom_line(data=c_31, aes(hour,value, colour=variable))+
  geom_line(data=cluster3_mean, aes(hour,mean_clus3),colour= "orange", size = 1)


cluster4_mean<-as.data.frame(colMeans(cluster4[2:25]))
names(cluster4_mean)<-"mean_clus4"
cluster4_mean<-cbind(hour,cluster4_mean)
ggplot()+
  geom_line(data=c_41, aes(hour,value, colour=variable))+
  geom_line(data=cluster4_mean, aes(hour,mean_clus4),colour= "green", size = 1)


cluster5_mean<-as.data.frame(colMeans(cluster5[2:25]))
names(cluster5_mean)<-"mean_clus5"
cluster5_mean<-cbind(hour,cluster5_mean)
ggplot()+
  geom_line(data=c_51, aes(hour,value, colour=variable))+
  geom_line(data=cluster5_mean, aes(hour,mean_clus5),colour= "black", size = 1)

all_wd<-
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
multiplot(all_wd,all_we, cols=2)
