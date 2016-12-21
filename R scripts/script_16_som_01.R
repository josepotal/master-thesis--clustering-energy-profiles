
##1. Import data from excel
  setwd("~/SELECT ERASMUS MUNDUS/ENERBYTE- Thesis&internship/THESIS - Enerbyte/Part 3. Data analysis/R for Tesi/test_01_tesi")
    rubi <- read.csv("~/SELECT ERASMUS MUNDUS/ENERBYTE- Thesis&internship/THESIS - Enerbyte/Part 3. Data analysis/R for Tesi/test_01_tesi/meter_rubi_01.csv", sep=";", stringsAsFactors=FALSE)
      rubi<-rubi[(rubi$source == "CURRENTCOST"),]
        rubi$source <-NULL
          rubi$idmeter<-as.numeric(rubi$idmeter)
            rubi$date <-NULL

##2.Caclulate the mean for each hour (0 to 23)
  cast<-as.data.frame(lapply(split(rubi, rubi$idmeter),colMeans))
    cast1<-as.data.frame(t(cast))
      cast1$idmeter<-NULL
        cast1<-round(cast1, digits = 0)
          cast1

##3. Remove the rows that contain at least a zero
  row_sub <- apply(cast1, 1, function(row) all(row !=0 )) #Go through each row and determine if a value is zero
    cast2<- cast1[row_sub,]   #Subset as usual
        cast2<-cast2[-c(42,145), ]
          


## 4. call libraries
library(som)
library (kohonen)

##5.1 som cluster no scaling
  cast3<-as.matrix(cast2)
      hol<-som(cast3, grid = somgrid(12, 12, "hexagonal"))
        plot(hol)

  ## 5.2. som cluster scaling data
  cast4<-as.matrix (scale(cast2))
      holy<-som(cast4, grid = somgrid(12, 12, "hexagonal"))
        plot(holy)

  ##5.3. som clustering dividing row per each maximum
  cast5<-as.matrix(apply(cast2, 1, max))
      cast6<-as.matrix(cast2/cast5)           ## not scaled matrix
      cast61<-as.matrix(scale(cast2/cast5))   ## scaled matrix
      
        som_model<-som(cast61, 
                  grid=somgrid(12,12,"hexagonal"), 
                  rlen=1000, 
                  alpha=c(0.05,0.001),
                  #toroidal=TRUE,
                  keep.data = TRUE,
                  n.hood="circular")
          plot(som_model)
str(cast61)

  ##6. Plotting som
  summary(som_model) 
  str(som_model)
    plot(som_model)
      plot(som_model, type="codes")    
      plot(som_model, type="changes")
      plot(som_model, type="quality")
      plot(som_model, type="dist.neighbours")
      plot(som_model, type="mapping", labels= row(cast61))
      plot(som_model, type="count")
     
      plot(som_model, type = "property", property = som_model$codes[,4], main=names(som_model$data)[4])

  
  
  plot(som_model, type = "property", property = som_model$codes, main=names(som_model$data))
  
  ## Hierarchical clustering to cluster the codebook vectors
  str(som_model)
  a<-as.matrix(dist(som_model$codes))
  plot(hclust(dist(som_model$codes)))
  som_cluster <- cutree(hclust(dist(som_model$codes)), 5)
  som_cluster
  
  # plot these results:
  plot(som_model, type="mapping", bgcol = c("purple","red","gray", "pink", "blue","green", "yellow","black")[som_cluster], main = "Clusters") 
  add.cluster.boundaries(som_model, som_cluster)


