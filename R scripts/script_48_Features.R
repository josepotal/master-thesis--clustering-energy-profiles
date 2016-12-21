complete_features <- read.csv("~/SELECT ERASMUS MUNDUS/ENERBYTE- Thesis&internship/THESIS - Enerbyte/Chapter 2. Case study Data analysis/R for Tesi/test_01_tesi/complete_features.csv", sep=";", stringsAsFactors=FALSE)
str(complete_features)

#all
library(ggplot2)

a<-qplot(complete_features$Tipology,data=complete_features, geom="histogram", xlab="Tipology")
b<-qplot(complete_features$Contract,data=complete_features, geom="histogram",xlab="Contract")
c<-qplot(complete_features$Surface,data=complete_features, geom="histogram",binwidth=25,xlab="Area")  
d<-qplot(complete_features$Year,data=complete_features, geom="histogram",binwidth=10, xlab="Year") 
e<-qplot(complete_features$Power_con, data=complete_features, geom="histogram", binwidth=2,xlab="Contracted Power")
f<-qplot(complete_features$Older_65, data=complete_features, geom="histogram", binwidth=1,xlab="Pensionists")
g<-qplot(complete_features$Adults_13_65, data=complete_features, geom="histogram", binwidth=1, xlab="Adults")
h<-qplot(complete_features$Less_12y, data=complete_features, geom="histogram", binwidth=1,xlab="Children")
i<-qplot(complete_features$AA_Electric, data=complete_features, geom="histogram",xlab="Air Conditioning")
j<-qplot(complete_features$DHW_Electric, data=complete_features, geom="histogram",xlab="Electric Domestic Hot Water")
k<-qplot(complete_features$HEAT_Electric, data=complete_features, geom="histogram", xlab="Electric Heating")
l<-qplot(complete_features$Kitchen_Electric, data=complete_features, geom="histogram",xlab="Electric Kitchen")
m<-qplot(complete_features$Drier, data=complete_features, geom="histogram", xlab="Drier")
n<-qplot(complete_features$Dishwash, data=complete_features, geom="histogram", xlab="Dishwasher")

source("multiplot_function.R")
multiplot(a,b,c,d,e,f,g,h,i,j,k,l,m,n, cols=3)


library(sqldf)
cluster1<-sqldf("select *  from complete_features where Cluster = '1'")
cluster2<-sqldf("select *  from complete_features where Cluster = '2'")
cluster3<-sqldf("select *  from complete_features where Cluster = '3'")
cluster4<-sqldf("select *  from complete_features where Cluster = '4'")
cluster5<-sqldf("select *  from complete_features where Cluster = '5'")
cluster6<-sqldf("select *  from complete_features where Cluster = '6'")
cluster7<-sqldf("select *  from complete_features where Cluster = '7'")



### cluster1
library(ggplot2)

a1<-qplot(cluster1$Tipology,data=cluster1, geom="histogram", xlab="Tipology")
b1<-qplot(cluster1$Contract,data=cluster1, geom="histogram",xlab="Contract")
c1<-qplot(cluster1$Surface,data=cluster1, geom="histogram",binwidth=25,xlab="Area")  
d1<-qplot(cluster1$Year,data=cluster1, geom="histogram",binwidth=10, xlab="Year") 
e1<-qplot(cluster1$Power_con, data=cluster1, geom="histogram", binwidth=2,xlab="Contracted Power")
f1<-qplot(cluster1$Older_65, data=cluster1, geom="histogram", binwidth=1,xlab="Pensionists")
g1<-qplot(cluster1$Adults_13_65, data=cluster1, geom="histogram", binwidth=1, xlab="Adults")
h1<-qplot(cluster1$Less_12y, data=cluster1, geom="histogram", binwidth=1,xlab="Children")
i1<-qplot(cluster1$AA_Electric, data=cluster1, geom="histogram",xlab="Air Conditioning")
j1<-qplot(cluster1$DHW_Electric, data=cluster1, geom="histogram",xlab="Electric Domestic Hot Water")
k1<-qplot(cluster1$HEAT_Electric, data=cluster1, geom="histogram", xlab="Electric Heating")
l1<-qplot(cluster1$Kitchen_Electric, data=cluster1, geom="histogram",xlab="Electric Kitchen")
m1<-qplot(cluster1$Drier, data=cluster1, geom="histogram", xlab="Drier")
n1<-qplot(cluster1$Dishwash, data=cluster1, geom="histogram", xlab="Dishwasher")

source("multiplot_function.R")
multiplot(a1,b1,c1,d1,e1,f1,g1,h1,i1,j1,k1,l1,m1,n1, cols=3)


### cluster2
library(ggplot2)

a2<-qplot(cluster2$Tipology,data=cluster2, geom="histogram", xlab="Tipology")
b2<-qplot(cluster2$Contract,data=cluster2, geom="histogram",xlab="Contract")
c2<-qplot(cluster2$Surface,data=cluster2, geom="histogram",binwidth=25,xlab="Area")  
d2<-qplot(cluster2$Year,data=cluster2, geom="histogram",binwidth=10, xlab="Year") 
e2<-qplot(cluster2$Power_con, data=cluster2, geom="histogram", binwidth=2,xlab="Contracted Power")
f2<-qplot(cluster2$Older_65, data=cluster2, geom="histogram", binwidth=1,xlab="Pensionists")
g2<-qplot(cluster2$Adults_13_65, data=cluster2, geom="histogram", binwidth=1, xlab="Adults")
h2<-qplot(cluster2$Less_12y, data=cluster2, geom="histogram", binwidth=1,xlab="Children")
i2<-qplot(cluster2$AA_Electric, data=cluster2, geom="histogram",xlab="Air Conditioning")
j2<-qplot(cluster2$DHW_Electric, data=cluster2, geom="histogram",xlab="Electric Domestic Hot Water")
k2<-qplot(cluster2$HEAT_Electric, data=cluster2, geom="histogram", xlab="Electric Heating")
l2<-qplot(cluster2$Kitchen_Electric, data=cluster2, geom="histogram",xlab="Electric Kitchen")
m2<-qplot(cluster2$Drier, data=cluster2, geom="histogram", xlab="Drier")
n2<-qplot(cluster2$Dishwash, data=cluster2, geom="histogram", xlab="Dishwasher")

source("multiplot_function.R")
multiplot(a2,b2,c2,d2,e2,f2,g2,h2,i2,j2,k2,l2,m2,n2, cols=3)

### cluster3
library(ggplot2)

a3<-qplot(cluster3$Tipology,data=cluster3, geom="histogram", xlab="Tipology")
b3<-qplot(cluster3$Contract,data=cluster3, geom="histogram",xlab="Contract")
c3<-qplot(cluster3$Surface,data=cluster3, geom="histogram",binwidth=25,xlab="Area")  
d3<-qplot(cluster3$Year,data=cluster3, geom="histogram",binwidth=10, xlab="Year") 
e3<-qplot(cluster3$Power_con, data=cluster3, geom="histogram", binwidth=2,xlab="Contracted Power")
f3<-qplot(cluster3$Older_65, data=cluster3, geom="histogram", binwidth=1,xlab="Pensionists")
g3<-qplot(cluster3$Adults_13_65, data=cluster3, geom="histogram", binwidth=1, xlab="Adults")
h3<-qplot(cluster3$Less_12y, data=cluster3, geom="histogram", binwidth=1,xlab="Children")
i3<-qplot(cluster3$AA_Electric, data=cluster3, geom="histogram",xlab="Air Conditioning")
j3<-qplot(cluster3$DHW_Electric, data=cluster3, geom="histogram",xlab="Electric Domestic Hot Water")
k3<-qplot(cluster3$HEAT_Electric, data=cluster3, geom="histogram", xlab="Electric Heating")
l3<-qplot(cluster3$Kitchen_Electric, data=cluster3, geom="histogram",xlab="Electric Kitchen")
m3<-qplot(cluster3$Drier, data=cluster3, geom="histogram", xlab="Drier")
n3<-qplot(cluster3$Dishwash, data=cluster3, geom="histogram", xlab="Dishwasher")

source("multiplot_function.R")
multiplot(a3,b3,c3,d3,e3,f3,g3,h3,i3,j3,k3,l3,m3,n3, cols=3)



### cluster4
library(ggplot2)

a4<-qplot(cluster4$Tipology,data=cluster4, geom="histogram", xlab="Tipology",ylim=c(0,25))
b4<-qplot(cluster4$Contract,data=cluster4, geom="histogram",xlab="Contract")
c4<-qplot(cluster4$Surface,data=cluster4, geom="histogram",binwidth=25,xlab="Area")  
d4<-qplot(cluster4$Year,data=cluster4, geom="histogram",binwidth=10, xlab="Year") 
e4<-qplot(cluster4$Power_con, data=cluster4, geom="histogram", binwidth=2,xlab="Contracted Power")
f4<-qplot(cluster4$Older_65, data=cluster4, geom="histogram", binwidth=1,xlab="Pensionists")
g4<-qplot(cluster4$Adults_13_65, data=cluster4, geom="histogram", binwidth=1, xlab="Adults")
h4<-qplot(cluster4$Less_12y, data=cluster4, geom="histogram", binwidth=1,xlab="Children")
i4<-qplot(cluster4$AA_Electric, data=cluster4, geom="histogram",xlab="Air Conditioning")
j4<-qplot(cluster4$DHW_Electric, data=cluster4, geom="histogram",xlab="Electric Domestic Hot Water")
k4<-qplot(cluster4$HEAT_Electric, data=cluster4, geom="histogram", xlab="Electric Heating")
l4<-qplot(cluster4$Kitchen_Electric, data=cluster4, geom="histogram",xlab="Electric Kitchen")
m4<-qplot(cluster4$Drier, data=cluster4, geom="histogram", xlab="Drier")
n4<-qplot(cluster4$Dishwash, data=cluster4, geom="histogram", xlab="Dishwasher")

source("multiplot_function.R")
multiplot(a4,b4,c4,d4,e4,f4,g4,h4,i4,j4,k4,l4,m4,n4, cols=3)


### cluster5
library(ggplot2)

a5<-qplot(cluster5$Tipology,data=cluster5, geom="histogram", xlab="Tipology")
b5<-qplot(cluster5$Contract,data=cluster5, geom="histogram",xlab="Contract")
c5<-qplot(cluster5$Surface,data=cluster5, geom="histogram",binwidth=25,xlab="Area")  
d5<-qplot(cluster5$Year,data=cluster5, geom="histogram",binwidth=10, xlab="Year") 
e5<-qplot(cluster5$Power_con, data=cluster5, geom="histogram", binwidth=2,xlab="Contracted Power",xlim=c(0,10))
f5<-qplot(cluster5$Older_65, data=cluster5, geom="histogram", binwidth=1,xlab="Pensionists")
g5<-qplot(cluster5$Adults_13_65, data=cluster5, geom="histogram", binwidth=1, xlab="Adults")
h5<-qplot(cluster5$Less_12y, data=cluster5, geom="histogram", binwidth=1,xlab="Children")
i5<-qplot(cluster5$AA_Electric, data=cluster5, geom="histogram",xlab="Air Conditioning")
j5<-qplot(cluster5$DHW_Electric, data=cluster5, geom="histogram",xlab="Electric Domestic Hot Water")
k5<-qplot(cluster5$HEAT_Electric, data=cluster5, geom="histogram", xlab="Electric Heating")
l5<-qplot(cluster5$Kitchen_Electric, data=cluster5, geom="histogram",xlab="Electric Kitchen")
m5<-qplot(cluster5$Drier, data=cluster5, geom="histogram", xlab="Drier")
n5<-qplot(cluster5$Dishwash, data=cluster5, geom="histogram", xlab="Dishwasher")

source("multiplot_function.R")
multiplot(a5,b5,c5,d5,e5,f5,g5,h5,i5,j5,k5,l5,m5,n5, cols=3)

### cluster6
library(ggplot2)

a6<-qplot(cluster6$Tipology,data=cluster6, geom="histogram", xlab="Tipology")
b6<-qplot(cluster6$Contract,data=cluster6, geom="histogram",xlab="Contract")
c6<-qplot(cluster6$Surface,data=cluster6, geom="histogram",binwidth=25,xlab="Area")  
d6<-qplot(cluster6$Year,data=cluster6, geom="histogram",binwidth=10, xlab="Year") 
e6<-qplot(cluster6$Power_con, data=cluster6, geom="histogram", binwidth=2,xlab="Contracted Power",xlim=c(0,10))
f6<-qplot(cluster6$Older_65, data=cluster6, geom="histogram", binwidth=1,xlab="Pensionists")
g6<-qplot(cluster6$Adults_13_65, data=cluster6, geom="histogram", binwidth=1, xlab="Adults")
h6<-qplot(cluster6$Less_12y, data=cluster6, geom="histogram", binwidth=1,xlab="Children")
i6<-qplot(cluster6$AA_Electric, data=cluster6, geom="histogram",xlab="Air Conditioning")
j6<-qplot(cluster6$DHW_Electric, data=cluster6, geom="histogram",xlab="Electric Domestic Hot Water")
k6<-qplot(cluster6$HEAT_Electric, data=cluster6, geom="histogram", xlab="Electric Heating")
l6<-qplot(cluster6$Kitchen_Electric, data=cluster6, geom="histogram",xlab="Electric Kitchen")
m6<-qplot(cluster6$Drier, data=cluster6, geom="histogram", xlab="Drier")
n6<-qplot(cluster6$Dishwash, data=cluster6, geom="histogram", xlab="Dishwasher")

source("multiplot_function.R")
multiplot(a6,b6,c6,d6,e6,f6,g6,h6,i6,j6,k6,l6,m6,n6, cols=3)


### cluster7
library(ggplot2)

a7<-qplot(cluster7$Tipology,data=cluster7, geom="histogram", xlab="Tipology")
b7<-qplot(cluster7$Contract,data=cluster7, geom="histogram",xlab="Contract")
c7<-qplot(cluster7$Surface,data=cluster7, geom="histogram",binwidth=25,xlab="Area")  
d7<-qplot(cluster7$Year,data=cluster7, geom="histogram",binwidth=10, xlab="Year") 
e7<-qplot(cluster7$Power_con, data=cluster7, geom="histogram", binwidth=2,xlab="Contracted Power",xlim=c(0,10))
f7<-qplot(cluster7$Older_65, data=cluster7, geom="histogram", binwidth=1,xlab="Pensionists")
g7<-qplot(cluster7$Adults_13_65, data=cluster7, geom="histogram", binwidth=1, xlab="Adults")
h7<-qplot(cluster7$Less_12y, data=cluster7, geom="histogram", binwidth=1,xlab="Children")
i7<-qplot(cluster7$AA_Electric, data=cluster7, geom="histogram",xlab="Air Conditioning")
j7<-qplot(cluster7$DHW_Electric, data=cluster7, geom="histogram",xlab="Electric Domestic Hot Water")
k7<-qplot(cluster7$HEAT_Electric, data=cluster7, geom="histogram", xlab="Electric Heating")
l7<-qplot(cluster7$Kitchen_Electric, data=cluster7, geom="histogram",xlab="Electric Kitchen")
m7<-qplot(cluster7$Drier, data=cluster7, geom="histogram", xlab="Drier")
n7<-qplot(cluster7$Dishwash, data=cluster7, geom="histogram", xlab="Dishwasher")

source("multiplot_function.R")
multiplot(a7,b7,c7,d7,e7,f7,g7,h7,i7,j7,k7,l7,m7,n7, cols=3)
