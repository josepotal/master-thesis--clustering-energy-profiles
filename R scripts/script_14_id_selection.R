source("read_dt.R")
all<-read.csv("all_clean.csv",header=T)
st<-split(all,all$idmeter)
temp<-c(as.numeric(names(st)))
temp1<-sort(temp)



test1<-all[all$idmeter==90713,]
s_63<-as.data.frame(veure(x))

rbind(s_61,s_62,s_63)

mill<-data.frame()
for(i in seq_along(temp1)){
  test1<-read_dt(i)
  s_1000<-as.data.frame(veure(x))
  mill<-rbind(mill,s_1000)
}

veure<-function (x){
  ##histogram
  #hist(test1$power)
  ## know how many 0
  num_zeros <- length(which(test1$power == 0))
  
  ## percentage 0 respect to total
  #total measures
  tot<-length(test1$power)
  if (tot-num_zeros>8000){
    s<-print("YES")
  } else {
    s<-print("NO")
  }
  #percentage 0 to total  
  percen_zeros<- num_zeros/tot
  resum<-c(num_zeros,tot,s,percen_zeros)
  resum<-as.data.frame(resum)
  resum<-t(resum)
  View(resum)
  }
## know how many consecutive numbers --> frozens
test2<-data.frame(count=sort(table(test1$power), decreasing=TRUE))
View(head(test2))

test1[which(test1$power == 212),]





mill<-data.frame()
for (i in seq_along(temp1)){
  test1<-as.data.frame(st[i])
  num_zeros <- length(which(st$power == 0))
  tot<-length(st$power)
  percen_zeros<- num_zeros/tot
  resum<-c(num_zeros,tot,percen_zeros)
  resum<-as.data.frame(resum)
  resum<-t(resum)
  mill<-rbind(mill,resum[i])
}

mill<-lapply(temp1,veure1)

veure1<-function (x){
  ## know how many 0
  num_zeros <- length(which(st$power == 0))
  
  ## percentage 0 respect to total
  #total measures
  tot<-length(st$power)
  if (tot-num_zeros>8000){
    s<-print("YES")
  } else {
    s<-print("NO")
  }
  #percentage 0 to total  
  percen_zeros<- num_zeros/tot
  resum<-c(num_zeros,tot,s,percen_zeros)
  resum<-as.data.frame(resum)
  resum<-t(resum)
  ## know how many consecutive numbers --> frozens
  test2<-data.frame(count=sort(table(st$power), decreasing=TRUE))
 }




#write.csv(test1,file="id_103433.csv")
veure(x)






      


