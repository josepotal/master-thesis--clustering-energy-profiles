    
  source("read_dt_w.R")
  rubi_net<-read_dt_w(112854)

  #Column means
  rubi_low<-rubi_net[3:26]
  rubi_means<-as.data.frame(colMeans(rubi_low))
  rubi_means<-rubi_means/1000
  names(rubi_means)<-"power"
  hour<-as.data.frame(c(0:23))
  names(hour)<-"hour"
  rubi_net_abs<-cbind(hour,rubi_means)  
  
  
  #Percentage calcualtions  
  ## RowSUms, Division, ColMeans --> MORE ACCURATE!
  
  ###1. Get the sum of each row 
  rubi_sum<-rubi_net[3:26]
  row_sum<-as.matrix(rowSums(rubi_sum))
  names(row_sum)<-"sum"
  
  ###2. Division to get the percentages per hour  
  division<-as.data.frame(rubi_sum/row_sum)
  names(division)<-c("00:00","01:00","02:00","03:00","04:00","05:00","06:00","07:00","08:00","09:00","10:00","11:00","12:00","13:00","14:00","15:00","16:00","17:00","18:00","19:00","20:00","21:00","22:00","23:00")  
  division_id<-as.data.frame(division)
  
  ###3. Column means
  cast_99<-as.data.frame(colMeans(division_id))
  names(cast_99)<-"percent"
  hour<-as.data.frame(c(0:23))
  names(hour)<-"hour"
  rubi_net_perc<-cbind(hour,cast_99)  
  ########  Reshaping
  
  
  ##PLOTTING ALL JOINT In the same graph, all weekdays vs weekends
  library(ggplot2)
  
  ggplot(rubi_net_perc,aes(hour, percent,group=1))+ geom_bar(width=0.9,aes(fill=percent),stat = "identity")+
    labs(title = "LOAD PROFILE",x = "Hour",y="Proportion [%]")+scale_fill_gradientn(colours=c("green","orange","red"))+
    scale_x_continuous(breaks=c(seq(0,23,by=1)))+scale_y_continuous(breaks=c(seq(0,0.15,by=0.01)))+
    theme_set(theme_gray(base_size = 26))
  
  ggplot(rubi_net_abs,aes(hour, power,group=1))+ geom_bar(width=0.75,aes(fill=power),stat = "identity")+ 
    labs(title = "User:112854  Load profile [kWh]",x = "Hour",y="Consumption [kWh]")+scale_x_continuous(breaks=c(seq(0,23,by=1)))+
    scale_x_continuous(breaks=c(seq(0,23,by=1)))+scale_y_continuous(breaks=c(seq(0,10,by=0.2)))
  
  ggplot(rubi_net_perc,aes(hour, percent,color=percent))+ geom_point(size=4)+geom_line(size=1)+
    labs(title = "User:112696  Load profile [%]",x = "Hour",y="Proportion [%]")+scale_colour_gradientn(colours=c("green","orange","red"))+
    scale_x_continuous(breaks=c(seq(0,23,by=1)))+scale_y_continuous(breaks=c(seq(0,0.15,by=0.01)))#+expand_limits(y = 0)
  
  ggplot(rubi_net_abs,aes(hour, power))+ geom_bar(aes(fill=power),stat = "identity")+coord_polar()+ 
    labs(title = "User:112723  Load distribution [kWh]",x = "Hour",y="Consumption [kWh]")+scale_fill_gradientn(colours=c("green","orange","red"))+
    scale_x_continuous(breaks=c(seq(0,23,by=1)))+scale_y_continuous(breaks=c(seq(0,10,by=0.5)))
  
  
