#web page -->     http://docs.ggplot2.org/0.9.2.1/scale_gradient.html

###acumulat periode seleccionat
write.csv(holo112917, file = "holo_112917MyData.csv")
write.csv(holo102802, file = "holo_102802MyData.csv")
write.csv(holo112873, file = "holo_112873MyData.csv")
write.csv(holo112789, file = "holo_112789MyData.csv")
write.csv(holo112980, file = "holo_112980MyData.csv")


setwd("~/SELECT ERASMUS MUNDUS/ENERBYTE- Thesis&internship/THESIS - Enerbyte/Chapter 2. Case study Data analysis/R for Tesi/test_01_tesi/idmeters_csv")
id_112695<-read.csv("id_112695.csv",header=T)
id_112718<-read.csv("id_112718.csv",header=T)
id_112788<-read.csv("id_112788.csv",header=T)
###plot various in the same plot--> we can activate and deactivate layers here!!!! by using #
ggplot()+ 
  geom_point(data=id_112695,aes(date, power,group=1))+
  geom_point(data=id_112718,aes(date, power,group=1))
  
    scale_colour_gradientn(colours=rainbow(10))


###It works the picker function!!!
library(manipulate)
manipulate({
  ggplot(data,aes(hour, power,group=1))+ geom_bar(width=0.9,aes(fill=power),stat = "identity")+
  xlab("hours[h]")+ylab("consumption[kWH]")+ggtitle("Hourly Mean Consumption")+
    scale_y_continuous(breaks=c(seq(0,7500,by=500)))
  },
  main="idmeter"  ,    
  data = picker("112917"=holo112917,"102802"=holo102802,"112873"=holo112873,"112789"=holo112789,"112980"=holo112980,label="Idmeters")
)
