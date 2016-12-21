infocasa_all <- read.csv("~/SELECT ERASMUS MUNDUS/ENERBYTE- Thesis&internship/THESIS - Enerbyte/Chapter 2. Case study Data analysis/R for Tesi/test_01_tesi/infocasa_rubí.csv", sep=";", stringsAsFactors=FALSE)
quants<-as.data.frame(unique(infocasa_all$idmeter))
names(quants)<-"idmeter"
char<-as.data.frame(unique(infocasa_all$variable))

infocasa_all1<-infocasa_all[,2:4]
infocasa_all1$value<-as.numeric(infocasa_all1$value)
str(infocasa_all1)
quants1<-as.data.frame(sort(quants$idmeter))

library(reshape2)

c_18<-dcast(infocasa_all1, idmeter  ~ variable, fun.aggregate=median, na.rm=T)
c_18<-round(c_18,1)
a<-infocasa_all[infocasa_all$idmeter== 90713,]
c_18$GE_TIP_JardiAmbRegAutomatic<-NULL
c_18$GE_TIP_Piscina<-NULL
c_18$AA_US_TempConsigna<-NULL
c_18$CA_US_TempConsigna<-NULL
c_18$CA_CALELE_Tipus_Electric<-NULL
c_18$CU_TIPUS_Induccio<-NULL
c_18$CU_TIPUS_Vitroceramica<-NULL

audits <- read.csv("~/SELECT ERASMUS MUNDUS/ENERBYTE- Thesis&internship/THESIS - Enerbyte/Chapter 2. Case study Data analysis/R for Tesi/test_01_tesi/audit_technic.csv", sep=";", stringsAsFactors=FALSE)
joint<-as.data.frame(cbind(audits,c_18[,2:13]))
joint$clus_num<-as.numeric(joint$clus_num)
p2<-joint
str(p2)
library(sqldf)
cluster1<-sqldf("select *  from joint where clus_num = '1'")
cluster2<-sqldf("select *  from joint where clus_num = '2'")
cluster3<-sqldf("select *  from joint where clus_num = '3'")
cluster4<-sqldf("select *  from joint where clus_num = '4'")
cluster5<-sqldf("select *  from joint where clus_num = '5'")
cluster6<-sqldf("select *  from joint where clus_num = '6'")
cluster7<-sqldf("select *  from joint where clus_num = '7'")


library(ggplot2)

DF<-sqldf("select idmeter,GE_POT_CONTRACTED from joint where GE_POT_CONTRACTED != 'NA'")
qplot(joint$GE_POT_CONTRACTED,data=joint, geom="histogram")

clus2<-as.data.frame(c("112718","112724","112739","112773","112788","112826","112862","112868","112873","112880","112886","112890","112931","112932","112997","113020","1049953413"))
names(clus2)<-"clus2"
