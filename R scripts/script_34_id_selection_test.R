####  Import database
setwd("~/SELECT ERASMUS MUNDUS/ENERBYTE- Thesis&internship/THESIS - Enerbyte/Part 3. Data analysis/R for Tesi/test_01_tesi")
    rubi <- read.csv("~/SELECT ERASMUS MUNDUS/ENERBYTE- Thesis&internship/THESIS - Enerbyte/Part 3. Data analysis/R for Tesi/test_01_tesi/data_id_selection.csv", sep=";", stringsAsFactors=FALSE)
      rubi<-rubi[(rubi$source == "CURRENTCOST"),]
          rubi$source <-NULL
              rubi$idmeter<-as.numeric(rubi$idmeter)
      rubi$date <-NULL

  a<-as.data.frame(unique(rubi$idmeter))
      names(a)<-"idmeters selected"

###1. Get the sum of each row 
  rubi_net<-rubi[2:25]
      row_sub <- apply(rubi_net, 1, function(row) all(row !=0 )) #Go through each row and determine if a value is zero
         rubi<-rubi[row_sub,]

### 2. frozens
  x<-rubi[2:25]
    y<-x[-1]
      diff <- y-x[1:length(x)-1]
  rubi_fr<-rubi[apply(diff==0,1,sum)<=6,]               ## more than six 0 per row, remove the row

## test if frozen code works well
  ho<-as.data.frame(rowSums(diff==0)<=6)                ## more than four 0 per row, remove the row
      names(ho)<-"ji"
        del<-as.data.frame(ho[ho$ji==FALSE,])
    rubi_del<-rubi[ho$ji==FALSE,]
