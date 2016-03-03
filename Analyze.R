#Analyze.R
#Jakke Mäkelä, 2016-01-04
#Analysoi Kainuun sääasemien sateita

library(zoo)

NumOfMonths <- 12

PlotType <- 1

infile <- "Sateisuus.csv"
rain <- read.csv(infile)
StationNames <- c("Kaarakkala","Paltaniemi","Saaresmaki","Saviaho")


for (station in StationNames){
  
  #Laskee sateisuuden ottamalla liukuvan keskiarvon edellisten 12 kk
  #sateista ja kertomalla 12:lla.
  #Toimii myös silloin vaikka olisi NA:ta
  temp.sum <- 12*rollapply(rain[,station],mean,
                    width=NumOfMonths,fill=NA,
                    align="left",
                    na.rm=TRUE)
  
  #Jos yli puolet pisteistä on NA, asetetaan keskiarvoksi NA
  nafun <- function(x) sum(is.na(x))/length(x)>0.5
  temp.nas.exist <- rollapply(rain[,station],nafun,
                        width=NumOfMonths,fill=NA,
                        align="left")

  temp.sum[which(temp.nas.exist)] <- NA
  cnam.sum <- paste("Roll.",station,sep="")
  rain[,cnam.sum] <- temp.sum  
  
  
  temp.sd <- rollapply(rain[,station],sd,
                    width=NumOfMonths,fill=NA,
                    align="left",
                    na.rm=TRUE)
  temp.sd[which(temp.nas.exist)] <- NA
  
  cnam.sd <- paste("SD.",station,sep="")
  rain[,cnam.sd] <- temp.sd
}

if (PlotType==1){
  Year0 <- 1960
  Year1 <- 2016
  mm0 <- 400
  mm1 <- 900
  xl <- c(Year0, Year1)
  yl <- c(mm0,mm1)
  ttl <- paste("12 kk sadesumma, liukuma ", round(NumOfMonths/12), "vuotta")
  ytxt <- "mm / 12 kk"
  ygrid <- 50
  rain$sav <- rain$Roll.Saviaho
  rain$pal <- rain$Roll.Paltaniemi
  rain$kaa <- rain$Roll.Kaarakkala
  lm.sav <- lm(sav~VUOSI.DEC,data=rain)
  lm.pal <- lm(pal~VUOSI.DEC,data=rain)
  lm.kaa <- lm(kaa~VUOSI.DEC,data=rain)
}

if (PlotType==1.1){
  Year0 <- 1990
  Year1 <- 2016
  mm0 <- 500
  mm1 <- 750
  xl <- c(Year0, Year1)
  yl <- c(mm0,mm1)
  ttl <- paste("12 kk sadesumma, liukuma ", round(NumOfMonths/12), "vuotta")
  ytxt <- "mm / 12 kk"
  ygrid <- 50
  rain$sav <- rain$Roll.Saviaho
  rain$pal <- rain$Roll.Paltaniemi
  rain$kaa <- rain$Roll.Kaarakkala
  lm.sav <- lm(sav~VUOSI.DEC,data=rain)
  lm.pal <- lm(pal~VUOSI.DEC,data=rain)
  lm.kaa <- lm(kaa~VUOSI.DEC,data=rain)
}

###############
if (PlotType==2){
  Year0 <- 1990
  Year1 <- 2016
  mm0 <- 0.95
  mm1 <- 1.125
  xl <- c(Year0, Year1)
  yl <- c(mm0,mm1)  
  ygrid <- 0.025
  rain$sav <- (rain$Roll.Saviaho/rain$Roll.Saviaho[rain$VUOSI.DEC==1990])  
  rain$pal <- (rain$Roll.Paltaniemi/rain$Roll.Paltaniemi[rain$VUOSI.DEC==1990])
  rain$kaa <- (rain$Roll.Kaarakkala/rain$Roll.Kaarakkala[rain$VUOSI.DEC==1990])
  #lm.saa <- lm(saa~VUOSI.DEC,data=rain)  
  lm.sav <- lm(sav~VUOSI.DEC,data=rain)
  lm.pal <- lm(pal~VUOSI.DEC,data=rain)
  lm.kaa <- lm(kaa~VUOSI.DEC,data=rain)
  ttl <- paste("Suhteellinen 12 kk sadesumma, liukuma ", round(NumOfMonths/12), "vuotta")
  
  ytxt <- "mm / 12 kk, 1990=1.00"
}

if (PlotType==3){
  Year0 <- 1990
  Year1 <- 2016
  mm0 <- 500
  mm1 <- 750
  xl <- c(Year0, Year1)
  yl <- c(mm0,mm1)
  ttl <- "Vuoden sademäärä, 30 vuoden liukuva keskiarvo"
  ytxt <- "mm / 12 kk"
  ygrid <- 25
  rain$sav <- rain$Roll.Saviaho
  rain$saa <- rain$Roll.Saaresmaki
  rain$pal <- rain$Roll.Paltaniemi
  rain$kaa <- rain$Roll.Kaarakkala

}


if (PlotType %in% c(1,1.1,2)){
  
  plot(rain$VUOSI.DEC,rain$sav, 
       xlim=xl,ylim=yl,type="l",col="red",xlab="",ylab="")
  abline(lm.sav$coefficients,col="red",lwd=2)
  par(new=TRUE)
  
  plot(rain$VUOSI.DEC,rain$pal, 
       xlim=xl,ylim=yl,type="l",col="blue",xlab="",ylab="")
  abline(lm.pal$coefficients,col="blue",lwd=2)
  par(new=TRUE)
  
  plot(rain$VUOSI.DEC,rain$kaa, 
       xlim=xl,ylim=yl,type="l",col="black",
       xlab="Vuosi",ylab=ytxt,
       main=ttl)
  abline(lm.kaa$coefficients,col="black",lwd=2)
  
  abline(v=seq(Year0,Year1,5),col="darkgray")
  abline(h=seq(mm0,mm1,ygrid),col="darkgray")
}

if (PlotType==4){
  hist(rain$Roll.Kaarakkala, 100, 
       main="Kaarakkala", xlab = "12 kk sade",ylab="N"
       )
}

