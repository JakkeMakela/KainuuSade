



infile <- "sade20132015.csv"

indata <- read.csv(infile)


indata <- indata[substr(indata$aika,5,5)=="-" &
                   substr(indata$aika,8,8)=="-" ,]

indata$year <- NULL
indata$month <- NULL
for (i in 1:nrow(indata)){
  indata$year[i] <- as.numeric(substr(indata$aika[i],1,4))
  indata$month[i] <- as.numeric(substr(indata$aika[i],6,7))  
}