setwd("C:/Users/ziqi/Uconn/5225 Data management and programming in R and SAS/mini project")
library(readxl)
library(sqldf)

#Read and transform dataset 
for (i in 1999:2019){
  crime = read_excel(paste(i,".xls",sep=""))
  if (i<=2004){
    crimerate <- sqldf("select * from crime where `Table 5` = 'Rate per 100,000 inhabitants'")
    if (i<=2002){
      crimerate <- crimerate[-c(1:3,11,15)]
    }else{
      crimerate <- crimerate[c(2:11)]#2003-2004
    }
  }else if(i>=2013 & i<=2016){
    crimerate = crime[which(crime[,3]=='Rate per 100,000 inhabitants'),c(4:6,8:14)]#2005-2012
  }else{
    crimerate = crime[which(crime[,3]=='Rate per 100,000 inhabitants'),c(4:13)]
  }
  if (i<=2002){
    names(crimerate) = c("STATE","VIOLENTCRIME","PROPERTYCRIME","MURDER","RAPE","ROBBERY","AGGRAVATEDASSAULT","BURGLARY","LARCENYTHEFT","MOTORVEHICLETHEFT")
  }else{
    names(crimerate) = c("STATE","VIOLENTCRIME","MURDER","RAPE","ROBBERY","AGGRAVATEDASSAULT","PROPERTYCRIME","BURGLARY","LARCENYTHEFT","MOTORVEHICLETHEFT")
  }
  if (i==2010){
    crimerate$STATE = sort(c(state.name,'District of Columbia'))
  }else{
    crimerate$STATE = sort(c(state.name,'District of Columbia','Puerto Rico'))
  }
  crimerate$YEAR<-(rep(i,length(crimerate$STATE)))
  numericcols = names(crimerate)[2:10]
  crimerate[numericcols] = lapply(lapply(crimerate[numericcols],as.character),as.numeric)
  assign(paste("c",i,sep=""),crimerate)
}

#combine dataset
c9902<-rbind(c1999,c2000,c2001,c2002)
library(dplyr)
c9902 <- c9902 %>% select(STATE,VIOLENTCRIME,MURDER,RAPE,ROBBERY,AGGRAVATEDASSAULT,PROPERTYCRIME,BURGLARY,LARCENYTHEFT,MOTORVEHICLETHEFT,YEAR)
Total<-rbind(c9902,c2003,c2004,c2005,c2006,c2007,c2008,c2009,c2010,c2011,c2012,c2013,c2014,c2015,c2016,c2017,c2018,c2019)
write.csv(x = Total,file = "crimerate.csv",row.names=F)
save(Total, file = "crimerate.RData")

