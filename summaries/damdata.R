#Retrieves dam data from DART

require(dplyr)
require(lubridate)

getDamData <- function(proj,year){
  url <- paste("http://www.cbr.washington.edu/dart/cs/php/rpt/river_daily.php?sc=1&outputFormat=csv&year=",year,"&proj=",proj,"&span=no&startdate=1%2F1&enddate=12%2F31",sep="")
  df <- read.csv(url) %>% filter(Date != "") %>% select(1,2,3,4,6,7,8,11,14)
  return(df)
  #print(url)
}

getAllDams <- function(year){
  dams1 <- c("LWG","LGS","LMN","IHR","MCN","JDA","TDA","BON")
  dams2 <- c("LGNW","LGSW","LMNW","IDSW","MCPW","JHAW","TDDO","CCIW")
  df1 <- data.frame()
  df2 <- data.frame()
  for(i in 1: length(dams1))df1 <- rbind(df1,getDamData(dams1[i],year))
  names(df1) <- c("Project","Date","Outflow","Spill","Inflow","TempSC","Temp","Dgas","Elev")
  for(i in 1: length(dams2))df2 <- rbind(df2,getDamData(dams2[i],year))
  names(df2) <- c("Project2","Date","Outflow","Spill","Inflow","TempSC","TempTW","DgasTW","ElevTW")
  df2 <- merge(df2,data.frame(Project2=dams2,Project=dams1))
  df <- merge(df1,select(df2,Project,Date,TempTW,DgasTW,ElevTW))
  return(df)
}

getAllYears <- function(){
  df <- data.frame()
  for(i in 2005:2018)df <- rbind(df,getAllDams(i))
  return(df)
}
