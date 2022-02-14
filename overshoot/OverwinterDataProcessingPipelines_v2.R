library(tidyverse)
library(lubridate)
library(cowplot)
library(ggExtra)

getMultiDamData <- function(year){
  url1 <- "http://www.cbr.washington.edu/dart/cs/php/rpt/mg.php?sc=1&mgconfig=river&outputFormat=csvSingle&loc%5B%5D=BON&loc%5B%5D=IHR&loc%5B%5D=JDA&loc%5B%5D=LGS&loc%5B%5D=LWG&loc%5B%5D=LMN&loc%5B%5D=MCN&loc%5B%5D=PRD&loc%5B%5D=RIS&loc%5B%5D=RRH&loc%5B%5D=TDA&loc%5B%5D=WEL&data%5B%5D=Dissolved+Gas+Percent&data%5B%5D=Elevation&data%5B%5D=Inflow&data%5B%5D=Outflow&data%5B%5D=Spill&data%5B%5D=Temp+%28Scroll+Case%29&data%5B%5D=Temp+%28WQM%29&startdate=1%2F1&enddate=12%2F31&avgyear=0&consolidate=1&grid=1&y1min=0&y1max=&y2min=&y2max=&size=medium&year%5B%5D="
  url <- paste(url1,year,sep="")
  df <- read.csv(url) %>% select(1,2,3,4,7) %>% filter(mm.dd != "") %>% mutate(date=paste(year,mm.dd,sep="-"))
  df1 <- df %>% pivot_wider(names_from = parameter, values_from = value) %>% select(-2)
  return(df1)
}

getSingleDam <- function(dam){
  url1 <- "http://www.cbr.washington.edu/dart/cs/php/rpt/mg.php?sc=1&mgconfig=river&outputFormat=csv&year%5B%5D=2020&year%5B%5D=2019&year%5B%5D=2018&year%5B%5D=2017&year%5B%5D=2016&year%5B%5D=2015&year%5B%5D=2014&year%5B%5D=2013&year%5B%5D=2012&year%5B%5D=2011&year%5B%5D=2010&year%5B%5D=2009&year%5B%5D=2008&year%5B%5D=2007&year%5B%5D=2006&year%5B%5D=2005&year%5B%5D=2004&year%5B%5D=2003&year%5B%5D=2002&year%5B%5D=2001&year%5B%5D=2000&data%5B%5D=Dissolved+Gas+Percent&data%5B%5D=Elevation&data%5B%5D=Outflow&data%5B%5D=Spill&data%5B%5D=Temp+%28Scroll+Case%29&data%5B%5D=Temp+%28WQM%29&startdate=1%2F1&enddate=12%2F31&avgyear=0&consolidate=1&grid=1&y1min=0&y1max=&y2min=&y2max=&size=medium"
  url <- paste(url1,"&loc%5B%5D=",dam,sep="")
  df <- read.csv(url)
  return(df)  
}

getAllDamData <- function(){
  df <- data.frame()
  for(i in 2010:2020){
    print(i)
    df <- rbind(df,getMultiDamData(i))
  }
  return(df)
}

getTransportDates <- function(){
  print("Fetching from DART")
  url <- "http://www.cbr.washington.edu/dart/cs/php/rpt/mg.php?sc=1&mgconfig=transport&outputFormat=csvSingle&year%5B%5D=2021&year%5B%5D=2020&year%5B%5D=2019&year%5B%5D=2018&year%5B%5D=2017&year%5B%5D=2016&year%5B%5D=2015&year%5B%5D=2014&year%5B%5D=2013&year%5B%5D=2012&year%5B%5D=2011&year%5B%5D=2010&year%5B%5D=2009&year%5B%5D=2008&year%5B%5D=2007&year%5B%5D=2006&year%5B%5D=2005&year%5B%5D=2004&year%5B%5D=2003&year%5B%5D=2002&year%5B%5D=2001&year%5B%5D=2000&loc%5B%5D=LWG&loc%5B%5D=LGS&loc%5B%5D=LMN&loc%5B%5D=MCN&data%5B%5D=Transported&ftype%5B%5D=Chin0&ftype%5B%5D=Chin1&ftype%5B%5D=Coho&ftype%5B%5D=Sock&ftype%5B%5D=Stlhd&data%5B%5D=&startdate=1%2F1&enddate=12%2F31&sumAttribute=none&consolidate=1&zeros=1&grid=1&y1min=0&y1max=&y2min=&y2max=&size=medium"
  tdates <- read.csv(url,stringsAsFactors=F) %>% filter(!is.na(value),mm.dd != "")
  tdates$tdate <- ymd(paste(tdates$year,tdates$mm.dd,sep="-"))
  tdates <- merge(tdates,data.frame(parameter=c("trans:Sthd","trans:Ch0","trans:Ch1","trans:Coho","trans:Sock"),species=c("Steelhead","Chinook","Chinook","Coho","Sockeye")),stringsAsFactors=F)
  tdates <- merge(data.frame(location=c("LWG","LGS","LMN","MCN"),obs_site=c("GRJ","GOJ","LMJ","MCJ")),tdates)
  tdates <- tdates %>% group_by(species,year,tdate,obs_site) %>% summarise(count=sum(value))
  tdates <- tdates %>% filter(count>0)
  print("Returned from DART")
  #merge this
  tmatrix <- read.csv("https://raw.githubusercontent.com/bmasch/fishtracks/master/TransportationMatrix.csv",stringsAsFactors=F)
  tmatrix <- filter(tmatrix,transported==T) %>% select(1,2) %>% unique()
  names(tmatrix) <- c("obs_site","ant")
  tdates <- merge(tdates,tmatrix)
  tdates <- select(tdates,species,obs_site,ant,tdate)
  tdates$transported <- T
  #write.csv(tdates,"TransportedDates_2000-2020.csv",row.names=F)
  return(tdates)
}


convertRaw <- function(df){
  renamePTAGIS = list()
  renamePTAGIS["Tag.Code"]="tag_id"
  renamePTAGIS["Release.Site.Code.Value"]="rel_site"
  renamePTAGIS["Release.Date.MMDDYYYY"]="rel_date"
  renamePTAGIS["Event.Site.Code.Value"]="obs_site"
  renamePTAGIS["Event.Date.Time.Value"]="date"
  renamePTAGIS["Event.Date.MMDDYYYY"]="date"
  renamePTAGIS["Event.Release.Date.Time.Value"]="rdate"
  renamePTAGIS["Event.Length.mm"]="length"
  renamePTAGIS["CTH.Count"]="count"
  renamePTAGIS["Event.Type.Name"]="etype"
  renamePTAGIS["Rear.Type.Code"]="rtype"
  renamePTAGIS["Rear.Type.Name"]="rtype_name"
  renamePTAGIS["Site.Code.Value"]="obs_site"
  renamePTAGIS["First.Antenna.Group.Name"]="mon1"
  renamePTAGIS["Last.Antenna.Group.Name"]="mon2"
  renamePTAGIS["Antenna.Group.Name"]="ant"
  renamePTAGIS["Antenna.ID"]="ant_id"
  renamePTAGIS["First.Time.Value"]="date1"
  renamePTAGIS["Last.Time.Value"]="date2"
  renamePTAGIS["Obs.Time.Value"]="date"
  renamePTAGIS["Count"]="count"
  renamePTAGIS["Obs.Count"]="count"
  renamePTAGIS["Mark.Count"]="count"
  renamePTAGIS["Run.Code"]="run_code"
  renamePTAGIS["Run.Name"]="run"
  renamePTAGIS["Mark.Length.mm"]="length"
  renamePTAGIS["Mark.Weight.g"]="weight"
  renamePTAGIS["Mark.Run.Name"]="run"
  renamePTAGIS["Mark.File.Name"]="filename"
  renamePTAGIS["Mark.Rear.Type.Name"]="rtype_name"
  renamePTAGIS["Mark.Species.Name"]="species"
  renamePTAGIS["Mark.Species.Code"]="species_code"
  renamePTAGIS["Release.Site.Subbasin.Code"]="subbasin"
  renamePTAGIS["Release.Site.Subbasin.Name"]="subbasin_name"
  renamePTAGIS["Release.Site.RKM.Value"]="rel_rkm"
  old.names <- names(df)
  new.names <- renamePTAGIS[old.names[1]]
  for(i in 2:length(old.names))new.names[i]=renamePTAGIS[old.names[i]]
  names(df) <- new.names
  return(df)
}

fixRecaptureDate <- function(df){
  df$date <- ifelse(df$etype=="Recapture" & df$rdate != "",df$rdate,df$date)
  df$rdate <- NULL
  return(df)
}

removeComma <- function(vect){
  return(as.numeric(gsub(",","",as.character(vect), fixed = TRUE)))
}

getNext <- function(field){
  nxt <- c(field[2:length(field)],"NA")
  return(nxt)
}

getPrevious <- function(field){
  n <- length(field)-1
  prev <- c("NA",field[1:n])
  return(prev)
}

assignICH <- function(df){
  df$obs_site <- ifelse(df$obs_site=="ICH" & df$ant == "FULL FLOW BYPASS","ICHFFB",df$obs_site)
  return(df)
}

convertRawHistory <- function(df){
  df <- convertRaw(df)
  df <- merge(df,data.frame(rtype=c("U","W","H"),rtype_name=c("Unknown","Wild Fish or Natural Production","Hatchery Reared")),stringsAsFactors=F)
  df$rtype_name <- NULL
  df.mark <- df %>% filter(etype=="Mark") %>% select(tag_id,length)
  df.other <- df %>% filter(etype!="Mark",etype!= "Mark Duplicate") %>% select(-length)
  #df <- merge(df.other,df.mark) %>% assignICH()
  df <- merge(df.other,df.mark)
  return(df)
}

fixTime <- function(df){
  df %>% mutate(date=as.POSIXct(as.numeric(date), origin = "1970-01-01", tz = "GMT")) -> df
  df %>% mutate(next.date=as.POSIXct(as.numeric(next.date), origin = "1970-01-01", tz = "GMT")) -> df
  df %>% mutate(rel_date=as.Date(as.numeric(rel_date), origin = "1970-01-01", tz = "GMT")) -> df
  return(df)
}

addDTime <- function(df3){
  df3$dtime <- as.numeric(difftime(ymd_hms(as.character(df3$next.date)),ymd_hms(as.character(df3$date)),units="hours"))
  return(df3)
}

fixDateTime <- function(dates){
  fixeddates <- as.POSIXct(as.numeric(dates), origin = "1970-01-01", tz = "GMT")
  return(fixeddates)
}

getDiffTime <- function(date1,date2,units){
  #dtime <- as.numeric(difftime(ymd_hms(as.character(date2)),ymd_hms(as.character(date1)),units=units))
  dtime <- as.numeric(difftime(date1,date2,units=units))
  return(dtime)
}

#problems with this
deleteDupes <- function(df3,minhours){
  df3$prev.tag_id <- getPrevious(df3$tag_id)
  df3$prev.obs_site <- getPrevious(df3$obs_site)
  df3$prev.ant <- getPrevious(df3$ant)
  df3$prev.date <- fixDateTime(getPrevious(df3$date))
  df3$dtime <- getDiffTime(df3$date,df3$prev.date,"hours")
  df3$delete <- ifelse(df3$prev.tag_id == df3$tag_id & df3$prev.obs_site == df3$obs_site & df3$prev.ant == df3$ant & df3$dtime<minhours,T,F)
  df3 <- filter(df3,df3$delete==T)
  df3$prev.tag_id <- NULL
  df3$prev.date <- NULL
  df3$prev.obs_site <- NULL
  df3$prev.ant <- NULL
  df3$dtime <- NULL
  df3$delete <- NULL
  return(df3)
}

#just for testing
testTracks <- function(infile,tmatrix,minhours){
  df1 <- infile
  df1$date <- mdy_hms(df1$date)
  df1$tdate <- floor_date(df1$date,unit="day")
  df1$rel_date <- mdy(df1$rel_date)
  df2 <- merge(df1,tmatrix,all.x=T)
  df2$transported <- ifelse(is.na(df2$transported),FALSE,df2$transported)
  df3 <- arrange(df2,tag_id,date)
  df3 <- deleteDupes(df3,minhours)
  return(df3)
}


getTracks <- function(infile,tmatrix){
  df1 <- infile
  df1$date <- fixDateTime(mdy_hms(df1$date))
  df1$tdate <- floor_date(df1$date,unit="day")
  df1$rel_date <- fixDateTime(mdy(df1$rel_date))
  df2 <- merge(df1,tmatrix,all.x=T)
  df2$transported <- ifelse(is.na(df2$transported),FALSE,df2$transported)
  df3 <- arrange(df2,tag_id,date)
  #df3 <- deleteDupes(df3,2)
  df3$next.tag_id <- getNext(df3$tag_id)
  df3$next.obs_site <- getNext(df3$obs_site)
  df3$next.date <- fixDateTime(getNext(df3$date))
  df3$next.ant <- getNext(df3$ant)
  df3$next.date[nrow(df3)] <- df3$date[nrow(df3)]
  #return(addDTime(fixTime(df3)))
  return(addDTime(df3))
}

getTracks2 <- function(infile,tmatrix, tmax){
  df1 <- infile
  df1$date <- fixDateTime(mdy_hms(df1$date))
  df1$tdate <- floor_date(df1$date,unit="day")
  df1$rel_date <- fixDateTime(mdy(df1$rel_date))
  df2 <- merge(df1,tmatrix,all.x=T)
  df2$transported <- ifelse(is.na(df2$transported),FALSE,df2$transported)
  df3 <- arrange(df2,tag_id,date)

  df3$prev.tag_id <- getPrevious(df3$tag_id)
  df3$prev.obs_site <- getPrevious(df3$obs_site)
  df3$prev.ant <- getPrevious(df3$ant)
  df3$prev.date <- getPrevious(df3$date)
  df3$prev.date[1] <- df3$date[1]
  df3$prev.date <- fixDateTime(df3$prev.date)
  
  df3$dtime <- as.numeric(difftime(df3$date,df3$prev.date,units="hours"))
  
  df3$new.obs <- ifelse(df3$tag_id != df3$prev.tag_id | df3$obs_site != df3$prev.obs_site | df3$dtime > tmax,1,0)
  df3$new.obs[1] <- 1
  
  df3$new.ant <- ifelse(df3$tag_id != df3$prev.tag_id | df3$obs_site != df3$prev.obs_site | df3$ant != df3$prev.ant,1,0)
  df3$new.ant[1] <- 1  
  
  df3$obs.block <- cumsum(df3$new.obs)
  df3$ant.block <- cumsum(df3$new.obs + df3$new.ant)
  df3$block <- paste(df3$obs.block)
  df4 <- df3 %>% group_by(tag_id,obs.block) %>% summarise(mindate=min(date),maxdate=max(date))
  df5 <- merge(df3,df4)
  df5$time <- as.numeric(difftime(df5$date,df5$mindate,units="hours"))
  return(df5)
}

getTagged <- function(df,names,values,method="intersect"){
  #see: https://stackoverflow.com/questions/48219732/pass-a-string-as-variable-name-in-dplyrfilter
  tags <- data.frame()
  for(i in 1:length(names)){
    name <- names[i]
    value <- values[i]
    if(method=="union")tags <- tags %>% rbind(df %>% filter(eval(parse(text=name)) == value) %>% select(tag_id))
    else{
      if(nrow(tags) > 0)tags <- tags %>% merge(df %>% filter(eval(parse(text=name)) == value) %>% select(tag_id))
      else tags <- df %>% filter(eval(parse(text=name)) == value) %>% select(tag_id)
    }
  }
  return(tags %>% unique())
}

#example
#sh2020<- read.csv("PITdata/Steelhead2020.csv",stringsAsFactors=F) %>% convertRawHistory() %>% mutate(species="Steelhead") %>% fixRecaptureDate()


addBlocks2 <- function(df,tmax){
#version which breaks at antenna group  
  df$blockstart <- NA
  df$ant <- ifelse(is.na(df$ant),"None",df$ant)
  df$next.ant <- ifelse(is.na(df$next.ant),"None",df$next.ant)
  blockstart <- df$date[1]
  df$reset <- ifelse(df$tag_id != df$next.tag_id | df$obs_site != df$next.obs_site | df$next.ant != df$ant | df$dtime > tmax,T,F)
  for(i in 1:(nrow(df)-1)){
  #  for(i in 1:10000){
    df$blockstart[i] <- blockstart
    if(df$reset[i])blockstart <- df$next.date[i]
  }
  df <- df %>% mutate(blockstart=as.POSIXct(blockstart, origin = "1970-01-01"))
  return(df)
}

markBlocks <- function(df,tmax){
  #version which breaks at antenna group  
  df$ant <- ifelse(is.na(df$ant),"None",df$ant)
  df$next.ant <- ifelse(is.na(df$next.ant),"None",df$next.ant)
  df$reset <- ifelse(df$tag_id != df$next.tag_id | df$obs_site != df$next.obs_site | df$next.ant != df$ant | df$dtime > tmax,1,0)
  block <- 0
  df$reset <- getPrevious(df$reset)
  df$reset[1] <- 0
  df$block <- cumsum(df$reset)
  df$block <- as.factor(df$block)
  return(df)
}


addBlocks <- function(df,tmax){
  df$blockstart <- NA
  blockstart <- df$date[1]
  df$reset <- ifelse(df$tag_id != df$next.tag_id | df$obs_site != df$next.obs_site | df$dtime > tmax,T,F)
  for(i in 1:(nrow(df)-1)){
  #for(i in 1:10000){
    df$blockstart[i] <- blockstart
    if(df$reset[i])blockstart <- df$next.date[i]
  }
  df <- df %>% mutate(blockstart=as.POSIXct(blockstart, origin = "1970-01-01"))
  return(df)
}

collapseBlocks <- function(df){
  df <- df %>% group_by(tag_id,species,run,rtype,subbasin,subbasin_name,rel_site,rel_date,etype,obs_site,length,blockstart) %>% summarise(transported=max(transported),startdate=min(date),enddate=max(date),date=startdate,time=as.numeric(enddate)-as.numeric(startdate))
  df %>% select(-blockstart,-startdate,-enddate) %>% return()
}

collapseBlocks2 <- function(df){
  #preserves blockstart, changing to factor
  df <- df %>% group_by(tag_id,species,run,rtype,subbasin,subbasin_name,rel_site,rel_date,etype,obs_site,length,blockstart) %>% summarise(transported=max(transported),startdate=min(date),enddate=max(date),date=startdate,time=as.numeric(enddate)-as.numeric(startdate))
  df %>% select(-startdate,-enddate) %>% return()
}


getDamDataSingleYear <- function(year){
  url1 <- "http://www.cbr.washington.edu/dart/cs/php/rpt/mg.php?sc=1&mgconfig=adult&outputFormat=csvSingle&year%5B%5D="
  url2 <- "&loc%5B%5D=BON&loc%5B%5D=IHR&loc%5B%5D=JDA&loc%5B%5D=LGS&loc%5B%5D=LMN&loc%5B%5D=LWG&loc%5B%5D=MCN&loc%5B%5D=PRD&loc%5B%5D=RIS&loc%5B%5D=RRH&loc%5B%5D=ROZ&loc%5B%5D=TDA&loc%5B%5D=WAN&loc%5B%5D=WEL&ftype%5B%5D=fc&data%5B%5D=&data%5B%5D=Dissolved+Gas+Percent&data%5B%5D=Elevation&data%5B%5D=Inflow&data%5B%5D=Outflow&data%5B%5D=Spill&data%5B%5D=Temp+%28Scroll+Case%29&data%5B%5D=Temp+%28WQM%29&startdate=1%2F1&enddate=12%2F31&avgyear=0&sumAttribute=none&consolidate=1&zeros=1&grid=1&y1min=0&y1max=&y2min=&y2max=&size=medium"
  url <- paste(url1,year,url2,sep="")
  df <- read.csv(url,stringsAsFactors=F) %>% filter(mm.dd != "",datatype != "Adult Passage") 
  df$date <- paste(df$year,df$mm.dd,sep="-")
  df <- df %>% select(date,location,parameter,value)
  df <- pivot_wider(df,names_from=parameter,values_from=value) %>% mutate(temp=ifelse(is.na(tempc),tempscr,tempc)) %>% select(-tempc,-tempscr)
  return(df)
}

getDamData <- function(){
  for(year in 2010:2020){
    df <- getDamDataSingleYear(year)
    print(year)
    write.csv(df,paste("DamData",year,".csv",sep=""),row.names=F)
  }
}


#might need to re-standardize date column
#blah blah blah... %>%
#mutate(my_column = as.Date(as.numeric(my_column), origin = "1970-01-01")) %>%
 # left_join(blah blah ...

#example

getLastBasin <- function(df,year){
  df1 <- df %>% filter(year(date) == year) %>% group_by(tag_id,obs_basin) %>% summarise(date=max(date))
  return(merge(df,df1))
}

getFirstBasin<- function(df,year){
  df1 <- df %>% filter(year(date) == year) %>% group_by(tag_id,obs_basin) %>% summarise(date=min(date))
  return(merge(df,df1))
}


sh2010 <- read.csv("PITdata/Steelhead2010.csv",stringsAsFactors=F) %>% convertRawHistory() %>% getTracks(tmatrix) %>% select(3,4,13,7,8,6,9,15,14,10,1,2,11)


#run this in the browser
#Object.keys(sites_basins).forEach(function(site){
#  console.log(sites_basins[site] + "," + basins_sites[sites_basins[site]].name + "," + site)
#})

sites.basins <- read.csv("sites_basins_indexed.csv",stringsAsFactors=F)

attributeSites <- function(df,index,year1){
  df1 <- df %>% merge(data.frame(rel_index = index$id,rel_basin=index$name,rel_site=index$site)) %>% merge(data.frame(obs_index = index$id,obs_basin=index$name,obs_site=index$site,stringsAsFactors=F))
  #df2 <- filter(df1,obs_index<rel_index)
  #df3 <- rbind(getLastBasin(df2,year),getFirstBasin(df2,year+1))
  df3 <- df1
  df3$year <- year(df3$date)
  df3 <- filter(df3,year >= year1)
  df3$return_year <- year1
  df3$overwintered <- df3$year > year1
  basins <- index %>% select(id,name) %>% unique() %>% arrange(id)
  basins <- basins$name
  df3$obs_basin <- factor(df3$obs_basin,levels=rev(basins))
  df3$location <- ifelse(df3$obs_index<df3$rel_index,"overshoot",ifelse(df3$obs_index>df3$rel_index,"below","inbasin"))
  df3$yearname <- ifelse(df3$overwintered,"year2","year1")
  df3$state <- paste(df3$yearname,df3$location)
  return(df3)
}

removeAnonReleases <- function(df){
  asites <- c("BONAFF","LGRLDR","LGRRRR","LGRRBR","LGRRTR","LGRTAL","LGSTAL","LMNBYP","LMNTAL","LGRLDR","RI2BYP","RISTAL","PRDLD1","PRDTAL","MCNTAL","IHRBYP","SNAKE2","SNAKE1","WELH","RINH","WELTAL")
  aframe <- data.frame(rel_site=asites,yank=T)
  df1 <- merge(df,aframe,all.x=T) %>% filter(is.na(yank)) %>% select(-yank)
  return(df1)
}

setNextState <- function(df){
    df <- arrange(df,tag_id,date)
    df$next_state <- c(df$state[2:nrow(df)],"undetected")
    df$next_basin <- c(as.character(df$obs_basin[2:nrow(df)]),"undetected")
    df$next_tag <- c(df$tag_id[2:nrow(df)],"undetected")
    df$next_state <- ifelse(df$tag_id==df$next_tag,df$next_state,"undetected")
    df$next_basin <- ifelse(df$tag_id==df$next_tag,df$next_basin,"undetected")
    return(df)
}


#load raw files and save tracks file
writeTrackFile <- function(tmatrix,year){
  infile <- paste("PITdata/Steelhead",year,".csv",sep="")
  outfile <- paste("SteelheadOverwinter",year,".csv",sep="")
  read.csv(infile,stringsAsFactors=F) %>% convertRawHistory() %>% removeAnonReleases() %>% fixRecaptureDate() %>% getTracks(tmatrix) %>% select(3,4,13,7,8,6,9,15,14,10,1,2,11) %>% write.csv(outfile,row.names=F)
}

writeAllFiles <- function(tmatrix){
  for(year in 2010:2020){
    writeTrackFile(tmatrix,year)
    print(year)
  }
}


sh2019 %>% removeAnonReleases() %>% attributeSites(sites.basins,2019) %>% group_by(tag_id,state) %>% summarise(date=min(date)) %>% arrange(tag_id,date) %>% setNextState() %>% View()

nodes <- data.frame(node = c(0:5), name = c(	"year1 below","year1 inbasin", "year1 overshoot"),"year2 inbasin","year2 overshoot","year2 below")

sh2019.states <- sh2019 %>% removeAnonReleases() %>% attributeSites(sites.basins,2019) %>% group_by(tag_id,state) %>% summarise(date=min(date)) %>% arrange(tag_id,date) %>% setNextState()

links <- data.frame()

sh2010 <- read.csv("PITdata/Steelhead2010.csv",stringsAsFactors=F) %>% convertRawHistory() %>% fixRecaptureDate() %>% getTracks(tmatrix) %>% select(3,4,13,7,8,6,9,15,14,10,1,2,11) %>% removeAnonReleases() %>% attributeSites(sites.basins,2010)
#all others, do this instead
sh2019 <- read.csv("PITdata/Steelhead2019.csv",stringsAsFactors=F) %>% convertRawHistory() %>% getTracks(tmatrix) %>% select(3,4,13,7,8,6,9,15,14,10,1,2,11) %>% removeAnonReleases() %>% attributeSites(sites.basins,2019)


#create giant collapsed file
createCollapse <- function(){
  df <- data.frame()
  for(year in 2010:2020){
    fname <- paste("PITdata/Steelhead",year,".csv",sep="")
    df1 <- read.csv(fname,stringsAsFactors=F) %>% convertRawHistory() %>% fixRecaptureDate() %>% getTracks(tmatrix) %>% select(-etype,-count,-mindate,-maxdate) %>% removeAnonReleases() %>%  attributeSites(sites.basins,year) 
    df1 <- df1 %>% mutate(day=paste(yearname,yday(date),sep="-")) %>% setNextState() %>% filter(!(tag_id==next_tag & state==next_state & obs_basin==next_basin)) %>% group_by(tag_id,subbasin,subbasin_name,rel_site,rel_date,run,rtype,transported) %>% summarise(day=paste(day,collapse=","),obs_basin=paste(obs_basin=paste(obs_basin,collapse=","))) %>% mutate(return_year=year)
    df <- rbind(df,df1)  
    print(year)
  }

  return(df)
}

bigfile <- createCollapse()
write.csv(bigfile %>% filter(year(rel_date)!=return_year),"shAll_collapsed_2010-2020.csv",row.names=F)

extractRidges <- function(p){
build <- ggplot_build(p)
data <- build$data[[1]]
#get y axis labels
#labels <- build$layout$coord$labels(build$layout$panel_params)[[1]]$y.labels
#get data with groups labeled by y-axis labels
data.extract <- merge(build$data[[1]] %>% select(y,x,density),data.frame(y=seq(1:16),name=build$layout$coord$labels(build$layout$panel_params)[[1]]$y.labels))
return(data.extract)
}

#Dates usually need to be unpacked e.g.
GOJ.data$date <- as_datetime(GOJ.data$x, tz = "UTC")