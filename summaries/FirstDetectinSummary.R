setFirstDetections <- function(df,year,sites=c("GRJ","GOJ","LMJ","ICHFFB","MCJ","JDJ","BCC","B2J")){
  df <- filter(df,year(date)==year) %>% merge(data.frame(obs_site=sites))
  df1 <- group_by(df,tag_id) %>% summarise(date=min(date))
  df2 <- merge(df,df1)%>% arrange(date) %>% select(tag_id,date)
  df2$cum <- seq(1:nrow(df2))
  df2$first <- TRUE
  df3 <- merge(df,df2,all.x=T)
  df3$first <- ifelse(is.na(df3$first),FALSE,TRUE)
  df3$obs_site <- factor(df3$obs_site,levels=sites)
  return(df3 %>% arrange(tag_id,date))
}

getFirstDetections <- function(df,year){
  df.first <- setFirstDetections(df,year) %>% filter(first==TRUE)
  return(df.first)
}

getYearSummary <- function(year){
#loads a "reduced" data file for each species from a set file structure and summarizes
  ch <- loadReduced("chinook",year)
  ch.first <- getFirstDetections(ch,year)
  ch.first$rel_year <- year(ch.first$rel_date)
  ch.first$date<- floor_date(ch.first$date,unit="day")
  ch.first$species <- "chinook"
  st <- loadReduced("steelhead",year)
  st.first <- getFirstDetections(st,year)
  st.first$rel_year <- year(st.first$rel_date)
  st.first$date<- floor_date(st.first$date,unit="day")  
  st.first$species <- "steelhead"
  so <- loadReduced("sockeye",year)
  so.first <- getFirstDetections(so,year)
  so.first$rel_year <- year(so.first$rel_date)
  so.first$date<- floor_date(so.first$date,unit="day")
  so.first$species <- "sockeye"
  df <- rbind(ch.first,st.first,so.first)
  df <- filter(df,subbasin_name != "Lower Snake",subbasin_name != "Lower Snake-Tucannon")
  df$obs_year <- year
  #year(df$date) <- 2018
  return(df %>% select(species,rtype,run,subbasin_name,rel_site,rel_year,obs_site,obs_year,date))
}

getCombinedSummaries <- function(years){
#creates a single summary file for a vector of years
  df <- data.frame()
  for(i in 1:length(years)){
    df <- rbind(df,getYearSummary(years[i]) %>% mutate(day=yday(date))) 
  }
  df.agg <- df %>% group_by(species,rtype,run,subbasin_name,rel_year,rel_site,obs_site,obs_year,day) %>% summarise(count=n())
  return(df.agg)
}
