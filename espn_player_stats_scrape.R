#USE id.list to find historical team statistics
#Prep
years <- c(2005:2020) 
#save values
master.df <- stats.df[FALSE,]


for (y in 1:length(years)){
  loop.year <- years[y]
  
  #Subloop to iterate over teams in each year
  for(a in 1:length(id.list)){
    stats.page <- paste("http://www.espn.com/mens-college-basketball/team/stats/_/id/",
                        id.list[a],"/season/",years[y],sep="")
    
    loop.stats.page <- readLines(url(stats.page))
    loop.stats.index <- grep("playerStats", loop.stats.page)
    
    names.df <-readHTMLTable(rawToChar(GET(stats.page)$content),which=3)
    stats.df <-readHTMLTable(rawToChar(GET(stats.page)$content),which=4)
    stats.df$names <- names.df$Name
    stats.df$year <- years[y]
    stats.df <- stats.df[-nrow(stats.df),]
    
    #Save outside of loop
    master.df <- rbind(master.df, stats.df)
    
    
    print(paste("Done with team ", match(id.list[a], id.list), " out of ",
                length(id.list), "Year-", years[y], sep="-"))  
  }#End team subloop
  
  
  
}#End Year Loop