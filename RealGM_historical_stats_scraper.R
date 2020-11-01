library(httr)
library(XML)

#Code to get high level information for each team
site <- "https://basketball.realgm.com/ncaa/teams"
team.info <- readHTMLTable(rawToChar(GET(site)$content),which=1)

years <- c(2005:2020)

#First we have to get URLs for each team
page.doc <- readLines(url("https://basketball.realgm.com/ncaa/teams"))
page.doc <- page.doc[448]
url.list.index <- unlist(gregexpr("/ncaa/conferences/", page.doc))
url.seq <- seq(from = 1, to = length(url.list.index), by =3)
url.list.index <- url.list.index[url.seq]

#Loop Prep
url.list <- c()

#Loop to get URL list
for (a in 1:length(url.list.index)){
  loop.string <- substr(page.doc, url.list.index[a],url.list.index[a]+1000)
  loop.value <- substr(loop.string,1,unlist(gregexpr("/",loop.string))[7])
  
  #Save URL
  url.list <- c(url.list, loop.value)
  
  print(paste("Done with URL ", match(url.list.index[a], url.list.index),
              " out of ", length(url.list.index), sep=""))
}#End Loop to find team URLs

#Loop to get stats
for (b in 1:length(url.list)){
  for(c in 1:length(years)){
    loop.site <- paste("https://basketball.realgm.com",url.list[b],"/stats/", years[c],
                       "/Averages/All/All/Season/All/points/desc/1/",sep="")
    loop.table <- readHTMLTable(rawToChar(GET(loop.site)$content),which=1)
    loop.table$team <- substr(loop.site, unlist(gregexpr("/", loop.site))[7]+1,
                              unlist(gregexpr("/", loop.site))[8]-1)
    loop.table$year <- years[z]
    
    #Save table
    stats.master <- rbind(stats.master, loop.table)
    
    
    print(paste("Done with year ", years[c],". Team ", match(url.list[b],url.list),
                " out of ", length(url.list),sep=""))
  }#End Years Loop
  
  
  
}#End Team Loop