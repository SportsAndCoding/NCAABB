library(stringr)
library(XML)


######################Module to generate daily game list and betting information###############
###############################################################################################
###############################################################################################
#By Hand
#daily.games <- read.csv("today_schedule.csv")
#If doing by hand, be sure to load in TeamLookups before blending module

#Scraper
game.index.doc = readLines(url(paste("http://www.oddsshark.com/ncaab/odds",sep="")))
#Info Module
game.index.doc.clean <- gsub("&#43;","",game.index.doc[15])
info.l <- gregexpr("data-op-info=",game.index.doc.clean)
info.list <- unlist(info.l)
info.list.loop <- c()

for (a in 1:length(info.list)){
  info.string <- substr(game.index.doc.clean, info.list[a]+13, info.list[a]+33)
  Info <- substr(info.string, regexpr(":",info.string)+2, regexpr(",",info.string)-2)
  info.list.loop <- c(info.list.loop,Info)
  
  print(paste("Done with retrieving info for pattern ",info.list[a]),sep=" ")
  
} # Info Loop

#Total module
total.l <- gregexpr("data-op-total=",game.index.doc.clean)
total.list <- unlist(total.l)
total.list.loop <- c()

for (b in 1:length(total.list)){
  total.string <- substr(game.index.doc.clean, total.list[b]+14, total.list[b]+34)
  Total <- substr(total.string, regexpr(":",total.string)+2, regexpr(",",total.string)-2)
  total.list.loop <- c(total.list.loop,Total)
  
  print(paste("Done with retrieving total for pattern ",total.list[b]),sep=" ")
  
} # Total Loop

#Moneyline module
moneyline.l <- gregexpr("data-op-moneyline=",game.index.doc.clean)
moneyline.list <- unlist(moneyline.l)
moneyline.list.loop <- c()

for (c in 1:length(moneyline.list)){
  moneyline.string <- substr(game.index.doc.clean, moneyline.list[c]+18, moneyline.list[c]+38)
  Moneyline <- substr(moneyline.string, regexpr(":",moneyline.string)+2, regexpr(",",moneyline.string)-2)
  moneyline.list.loop <- c(moneyline.list.loop,Moneyline)
  
  print(paste("Done with retrieving pattern for moneyline ",moneyline.list[c]),sep=" ")
  
} # Moneyline Loop

#Top Team Module
top.team.l <- gregexpr("op-matchup-team op-matchup-text op-team-top",game.index.doc.clean)
top.team.list <- unlist(top.team.l)
top.team.list.loop <- c()

for (d in 1:length(top.team.list)){
  top.team.string <- substr(game.index.doc.clean, top.team.list[d]+43, top.team.list[d]+150)
  Top.Team <- substr(top.team.string, regexpr(":",top.team.string)+2, regexpr(",",top.team.string)-2)
  top.team.list.loop <- c(top.team.list.loop,Top.Team)
  
  print(paste("Done with retrieving pattern for Top Team", top.team.list[d]),sep=" ")
  
} # Top Team Loop

#Bottom Team Module
bottom.team.l <- gregexpr("op-matchup-team op-matchup-text op-team-bottom",game.index.doc.clean)
bottom.team.list <- unlist(bottom.team.l)
bottom.team.list.loop <- c()

for (e in 1:length(bottom.team.list)){
  bottom.team.string <- substr(game.index.doc.clean, bottom.team.list[e]+46, bottom.team.list[e]+150)
  Bottom.Team <- substr(bottom.team.string, regexpr(":",bottom.team.string)+2, regexpr(",",bottom.team.string)-2)
  bottom.team.list.loop <- c(bottom.team.list.loop,Bottom.Team)
  
  print(paste("Done with retrieving pattern for Bottom Team", bottom.team.list[e]),sep=" ")
  
} # Bottom Team Loop

#Create sublists to break out patterns
newgame <- seq(1,length(info.list.loop),by=40)
fd.newgame <- newgame+32
fd.top.spread.price <- fd.newgame+1
fd.bottom.spread <- fd.newgame+2
fd.bottom.spread.price <- fd.newgame+3
Tot.newgame <- seq(1,length(total.list.loop), by=20)
fdTot.newgame <- Tot.newgame + 17
ML.newgame <- seq(1,length(moneyline.list.loop), by=20)
fdML.newgame <- ML.newgame + 16
fdML.newgame.bottom <- ML.newgame + 17

#Create placeholders for loops
top.spread <- c()
bottom.spread <- c()
top.spread.price <- c()
bottom.spread.price <- c()
game.total <- c()
ML.top <- c()
ML.bottom <- c()


#Bottom Spread Loop
for (f in 1:length(fd.bottom.spread)){
  bottom.spread <- c(bottom.spread,info.list.loop[fd.bottom.spread[f]])
} # Bottom Spread Loop

#top spread price
for (g in 1:length(fd.top.spread.price)){
  top.spread.price <- c(top.spread.price,info.list.loop[fd.top.spread.price[g]])
  
} # Top Spread Price Loop

#bottom spread price
for (h in 1:length(fd.bottom.spread.price)){
  bottom.spread.price <- c(bottom.spread.price,info.list.loop[fd.bottom.spread.price[h]])
  
} # Bottom Spread Price Loop


#game total loop
for (i in 1:length(fdTot.newgame)){
  game.total <- c(game.total,total.list.loop[fdTot.newgame[i]])
  
} # Game Total Loop

#Moneyline Top Price loop
for (j in 1:length(fdML.newgame)){
  ML.top <- c(ML.top,moneyline.list.loop[fdML.newgame[j]])
  
} # Moneyline Top Price loop

#Moneyline Bottom Price loop
for (k in 1:length(fdML.newgame.bottom)){
  ML.bottom <- c(ML.bottom,moneyline.list.loop[fdML.newgame.bottom[k]])
  
} # Moneyline Bottom Price loop

#Create dataframe with today's schedule
daily.games <- as.data.frame(cbind(top.team.list.loop,bottom.team.list.loop,bottom.spread,top.spread.price,bottom.spread.price,game.total,ML.top,ML.bottom))

#Clean up dataframe to bring into simulator
write.csv(daily.games, file="daily.games_preclean.csv")
daily.games <- read.csv("daily.games_preclean.csv",stringsAsFactors = FALSE)
daily.games <- daily.games[,-c(1)]
daily.games$ML.bottom <- daily.games$ML.bottom / 1
daily.games$ML.bottom[is.na(daily.games$ML.bottom)] <- -20000
daily.games$game.total <- daily.games$game.total / 1
daily.games$game.total[is.na(daily.games$game.total)] <- 200
team.lookup <- read.csv("TeamLookups.csv",stringsAsFactors = FALSE)
team.lookup$My.Name<- tolower(team.lookup$My.Name)
team.lookup <- team.lookup[,-c(2,4,5)]
colnames(team.lookup)[2] <- "Team"
colnames(daily.games)[1] <- "Team"
daily.games <- merge(daily.games, team.lookup, by="Team", all.x = TRUE)
daily.games <- daily.games[,c(9,2,3,4,5,6,7,8,1)]
daily.games <- daily.games[,-c(9)]
colnames(daily.games)[1] <- "Team1"
colnames(team.lookup)[2] <- "bottom.team.list.loop"
daily.games <- merge(daily.games, team.lookup, by="bottom.team.list.loop", all.x = TRUE)
daily.games <- daily.games[,c(2,9,3,4,5,6,7,8,1)]
daily.games <- daily.games[,-c(9)]
colnames(daily.games)[2] <- "Team2"
daily.games$bottom.spread <- as.numeric(daily.games$bottom.spread) / -1
colnames(daily.games)[3] <- "Spread"
daily.games <- daily.games[,c(1,2,3,6,7,8,4,5)]
colnames(daily.games)[4] <- "Total"
colnames(daily.games)[5] <- "AW"
colnames(daily.games)[6] <- "HW"
colnames(daily.games)[7] <- "ac"
colnames(daily.games)[8] <- "hc"
daily.games$over <- .909
daily.games$under <- .909
daily.games$ac <- .909
daily.games$hc <- .909
#daily.games$AW[is.na(daily.games$AW)] <- -20000
daily.games <- daily.games[complete.cases(daily.games),]