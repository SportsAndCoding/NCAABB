library(stringr)
library(XML)

setwd("D:/SportsAnalytics/NCAA_Model_Runs")
#date.start = Sys.Date()-1
#date.end = Sys.Date()-1
date.start = as.Date("2019-12-30")
date.end = as.Date("2020-01-02")



#Game Scraper

date.seq = seq(date.start,date.end,by=1)

game.counter = 1
game.table <- data.frame(Totals= character(0), Minutes= numeric(0), FG = numeric(0),x3FG = numeric(0),
                         FT = numeric(0),OR = numeric(0),TR = numeric(0),A = numeric(0),TO = numeric(0),
                         STL = numeric(0),BLK = numeric(0),PF = numeric(0),PTS = numeric(0),Game.ID = character(0))

for (d in 1:length(date.seq)){##----Must find the RealGM Ids for each day------
  index.page.date = paste(format(date.seq[d],format="%Y"),format(date.seq[d],format="%m"),format(date.seq[d],format="%d"),sep="-")
  game.index.page = "https://basketball.realgm.com/ncaa/scores/"
  
  game.index.doc = readLines(url(paste(game.index.page,index.page.date,"/All",sep="")))
  
  boxscore.line.list <- grep(">Box Score<",game.index.doc)
  link.list <- c()
  
  for (b in 1:length(boxscore.line.list)){
    game.index = game.index.doc[boxscore.line.list[b]]
    game.index.link <- substr(game.index,regexpr("=",game.index)+3,regexpr(">Box Score<",game.index)-2)
    link.list <- c(link.list,game.index.link)
    
  } # Link Loop
  
  for (g in 1:length(link.list)){
    game.id <- link.list[g]
    boxscore.index.doc = readLines(url(paste("https://basketball.realgm.com/",game.id,sep="")))
    Top.Team.Match <- grep("stattotals",boxscore.index.doc)[2]
    Top.Team.Name <- substr(boxscore.index.doc[grep("<h2>",boxscore.index.doc)[1]],
                            5,
                            nchar(boxscore.index.doc[grep("<h2>",boxscore.index.doc)[1]])-5)
    Top.Team.FGM <- as.numeric(substr(boxscore.index.doc[Top.Team.Match + 6],
                                      5,
                                      regexpr("-", boxscore.index.doc[Top.Team.Match +6])-1))
    Top.Team.FGA <- as.numeric(substr(boxscore.index.doc[Top.Team.Match + 6],
                                      regexpr("-", boxscore.index.doc[Top.Team.Match +6])+1,
                                      regexpr("</td>", boxscore.index.doc[Top.Team.Match +6])-1))
    Top.Team.X3PM <- as.numeric(substr(boxscore.index.doc[Top.Team.Match + 7],
                                       5,
                                       regexpr("-", boxscore.index.doc[Top.Team.Match +7])-1))
    Top.Team.X3PA <- as.numeric(substr(boxscore.index.doc[Top.Team.Match + 7],
                                       regexpr("-", boxscore.index.doc[Top.Team.Match +7])+1,
                                       regexpr("</td>", boxscore.index.doc[Top.Team.Match +7])-1))
    Top.Team.FTM <- as.numeric(substr(boxscore.index.doc[Top.Team.Match + 8],
                                      5,
                                      regexpr("-", boxscore.index.doc[Top.Team.Match +8])-1))
    Top.Team.FTA <- as.numeric(substr(boxscore.index.doc[Top.Team.Match + 8],
                                      regexpr("-", boxscore.index.doc[Top.Team.Match +8])+1, 
                                      regexpr("</td>", boxscore.index.doc[Top.Team.Match +8])-1))
    Top.Team.OREB <- as.numeric(substr(boxscore.index.doc[Top.Team.Match + 10],
                                       5,
                                       regexpr("</td>", boxscore.index.doc[Top.Team.Match +10])-1))
    Top.Team.TR <- as.numeric(substr(boxscore.index.doc[Top.Team.Match + 12],
                                     5,
                                     regexpr("</td>", boxscore.index.doc[Top.Team.Match +12])-1))
    Top.Team.Ast <- as.numeric(substr(boxscore.index.doc[Top.Team.Match + 13],
                                      5,
                                      regexpr("</td>", boxscore.index.doc[Top.Team.Match +13])-1))
    Top.Team.TO <- as.numeric(substr(boxscore.index.doc[Top.Team.Match + 16],
                                     5,
                                     regexpr("</td>", boxscore.index.doc[Top.Team.Match +16])-1))
    Top.Team.Stl <- as.numeric(substr(boxscore.index.doc[Top.Team.Match + 15],
                                      5,
                                      regexpr("</td>", boxscore.index.doc[Top.Team.Match +15])-1))
    Top.Team.Blk <- as.numeric(substr(boxscore.index.doc[Top.Team.Match + 17],
                                      5,
                                      regexpr("</td>", boxscore.index.doc[Top.Team.Match +17])-1))
    Top.Team.PF <- as.numeric(substr(boxscore.index.doc[Top.Team.Match + 14],
                                     5,      
                                     regexpr("</td>", boxscore.index.doc[Top.Team.Match +14])-1))
    Top.Team.Pts <- as.numeric(substr(boxscore.index.doc[Top.Team.Match + 18],
                                      5,  
                                      regexpr("</td>", boxscore.index.doc[Top.Team.Match +18])-1))       
    Bottom.Team.Match <- grep("stattotals",boxscore.index.doc)[5]
    
    Bottom.Team.Name <- substr(boxscore.index.doc[grep("<h2>",boxscore.index.doc)[2]],
                               5,
                               nchar(boxscore.index.doc[grep("<h2>",boxscore.index.doc)[2]])-5)
    Bottom.Team.FGM <- as.numeric(substr(boxscore.index.doc[Bottom.Team.Match + 6],
                                         5,
                                         regexpr("-", boxscore.index.doc[Bottom.Team.Match +6])-1))
    Bottom.Team.FGA <- as.numeric(substr(boxscore.index.doc[Bottom.Team.Match + 6],
                                         regexpr("-", boxscore.index.doc[Bottom.Team.Match +6])+1,
                                         regexpr("</td>", boxscore.index.doc[Bottom.Team.Match +6])-1))
    Bottom.Team.X3PM <- as.numeric(substr(boxscore.index.doc[Bottom.Team.Match + 7],
                                          5,
                                          regexpr("-", boxscore.index.doc[Bottom.Team.Match +7])-1))
    Bottom.Team.X3PA <- as.numeric(substr(boxscore.index.doc[Bottom.Team.Match + 7],
                                          regexpr("-", boxscore.index.doc[Bottom.Team.Match +7])+1,
                                          regexpr("</td>", boxscore.index.doc[Bottom.Team.Match +7])-1))
    Bottom.Team.FTM <- as.numeric(substr(boxscore.index.doc[Bottom.Team.Match + 8],
                                         5,
                                         regexpr("-", boxscore.index.doc[Bottom.Team.Match +8])-1))
    Bottom.Team.FTA <- as.numeric(substr(boxscore.index.doc[Bottom.Team.Match + 8],
                                         regexpr("-", boxscore.index.doc[Bottom.Team.Match +8])+1, 
                                         regexpr("</td>", boxscore.index.doc[Bottom.Team.Match +8])-1))
    Bottom.Team.OREB <- as.numeric(substr(boxscore.index.doc[Bottom.Team.Match + 10],
                                          5,
                                          regexpr("</td>", boxscore.index.doc[Bottom.Team.Match +10])-1))
    Bottom.Team.TR <- as.numeric(substr(boxscore.index.doc[Bottom.Team.Match + 12],
                                        5,
                                        regexpr("</td>", boxscore.index.doc[Bottom.Team.Match +12])-1))
    Bottom.Team.Ast <- as.numeric(substr(boxscore.index.doc[Bottom.Team.Match + 13],
                                         5,
                                         regexpr("</td>", boxscore.index.doc[Bottom.Team.Match +13])-1))
    Bottom.Team.TO <- as.numeric(substr(boxscore.index.doc[Bottom.Team.Match + 16],
                                        5,
                                        regexpr("</td>", boxscore.index.doc[Bottom.Team.Match +16])-1))
    Bottom.Team.Stl <- as.numeric(substr(boxscore.index.doc[Bottom.Team.Match + 15],
                                         5,
                                         regexpr("</td>", boxscore.index.doc[Bottom.Team.Match +15])-1))
    Bottom.Team.Blk <- as.numeric(substr(boxscore.index.doc[Bottom.Team.Match + 17],
                                         5,
                                         regexpr("</td>", boxscore.index.doc[Bottom.Team.Match +17])-1))
    Bottom.Team.PF <- as.numeric(substr(boxscore.index.doc[Bottom.Team.Match + 14],
                                        5,      
                                        regexpr("</td>", boxscore.index.doc[Bottom.Team.Match +14])-1))
    Bottom.Team.Pts <- as.numeric(substr(boxscore.index.doc[Bottom.Team.Match + 18],
                                         5,  
                                         regexpr("</td>", boxscore.index.doc[Bottom.Team.Match +18])-1))  
    
    #Build out game.df  
    Top.Team.df <- as.data.frame(cbind(Top.Team.Name,Top.Team.FGM,Top.Team.FGA,Top.Team.X3PM,Top.Team.X3PA,Top.Team.FTM,Top.Team.FTA,
                                       Top.Team.OREB,Top.Team.TR,Top.Team.Ast,Top.Team.TO,Top.Team.Stl,Top.Team.Blk,
                                       Top.Team.PF,Top.Team.Pts))
    Bottom.Team.df <- as.data.frame(cbind(Bottom.Team.Name,Bottom.Team.FGM,Bottom.Team.FGA,Bottom.Team.X3PM,Bottom.Team.X3PA,Bottom.Team.FTM,Bottom.Team.FTA,
                                          Bottom.Team.OREB,Bottom.Team.TR,Bottom.Team.Ast,Bottom.Team.TO,Bottom.Team.Stl,Bottom.Team.Blk,
                                          Bottom.Team.PF,Bottom.Team.Pts))
    colnames(Top.Team.df)[1:15] <- c("Team","FGA","FGM","x3PM","x3PA","FTM","FTA","OR","TR","Ast","TO","Stl","Blk","PF","Pts")
    colnames(Bottom.Team.df)[1:15] <- c("Team","FGA","FGM","x3PM","x3PA","FTM","FTA","OR","TR","Ast","TO","Stl","Blk","PF","Pts")
    Game.df <- rbind(Top.Team.df,Bottom.Team.df)
    game.table <- rbind(game.table,Game.df)
    
    
    
    print(paste("Done with game ",match(link.list[g],link.list)," of ",length(link.list),
                ". Current day ",match(date.seq[d],date.seq)," of ",length(date.seq),sep=" "))
  }#Game Scraper Loop
  
  print(paste("Done with day ",match(date.seq[d],date.seq)," of ",length(date.seq),sep=""))
  
} #End Full Loop