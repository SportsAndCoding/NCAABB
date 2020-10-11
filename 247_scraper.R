setwd("D:/SportsAnalytics/NCAA_Prep")

#Loops to find team names

#Read in HTML code to R Studio
index.page <- readLines(url(paste("https://247sports.com/League/NCAA-BK/Teams/",sep="")))
index.line <- index.page[71]

html.triggers <- unlist(gregexpr("https://247sports.com/college/",index.line))

#Loop to find team names
#Prep
team.names <- c()

for (z in 1:length(html.triggers)){
  loop.string <- substr(index.line, html.triggers[z],html.triggers[z]+500)
  loop.value <- substr(loop.string, nchar("https://247sports.com/college/")+1,
                                          regexpr(">",loop.string)-1)
  
  team.names <- c(team.names,loop.value)
  
}#End Team Name Loop

#Clean up list
delete.match <- unlist(gregexpr("Team",team.names))
delete.match <- which(delete.match <0)
team.names.clean <- team.names[delete.match]

#Round 2 of cleaning
delete.match2 <- unlist(gregexpr("recruiting",team.names.clean))
delete.match2 <- which(delete.match2 <0)
team.names.clean2 <- team.names.clean[delete.match2]

#Remove unwanted characters
team.names.final <- gsub("/","",team.names.clean2)


#Export team names for master team lookup
write.csv(team.names.final, file="247_team_names.csv")

#Build historical recruiting dataset
#Years Loop Prep
years <- seq(2019,2010, by=-1)

#Main Master Team Loop Prep
Nat.Rank <- c()
Conf.Rank <- c()
Avg.Rating <- c()
Num.LOT <- c()
year <- c()
team <- c()
Num.Enrollees <- c()
Num.Transfers <- c()
Num.LOT <- c()
Num.Tot.Signees <- c()


#SubLoop Prep
player.name <- c()
player.rating <- c()
player.pos <- c()
player.ht <- c()
player.wt <- c()
player.nat.rank <- c()
player.pos.rank <- c()
player.state.rank <- c()
player.year <- c()
player.team <- c()

for (y in 1:length(years)){
  for (a in 1:length(team.names.final)){
    html.page <- readLines(url(paste("https://247sports.com/college/",
                                     substr(team.names.final[a],1,nchar(team.names.final[a])-1),
                                     "/Season/",
                                     years[y],
                                     "-Basketball/Commits/",sep="")))
    html.page <- tolower(html.page)
    
    #Team-Wide Metrics
    html.index <- grep("circle-image-block",html.page)
    if(length(html.index)>0){
      html.nat.rank <- html.page[html.index]
      nat.rank.string <- substr(html.nat.rank, regexpr("national rank",html.nat.rank),
                                regexpr("national rank",html.nat.rank)+5000)
      nat.rank.lv <- as.numeric(substr(nat.rank.string, unlist(gregexpr(">",nat.rank.string))[2]+1,
                                       unlist(gregexpr("<",nat.rank.string))[3]-1))
      conf.rank.lv <- as.numeric(substr(nat.rank.string, unlist(gregexpr(">",nat.rank.string))[14]+1,
                                        unlist(gregexpr("<",nat.rank.string))[15]-1))
      avg.rating.lv <- as.numeric(substr(nat.rank.string, unlist(gregexpr(">",nat.rank.string))[26]+1,
                                         unlist(gregexpr("<",nat.rank.string))[27]-1))
      enrollees.string <- substr(nat.rank.string, regexpr("enrollees",nat.rank.string),
                                 regexpr("enrollees",nat.rank.string)+250)
      num.enrollees.lv <- as.numeric(substr(enrollees.string,regexpr(" ",enrollees.string)+2,
                                            regexpr("<",enrollees.string)-2))
      transfers.string <- substr(html.nat.rank, regexpr("transfers",html.nat.rank),
                                 regexpr("transfers",html.nat.rank)+250)
      num.transfers.lv <- as.numeric(substr(transfers.string, regexpr(" ",transfers.string)+2,
                                            regexpr("<",transfers.string)-2))
      lot.string <- substr(html.nat.rank, regexpr("signed letter of intent", html.nat.rank),
                                          regexpr("signed letter of intent", html.nat.rank)+250)
      num.lot.lv <- as.numeric(substr(lot.string, unlist(gregexpr(" ", lot.string))[4]+2,
                                          regexpr("<", lot.string)-2))
      Num.signings1 <- which(!is.na(c(num.enrollees.lv, num.transfers.lv, num.lot.lv)))
      Num.signings.lv <- sum(Num.signings1)
      
      #Save values
      Nat.Rank <- c(Nat.Rank, nat.rank.lv)
      Conf.Rank <- c(Conf.Rank, conf.rank.lv)
      Avg.Rating <- c(Avg.Rating, avg.rating.lv)
      year <- c(year,years[y])
      team <- c(team, team.names.final[a])
      Num.Enrollees <- c(Num.Enrollees, num.enrollees.lv)
      Num.Transfers <- c(Num.Transfers, num.transfers.lv)
      Num.LOT <- c(Num.LOT, num.lot.lv)
      Num.Tot.Signees <- c(Num.Tot.Signees, Num.signings.lv)
      
      
      #Subloop
      #Loop to find the players that committed to the school
      new.player.indices <- unlist(gregexpr("circle-image-block",html.nat.rank))
      end.player.indices <- c(new.player.indices[2:length(new.player.indices)],nchar(html.nat.rank))
      
      if(!is.na(Num.signings)){
        for (w in 1:length(new.player.indices)){
          
          subloop.player.string <- substr(html.nat.rank, new.player.indices[w], end.player.indices[w])
          subloop.PN <- substr(subloop.player.string,
                               regexpr("=",subloop.player.string)+2,
                               regexpr("class",subloop.player.string)-3)
          subloop.PRating <- as.numeric(substr(subloop.player.string,
                                               regexpr(">0.",subloop.player.string)+1,
                                               regexpr(">0.",subloop.player.string)+6))
          subloop.PPos <- substr(subloop.player.string,
                                 regexpr("position=",subloop.player.string)+9,
                                 regexpr("position=",subloop.player.string)+10)
          subloop.PHT.string <- substr(subloop.player.string,
                                       regexpr("metrics",subloop.player.string)+10,
                                       regexpr("metrics",subloop.player.string)+50)
          subloop.PHT <- substr(subloop.PHT.string,1,
                                regexpr(" ",subloop.PHT.string)-1)
          subloop.PWT <- as.numeric(substr(subloop.PHT.string,
                                           regexpr(" ",subloop.PHT.string)+3,
                                           regexpr(" ", subloop.PHT.string)+5))
          subloop.PNatRank.string <- substr(subloop.player.string, 
                                    unlist(gregexpr("highschool", subloop.player.string))[1],
                                    unlist(gregexpr("highschool", subloop.player.string))[1]+250)
          subloop.PNatRank <- as.numeric(substr(subloop.PNatRank.string, 
                                     regexpr(">", subloop.PNatRank.string)+1,
                                     regexpr("<", subloop.PNatRank.string)-1))
          subloop.PPosRank.string <- substr(subloop.player.string, 
                                    unlist(gregexpr("highschool", subloop.player.string))[2],
                                    unlist(gregexpr("highschool", subloop.player.string))[2]+250)
          subloop.PPosRank <- as.numeric(substr(subloop.PPosRank.string,
                                     regexpr(">",subloop.PPosRank.string)+1,
                                      regexpr("<",subloop.PPosRank.string)-1))
          subloop.PStateRank.string <- substr(subloop.player.string, 
                                      unlist(gregexpr("highschool", subloop.player.string))[3],
                                      unlist(gregexpr("highschool", subloop.player.string))[3]+250)
          subloop.PStateRank <- as.numeric(substr(subloop.PStateRank.string,
                                                  regexpr(">",subloop.PStateRank.string)+1,
                                                  regexpr("<",subloop.PStateRank.string)-1))
          
          #Save values
          player.name <- c(player.name,subloop.PN )
          player.rating <- c(player.rating,subloop.PRating)
          player.pos <- c(player.pos,subloop.PPos)
          player.ht <- c(player.ht,subloop.PHT)
          player.wt <- c(player.wt,subloop.PWT)
          player.nat.rank <- c(player.nat.rank,subloop.PNatRank)
          player.pos.rank <- c(player.pos.rank,subloop.PPosRank)
          player.state.rank <- c(player.state.rank,subloop.PStateRank)
          player.year <- c(player.year,years[y])
          player.team <- c(player.team,team.names.final[a])
          
          
          
          
        }#End if True
      }  
      
      
    }
    
    print(paste("Done with team ",team.names.final[a],match(team.names.final[a],team.names.final),
                " out of ",length(team.names.final),". Year=",years[y],sep=""))
    
    
  }#End Master Team Loop
  
}#End Year Loop


#Clean up loop outputs
#Player DF
player.df <- as.data.frame(cbind(player.name, player.rating))
player.df$player.pos <- player.pos#Player's position
player.df$player.ht <- player.ht#Player's Height
player.df$player.wt <- player.wt#Player's Weight
player.df$player.nat.rank <- player.nat.rank#Player's national recrutiting rank
player.df$player.pos.rank <- player.pos.rank#Player's national position recruiting rank
player.df$player.state.rank <- player.state.rank#Player's state recruiting rank
player.df$player.year <- player.year#Year player signed
player.df$player.team <- player.team#Player signed with what team


#Team DF
team.df <- as.data.frame(cbind(Nat.Rank,Conf.Rank))
team.df$Avg.Rating <- Avg.Rating
team.df$Year <- year 
team.df$Team <- team
team.df$Num.Enrollees <- Num.Enrollees
team.df$Num.Transfers <- Num.Transfers
team.df$Num.LOT <- Num.LOT
team.df$Num.Tot.Signees <- Num.Tot.Signees

#Export to csv
write.csv(player.df, file="247_player.df.csv")
write.csv(team.df, file="247_team.df.csv")











