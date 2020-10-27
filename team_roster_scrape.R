library(stringr)
library(XLConnectJars)
library(XLConnect)
library(XML)


#Initial loop to find team IDs since cleaned after an export to csv
#Loop to find team IDs on espn
game.index.doc <- readLines(url("http://www.espn.com/mens-college-basketball/standings"))
game.index.doc <- game.index.doc[65]
id.spots <- unlist(gregexpr("/id/",game.index.doc))
ph <- c()
ph.2 <- c()



for (i in 1:length(id.spots)){
  id.string <- substr(game.index.doc,unlist(id.spots)[i],unlist(id.spots)[i]+250)
  id.final <- as.numeric(substr(id.string,unlist(gregexpr("/",id.string))[2]+1,unlist(gregexpr("/",id.string))[3]-1))
  id.team.pair <- substr(id.string,regexpr(">",id.string)+1,regexpr("<",id.string)-1)
  ph <- c(ph,id.final)
  ph.2 <- c(ph.2,id.team.pair)
  
  
} # Link Loop

id.table <- read.csv("espn_ids.csv",stringsAsFactors = FALSE)
id.list <- unique(id.table$ID)

#Loop Prep
player.name <- c()
player.pos <- c()
player.ht <- c()
player.wt <- c()
player.class <- c()
team <- c()




for (i in 1:length(id.list)){##----grabbing season player stats for each team------
  tryCatch({
    
    index.page.link.main <- paste("http://www.espn.com/mens-college-basketball/team/roster/_/id/",id.list[i],sep="")
    
    loop.page <- readLines(url(index.page.link.main))
    loop.page.index <- grep("recordSummary", loop.page)
    loop.string <- substr(loop.page[loop.page.index],
                          regexpr("recordSummary",loop.page[loop.page.index]),
                          nchar(loop.page[loop.page.index]))  
    
    player.list <- unlist(gregexpr("/player/", loop.string))
    
    
    for(z in 1:length(player.list)){
      subloop.string <- substr(loop.string, player.list[z], nchar(loop.string))
      name.lv <- substr(subloop.string, regexpr("lastName", subloop.string)+11,
                        regexpr("experience", subloop.string)-4)
      pos.lv <- substr(subloop.string, regexpr("position", subloop.string)+11,
                       regexpr("position", subloop.string)+11)
      ht.lv <- substr(subloop.string, regexpr("height", subloop.string)+9,
                      regexpr("weight", subloop.string)-6)
      wt.lv <- as.numeric(substr(subloop.string, regexpr("weight", subloop.string)+9,
                                 regexpr("lbs", subloop.string)-2))
      class.lv <- substr(subloop.string, regexpr("experience", subloop.string)+13,
                         regexpr("birthPlace", subloop.string)-4)
      
      
      #Save Values out of loop
      player.name <- c(player.name, name.lv)
      player.pos <- c(player.pos, pos.lv)
      player.ht <- c(player.ht, ht.lv)
      player.wt <- c(player.wt, wt.lv)
      player.class <- c(player.class, class.lv)
      team <- c(team, id.list[i])
      
    }#End Player subloop
    
    print(paste("Done with Team ",match(id.list[i],id.list)," of ",length(id.list)))
  })
}#Full Loop

team.roster.df <- as.data.frame(cbind(team, player.name))
team.roster.df$player.pos <- player.pos
team.roster.df$player.ht <- player.ht
team.roster.df$player.wt <- player.wt
team.roster.df$player.class <- player.class
team.roster.df$player.class <- substr(team.roster.df$player.class,1,2)