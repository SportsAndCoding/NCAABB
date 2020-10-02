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

write.csv(team.names.final, file="247_team_names.csv")




