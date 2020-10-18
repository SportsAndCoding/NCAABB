#Set WD
Off <- read.csv("Adj_Skills_Off.csv", stringsAsFactors = FALSE)
Def <- read.csv("Adj_Skills_Def.csv", stringsAsFactors = FALSE)
Off <- Off[,-1]
Def <- Def[,-1]
Off$Team.Year <- paste(Off$Team, Off$Year, sep="-")
Def$Team.Year <- paste(Def$Team, Def$Year, sep="-")

#Loop Prep
IVs <- colnames(Off)[2:8]
years <- seq(2019,2007, by=-1)
teams <- unique(Off$Team)
#Buckets
TOPM1 <- c()
TOPM2 <- c()
TOPM3 <- c()
TOPM4 <- c()
TOPM1D <- c()
TOPM2D <- c()
TOPM3D <- c()
TOPM4D <- c()
FTPM1 <- c()
FTPM2 <- c()
FTPM3 <- c()
FTPM4 <- c()
FTPM1D <- c()
FTPM2D <- c()
FTPM3D <- c()
FTPM4D <- c()
FTRATM1 <- c()
FTRATM2 <- c()
FTRATM3 <- c()
FTRATM4 <- c()
FTRATM1D <- c()
FTRATM2D <- c()
FTRATM3D <- c()
FTRATM4D <- c()
OREBPM1 <- c()
OREBPM2 <- c()
OREBPM3 <- c()
OREBPM4 <- c()
OREBPM1D <- c()
OREBPM2D <- c()
OREBPM3D <- c()
OREBPM4D <- c()
X2PTPM1 <- c()
X2PTPM2 <- c()
X2PTPM3 <- c()
X2PTPM4 <- c()
X2PTPM1D <- c()
X2PTPM2D <- c()
X2PTPM3D <- c()
X2PTPM4D <- c()
X3PTPM1 <- c()
X3PTPM2 <- c()
X3PTPM3 <- c()
X3PTPM4 <- c()
X3PTPM1D <- c()
X3PTPM2D <- c()
X3PTPM3D <- c()
X3PTPM4D <- c()
X3PTRATM1 <- c()
X3PTRATM2 <- c()
X3PTRATM3 <- c()
X3PTRATM4 <- c()
X3PTRATM1D <- c()
X3PTRATM2D <- c()
X3PTRATM3D <- c()
X3PTRATM4D <- c()
team.ph.top <- c()
team.ph.ftp <- c()
team.ph.ftrat <- c()
team.ph.orebp <- c()
team.ph.x2ptp <- c()
team.ph.x3ptp <- c()
team.ph.x3ptrat <- c()
team.ph.topdef <- c()
team.ph.ftpdef <- c()
team.ph.ftratdef <- c()
team.ph.orebpdef <- c()
team.ph.x2ptpdef <- c()
team.ph.x3ptpdef <- c()
team.ph.x3ptratdef <- c()
year.ph.top <- c()
year.ph.ftp <- c()
year.ph.ftrat <- c()
year.ph.orebp <- c()
year.ph.x2ptp <- c()
year.ph.x3ptp <- c()
year.ph.x3ptrat <- c()
year.ph.topdef <- c()
year.ph.ftpdef <- c()
year.ph.ftratdef <- c()
year.ph.orebpdef <- c()
year.ph.x2ptpdef <- c()
year.ph.x3ptpdef <- c()
year.ph.x3ptratdef <- c()

for (z in 1:length(teams)){
  loop.df <- Off[Off$Team == teams[z],]
  loop.def.df <- Def[Def$Team == teams[z],]
  
  for (y in 1:length(years)){
    loop.year <- years[y]
    loop.year.m1 <- match(years[y] -1, loop.df$Year)
    loop.year.m2 <- match(years[y] -2, loop.df$Year)
    loop.year.m3 <- match(years[y] -3, loop.df$Year)
    loop.year.m4 <- match(years[y] -4, loop.df$Year)
    
    for (x in 1:length(IVs)){
      if(IVs[x] == IVs[1]){
        TOP.m1 <- loop.df$TOP_Adj[loop.year.m1]
        TOP.m2 <- loop.df$TOP_Adj[loop.year.m2]
        TOP.m3 <- loop.df$TOP_Adj[loop.year.m3]
        TOP.m4 <- loop.df$TOP_Adj[loop.year.m4]
        TOP.def.m1 <- loop.def.df$TOP_Adj[loop.year.m1]
        TOP.def.m2 <- loop.def.df$TOP_Adj[loop.year.m2]
        TOP.def.m3 <- loop.def.df$TOP_Adj[loop.year.m3]
        TOP.def.m4 <- loop.def.df$TOP_Adj[loop.year.m4]
        
        #Save Values
        TOPM1 <- c(TOPM1,TOP.m1)
        TOPM2 <- c(TOPM2,TOP.m2)
        TOPM3 <- c(TOPM3,TOP.m3)
        TOPM4 <- c(TOPM4,TOP.m4)
        TOPM1D <- c(TOPM1D,TOP.def.m1)
        TOPM2D <- c(TOPM2D,TOP.def.m2)
        TOPM3D <- c(TOPM3D,TOP.def.m3)
        TOPM4D <- c(TOPM4D,TOP.def.m4)
        team.ph.top <- c(team.ph.top, teams[z])
        team.ph.topdef <- c(team.ph.topdef, teams[z])
        year.ph.top <- c(year.ph.top, years[y])
        year.ph.topdef <- c(year.ph.topdef, years[y])
      }else if(IVs[x] == IVs[2]){
        FTP.m1 <- loop.df$FTP_Adj[loop.year.m1]
        FTP.m2 <- loop.df$FTP_Adj[loop.year.m2]
        FTP.m3 <- loop.df$FTP_Adj[loop.year.m3]
        FTP.m4 <- loop.df$FTP_Adj[loop.year.m4]
        FTP.def.m1 <- loop.def.df$FTP_Adj[loop.year.m1]
        FTP.def.m2 <- loop.def.df$FTP_Adj[loop.year.m2]
        FTP.def.m3 <- loop.def.df$FTP_Adj[loop.year.m3]
        FTP.def.m4 <- loop.def.df$FTP_Adj[loop.year.m4]
        
        #Save Values
        FTPM1 <- c(FTPM1,FTP.m1)
        FTPM2 <- c(FTPM2,FTP.m2)
        FTPM3 <- c(FTPM3,FTP.m3)
        FTPM4 <- c(FTPM4,FTP.m4)
        FTPM1D <- c(FTPM1D,FTP.def.m1)
        FTPM2D <- c(FTPM2D,FTP.def.m2)
        FTPM3D <- c(FTPM3D,FTP.def.m3)
        FTPM4D <- c(FTPM4D,FTP.def.m4)
        team.ph.ftp <- c(team.ph.ftp, teams[z])
        team.ph.ftpdef <- c(team.ph.ftpdef, teams[z])
        year.ph.ftp <- c(year.ph.ftp, years[y])
        year.ph.ftpdef <- c(year.ph.ftpdef, years[y])
      }else if (IVs[x] == IVs[3]){
        FTRat.m1 <- loop.df$FTRat_Adj[loop.year.m1]
        FTRat.m2 <- loop.df$FTRat_Adj[loop.year.m2]
        FTRat.m3 <- loop.df$FTRat_Adj[loop.year.m3]
        FTRat.m4 <- loop.df$FTRat_Adj[loop.year.m4]
        FTRat.def.m1 <- loop.def.df$FTRat_Adj[loop.year.m1]
        FTRat.def.m2 <- loop.def.df$FTRat_Adj[loop.year.m2]
        FTRat.def.m3 <- loop.def.df$FTRat_Adj[loop.year.m3]
        FTRat.def.m4 <- loop.def.df$FTRat_Adj[loop.year.m4]
        #save values
        FTRATM1 <- c(FTRATM1,FTRat.m1)
        FTRATM2 <- c(FTRATM2,FTRat.m2)
        FTRATM3 <- c(FTRATM3,FTRat.m3)
        FTRATM4 <- c(FTRATM4,FTRat.m4)
        FTRATM1D <- c(FTRATM1D,FTRat.def.m1)
        FTRATM2D <- c(FTRATM2D,FTRat.def.m2)
        FTRATM3D <- c(FTRATM3D,FTRat.def.m3)
        FTRATM4D <- c(FTRATM4D,FTRat.def.m4)
        team.ph.ftrat <- c(team.ph.ftrat, teams[z])
        team.ph.ftratdef <- c(team.ph.ftratdef, teams[z])
        year.ph.ftrat <- c(year.ph.ftrat, years[y])
        year.ph.ftratdef <- c(year.ph.ftratdef, years[y])
      }else if (IVs[x] == IVs[4]){
        ORebP.m1 <- loop.df$ORebP_Adj[loop.year.m1]
        ORebP.m2 <- loop.df$ORebP_Adj[loop.year.m2]
        ORebP.m3 <- loop.df$ORebP_Adj[loop.year.m3]
        ORebP.m4 <- loop.df$ORebP_Adj[loop.year.m4]
        ORebP.def.m1 <- loop.def.df$ORebP_Adj[loop.year.m1]
        ORebP.def.m2 <- loop.def.df$ORebP_Adj[loop.year.m2]
        ORebP.def.m3 <- loop.def.df$ORebP_Adj[loop.year.m3]
        ORebP.def.m4 <- loop.def.df$ORebP_Adj[loop.year.m4]
        #save values
        OREBPM1 <- c(OREBPM1,ORebP.m1)
        OREBPM2 <- c(OREBPM2,ORebP.m2)
        OREBPM3 <- c(OREBPM3,ORebP.m3)
        OREBPM4 <- c(OREBPM4,ORebP.m4)
        OREBPM1D <- c(OREBPM1D,ORebP.def.m1)
        OREBPM2D <- c(OREBPM2D,ORebP.def.m2)
        OREBPM3D <- c(OREBPM3D,ORebP.def.m3)
        OREBPM4D <- c(OREBPM4D,ORebP.def.m4)
        team.ph.orebp <- c(team.ph.orebp, teams[z])
        team.ph.orebpdef <- c(team.ph.orebpdef, teams[z])
        year.ph.orebp <- c(year.ph.orebp, years[y])
        year.ph.orebpdef <- c(year.ph.orebpdef, years[y])
      }else if (IVs[x] == IVs[5]){
        x2PtP.m1 <- loop.df$x2PtP_Adj[loop.year.m1]
        x2PtP.m2 <- loop.df$x2PtP_Adj[loop.year.m2]
        x2PtP.m3 <- loop.df$x2PtP_Adj[loop.year.m3]
        x2PtP.m4 <- loop.df$x2PtP_Adj[loop.year.m4]
        x2PtP.def.m1 <- loop.def.df$x2PtP_Adj[loop.year.m1]
        x2PtP.def.m2 <- loop.def.df$x2PtP_Adj[loop.year.m2]
        x2PtP.def.m3 <- loop.def.df$x2PtP_Adj[loop.year.m3]
        x2PtP.def.m4 <- loop.def.df$x2PtP_Adj[loop.year.m4]
        #Save values
        X2PTPM1 <- c(X2PTPM1,x2PtP.m1)
        X2PTPM2 <- c(X2PTPM2,x2PtP.m2)
        X2PTPM3 <- c(X2PTPM3,x2PtP.m3)
        X2PTPM4 <- c(X2PTPM4,x2PtP.m4)
        X2PTPM1D <- c(X2PTPM1D,x2PtP.def.m1)
        X2PTPM2D <- c(X2PTPM2D,x2PtP.def.m2)
        X2PTPM3D <- c(X2PTPM3D,x2PtP.def.m3)
        X2PTPM4D <- c(X2PTPM4D,x2PtP.def.m4)
        team.ph.x2ptp <- c(team.ph.x2ptp, teams[z])
        team.ph.x2ptpdef <- c(team.ph.x2ptpdef, teams[z])
        year.ph.x2ptp <- c(year.ph.x2ptp, years[y])
        year.ph.x2ptpdef <- c(year.ph.x2ptpdef, years[y])
      }else if (IVs[x] == IVs[6]){
        x3PtRat.m1 <- loop.df$x3PtRat_Adj[loop.year.m1]
        x3PtRat.m2 <- loop.df$x3PtRat_Adj[loop.year.m2]
        x3PtRat.m3 <- loop.df$x3PtRat_Adj[loop.year.m3]
        x3PtRat.m4 <- loop.df$x3PtRat_Adj[loop.year.m4]
        x3PtRat.def.m1 <- loop.def.df$x3PtRat_Adj[loop.year.m1]
        x3PtRat.def.m2 <- loop.def.df$x3PtRat_Adj[loop.year.m2]
        x3PtRat.def.m3 <- loop.def.df$x3PtRat_Adj[loop.year.m3]
        x3PtRat.def.m4 <- loop.def.df$x3PtRat_Adj[loop.year.m4]
        #Save Values
        X3PTRATM1 <- c(X3PTRATM1,x3PtRat.m1)
        X3PTRATM2 <- c(X3PTRATM2,x3PtRat.m2)
        X3PTRATM3 <- c(X3PTRATM3,x3PtRat.m3)
        X3PTRATM4 <- c(X3PTRATM4,x3PtRat.m4)
        X3PTRATM1D <- c(X3PTRATM1D,x3PtRat.def.m1)
        X3PTRATM2D <- c(X3PTRATM2D,x3PtRat.def.m2)
        X3PTRATM3D <- c(X3PTRATM3D,x3PtRat.def.m3)
        X3PTRATM4D <- c(X3PTRATM4D,x3PtRat.def.m4)
        team.ph.x3ptrat <- c(team.ph.x3ptrat, teams[z])
        team.ph.x3ptratdef <- c(team.ph.x3ptratdef, teams[z])
        year.ph.x3ptrat <- c(year.ph.x3ptrat, years[y])
        year.ph.x3ptratdef <- c(year.ph.x3ptratdef, years[y])
      }else{
        x3PtP.m1 <- loop.df$x3PtP_Adj[loop.year.m1]
        x3PtP.m2 <- loop.df$x3PtP_Adj[loop.year.m2]
        x3PtP.m3 <- loop.df$x3PtP_Adj[loop.year.m3]
        x3PtP.m4 <- loop.df$x3PtP_Adj[loop.year.m4]
        x3PtP.def.m1 <- loop.def.df$x3PtP_Adj[loop.year.m1]
        x3PtP.def.m2 <- loop.def.df$x3PtP_Adj[loop.year.m2]
        x3PtP.def.m3 <- loop.def.df$x3PtP_Adj[loop.year.m3]
        x3PtP.def.m4 <- loop.def.df$x3PtP_Adj[loop.year.m4]
        #Save Values
        X3PTPM1 <- c(X3PTPM1,x3PtP.m1)
        X3PTPM2 <- c(X3PTPM2,x3PtP.m2)
        X3PTPM3 <- c(X3PTPM3,x3PtP.m3)
        X3PTPM4 <- c(X3PTPM4,x3PtP.m4)
        X3PTPM1D <- c(X3PTPM1D,x3PtP.def.m1)
        X3PTPM2D <- c(X3PTPM2D,x3PtP.def.m2)
        X3PTPM3D <- c(X3PTPM3D,x3PtP.def.m3)
        X3PTPM4D <- c(X3PTPM4D,x3PtP.def.m4)
        team.ph.x3ptp <- c(team.ph.x3ptp, teams[z])
        team.ph.x3ptpdef <- c(team.ph.x3ptpdef, teams[z])
        year.ph.x3ptp <- c(year.ph.x3ptp, years[y])
        year.ph.x3ptpdef <- c(year.ph.x3ptpdef, years[y])
      }
      
      
      
    }#End IV Loop
    
  }#End Year Loop
  
}#End Team Loop


#combine together
#Top
top.df <- as.data.frame(cbind(team.ph.top, year.ph.top))
top.df$m1 <- TOPM1
top.df$m2 <- TOPM2
top.df$m3 <- TOPM3
top.df$m4 <- TOPM4
top.dfdef <- as.data.frame(cbind(team.ph.topdef, year.ph.topdef))
top.dfdef$m1 <- TOPM1D
top.dfdef$m2 <- TOPM2D
top.dfdef$m3 <- TOPM3D
top.dfdef$m4 <- TOPM4D
#FTP
ftp.df <- as.data.frame(cbind(team.ph.ftp, year.ph.ftp))
ftp.df$m1 <- FTPM1
ftp.df$m2 <- FTPM2
ftp.df$m3 <- FTPM3
ftp.df$m4 <- FTPM4
ftp.dfdef <- as.data.frame(cbind(team.ph.ftpdef, year.ph.ftpdef))
ftp.dfdef$m1 <- FTPM1D
ftp.dfdef$m2 <- FTPM2D
ftp.dfdef$m3 <- FTPM3D
ftp.dfdef$m4 <- FTPM4D
#FTRAT
ftrat.df <- as.data.frame(cbind(team.ph.ftrat, year.ph.ftrat))
ftrat.df$m1 <- FTRATM1
ftrat.df$m2 <- FTRATM2
ftrat.df$m3 <- FTRATM3
ftrat.df$m4 <- FTRATM4
ftrat.dfdef <- as.data.frame(cbind(team.ph.ftratdef, year.ph.ftratdef))
ftrat.dfdef$m1 <- FTRATM1D
ftrat.dfdef$m2 <- FTRATM1D
ftrat.dfdef$m3 <- FTRATM1D
ftrat.dfdef$m4 <- FTRATM1D
#Orebp
orebp.df <- as.data.frame(cbind(team.ph.orebp, year.ph.orebp))
orebp.df$m1 <- OREBPM1
orebp.df$m2 <- OREBPM2
orebp.df$m3 <- OREBPM3
orebp.df$m4 <- OREBPM4
orebp.dfdef <- as.data.frame(cbind(team.ph.orebpdef, year.ph.orebpdef))
orebp.dfdef$m1 <- OREBPM1D
orebp.dfdef$m2 <- OREBPM2D
orebp.dfdef$m3 <- OREBPM3D
orebp.dfdef$m4 <- OREBPM4D
#x2ptp
x2ptp.df <- as.data.frame(cbind(team.ph.x2ptp, year.ph.x2ptp))
x2ptp.df$m1 <- X2PTPM1
x2ptp.df$m2 <- X2PTPM2
x2ptp.df$m3 <- X2PTPM3
x2ptp.df$m4 <- X2PTPM4
x2ptp.dfdef <- as.data.frame(cbind(team.ph.x2ptpdef, year.ph.x2ptpdef))
x2ptp.dfdef$m1 <- X2PTPM1D
x2ptp.dfdef$m2 <- X2PTPM2D
x2ptp.dfdef$m3 <- X2PTPM3D
x2ptp.dfdef$m4 <- X2PTPM4D
#x3ptp
x3ptp.df <- as.data.frame(cbind(team.ph.x3ptp, year.ph.x3ptp))
x3ptp.df$m1 <- X3PTPM1
x3ptp.df$m2 <- X3PTPM2
x3ptp.df$m3 <- X3PTPM3
x3ptp.df$m4 <- X3PTPM4
x3ptp.dfdef <- as.data.frame(cbind(team.ph.x3ptpdef, year.ph.x3ptpdef))
x3ptp.dfdef$m1 <- X3PTPM1D
x3ptp.dfdef$m2 <- X3PTPM2D
x3ptp.dfdef$m3 <- X3PTPM3D
x3ptp.dfdef$m4 <- X3PTPM4D
#x3ptrat
x3ptrat.df <- as.data.frame(cbind(team.ph.x3ptrat, year.ph.x3ptrat))
x3ptrat.df$m1 <- X3PTRATM1
x3ptrat.df$m2 <- X3PTRATM2
x3ptrat.df$m3 <- X3PTRATM3
x3ptrat.df$m4 <- X3PTRATM4
x3ptrat.dfdef <- as.data.frame(cbind(team.ph.x3ptratdef, year.ph.x3ptratdef))
x3ptrat.dfdef$m1 <- X3PTRATM1D
x3ptrat.dfdef$m2 <- X3PTRATM2D
x3ptrat.dfdef$m3 <- X3PTRATM3D
x3ptrat.dfdef$m4 <- X3PTRATM4D

#Bring back year value to each df
top.df$team.year <- paste(top.df$team.ph.top, top.df$year.ph.top, sep="-")
top.df$top <- Off$TOP_Adj[match(top.df$team.year, Off$Team.Year)]
top.dfdef$team.year <- paste(top.dfdef$team.ph.top, top.dfdef$year.ph.top, sep="-")
top.dfdef$top <- Def$TOP_Adj_Def[match(top.dfdef$team.year, Def$Team.Year)]
ftp.df$team.year <- paste(ftp.df$team.ph.ftp, ftp.df$year.ph.ftp, sep="-")
ftp.df$ftp <- Off$FTP_Adj[match(ftp.df$team.year, Off$Team.Year)]
ftp.dfdef$team.year <- paste(ftp.dfdef$team.ph.ftpdef, ftp.dfdef$year.ph.ftpdef, sep="-")
ftp.dfdef$ftp <- Def$FTP_Adj[match(ftp.dfdef$team.year, Def$Team.Year)]
ftrat.df$team.year <- paste(ftrat.df$team.ph.ftrat, ftrat.df$year.ph.ftrat, sep="-")
ftrat.df$ftrat <- Off$FTRat_Adj[match(ftrat.df$team.year, Off$Team.Year)]
ftrat.dfdef$team.year <- paste(ftrat.dfdef$team.ph.ftratdef, ftrat.dfdef$year.ph.ftratdef, sep="-")
ftrat.dfdef$ftrat <- Def$FTRat_Adj[match(ftrat.dfdef$team.year, Def$Team.Year)]
orebp.df$team.year <- paste(orebp.df$team.ph.orebp, orebp.df$year.ph.orebp, sep="-")
orebp.df$orebp <- Off$ORebP_Adj[match(orebp.df$team.year, Off$Team.Year)]
orebp.dfdef$team.year <- paste(orebp.dfdef$team.ph.orebpdef, orebp.dfdef$year.ph.orebpdef, sep="-")
orebp.dfdef$orebp <- Def$ORebP_Adj[match(orebp.dfdef$team.year, Def$Team.Year)]
x2ptp.df$team.year <- paste(x2ptp.df$team.ph.x2ptp, x2ptp.df$year.ph.x2ptp, sep="-")
x2ptp.df$x2ptp <- Off$x2PtP_Adj[match(x2ptp.df$team.year, Off$Team.Year)]
x2ptp.dfdef$team.year <- paste(x2ptp.dfdef$team.ph.x2ptpdef, x2ptp.dfdef$year.ph.x2ptpdef, sep="-")
x2ptp.dfdef$x2ptp <- Def$x2PtP_Adj[match(x2ptp.dfdef$team.year, Def$Team.Year)]
x3ptp.df$team.year <- paste(x3ptp.df$team.ph.x3ptp, x3ptp.df$year.ph.x3ptp, sep="-")
x3ptp.df$x3ptp <- Off$x3PtP_Adj[match(x3ptp.df$team.year, Off$Team.Year)]
x3ptp.dfdef$team.year <- paste(x3ptp.dfdef$team.ph.x3ptpdef, x3ptp.dfdef$year.ph.x3ptpdef, sep="-")
x3ptp.dfdef$x3ptp <- Def$x3PtP_Adj[match(x3ptp.dfdef$team.year, Def$Team.Year)]
x3ptrat.df$team.year <- paste(x3ptrat.df$team.ph.x3ptrat, x3ptrat.df$year.ph.x3ptrat, sep="-")
x3ptrat.df$x3ptrat <- Off$x3PtP_Adj[match(x3ptrat.df$team.year, Off$Team.Year)]
x3ptrat.dfdef$team.year <- paste(x3ptrat.dfdef$team.ph.x3ptratdef, x3ptrat.dfdef$year.ph.x3ptratdef, sep="-")
x3ptrat.dfdef$x3ptrat <- Def$x3PtP_Adj[match(x3ptrat.dfdef$team.year, Def$Team.Year)]

#Export to csv
write.csv(top.df, file="TOP_P4Y.csv")
write.csv(top.dfdef, file="TOPD_P4Y.csv")
write.csv(ftp.df, file="FTP_P4Y.csv")
write.csv(ftp.dfdef, file="FTPDEF_P4Y.csv")
write.csv(ftrat.df, file="FTRAT_P4Y.csv")
write.csv(ftrat.dfdef, file="FTRATDEF_P4Y.csv")
write.csv(orebp.df, file="OREBP_P4Y.csv")
write.csv(orebp.dfdef, file="OREBPDEF_P4Y.csv")
write.csv(x2ptp.df, file="x2PTP_P4Y.csv")
write.csv(x2ptp.dfdef, file="x2PTPDEF_P4Y.csv")
write.csv(x3ptp.df, file="x3PTP_P4Y.csv")
write.csv(x3ptp.dfdef, file="x3PTPDEF_P4Y.csv")
write.csv(x3ptrat.df, file="x3PTRAT_P4Y.csv")
write.csv(x3ptrat.dfdef, file="x3PTRATDEF_P4Y.csv")


