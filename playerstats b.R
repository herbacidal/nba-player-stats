
library(ggplot2)
playerstats <- read.csv("C:/Users/herb/Desktop/msbi courses/dss 470 R statistical language/2016-2017 nba player stats.csv",header=TRUE)


###assist to turnover ratio calculation
with(playerstats,asst2turnover<-function(playerstats$Assists,playerstats$Turnovers){
  playerstats$Assists/playerstats$Turnovers
  })


##stats grouped by position of player

win.graph()
par(mfrow=c(2,2))
boxplot(Blocks~Position,data=playerstats,main="Blocks by Position")
boxplot(TotalRebounds~Position,data=playerstats,main="Total Rebounds by Position")
boxplot(Assists~Position,data=playerstats,main="Assists by Position")
boxplot(Steals~Position,data=playerstats,main="Steals by Position")


##display top 20 scorers in FieldGoals/Game to their Minutes Played/Game to Discover Scoring Efficiency
scorers <- playerstats[order(playerstats[,8],decreasing=TRUE),c(1,7,8)]
topscorers<-head(scorers,n=20)
win.graph()
with(topscorers,plot(FieldGoals~MinutesPlayed, xlab="Minutes Played Per Game", ylab="Number Field Goals Per Game",main="Top 20 Players, Scoring Efficiency"))
text(FieldGoals~MinutesPlayed,labels=topscorers[,1],data=topscorers,pos=2,cex=0.9,font=1)##some overlap in labels name??

##player averages into teams
okc<-subset(playerstats,Team=="Oklahoma City Thunder",c(1,7,8))
phi<-subset(playerstats,Team=="Philadelphia 76ers",c(1,7,8))
nyk<-subset(playerstats,Team=="New York Knicks",c(1,7,8))
dal<-subset(playerstats,Team=="Dallas Mavericks",c(1,7,8))
hou<-subset(playerstats,Team=="Houston Rockets",c(1,7,8))
lal<-subset(playerstats,Team=="Los Angeles Lakers",c(1,7,8))
lac<-subset(playerstats,Team=="Los Angeles Clippers",c(1,7,8))
sac<-subset(playerstats,Team=="Sacramento Kings",c(1,7,8))
por<-subset(playerstats,Team=="Portland Trailblazers",c(1,7,8))
uta<-subset(playerstats,Team=="Utah Jazz",c(1,7,8))
sas<-subset(playerstats,Team=="San Antonio Spurs",c(1,7,8))
gsw<-subset(playerstats,Team=="Golden State Warriors",c(1,7,8))
den<-subset(playerstats,Team=="Denver Nuggets",c(1,7,8))
brk<-subset(playerstats,Team=="Brooklyn Nets",c(1,7,8))
bos<-subset(playerstats,Team=="Boston Celtics",c(1,7,8))
orl<-subset(playerstats,Team=="Orlando Magic",c(1,7,8))
mem<-subset(playerstats,Team=="Memphis Grizzlies",c(1,7,8))
mia<-subset(playerstats,Team=="Miami Heat",c(1,7,8))
nol<-subset(playerstats,Team=="New Orleans Pelicans",c(1,7,8))
atl<-subset(playerstats,Team=="Atlanta Hawks",c(1,7,8))
chi<-subset(playerstats,Team=="Chicago Bulls",c(1,7,8))
mil<-subset(playerstats,Team=="Milwaukee Bucks",c(1,7,8))
det<-subset(playerstats,Team=="Detroit Pistons",c(1,7,8))
tor<-subset(playerstats,Team=="Toronto Raptors",c(1,7,8))
cle<-subset(playerstats,Team=="Cleveland Cavaliers",c(1,7,8))
ind<-subset(playerstats,Team=="Indiana Pacers",c(1,7,8))
was<-subset(playerstats,Team=="Washington Wizards",c(1,7,8))
cha<-subset(playerstats,Team=="Charlotte Hornets",c(1,7,8))
min<-subset(playerstats,Team=="Minnesota Timberwolves",c(1,7,8))
pho<-subset(playerstats,Team=="Phoenix Suns",c(1,7,8))
##display team averages for player FieldGoals/Minutes Played
win.graph()
par(mfrow=c(3,5))
title("Player Scoring Efficiency by Team",outer=TRUE)
plot(mean(FieldGoals)~mean(MinutesPlayed),data=okc,xlab="Player Minutes/Game",ylab="Player Field Goals/Game",main="Oklahoma City Thunder")
plot(mean(FieldGoals)~mean(MinutesPlayed),data=phi,xlab="Player Minutes/Game",ylab="Player Field Goals/Game",main="Philadelphia 76ers")
plot(mean(FieldGoals)~mean(MinutesPlayed),data=nyk,xlab="Player Minutes/Game",ylab="Player Field Goals/Game",main="New York Knicks")
plot(mean(FieldGoals)~mean(MinutesPlayed),data=dal,xlab="Player Minutes/Game",ylab="Player Field Goals/Game",main="Dallas Mavericks")
plot(mean(FieldGoals)~mean(MinutesPlayed),data=hou,xlab="Player Minutes/Game",ylab="Player Field Goals/Game",main="Houston Rockets")
plot(mean(FieldGoals)~mean(MinutesPlayed),data=lal,xlab="Player Minutes/Game",ylab="Player Field Goals/Game",main="Los Angeles Lakers")
plot(mean(FieldGoals)~mean(MinutesPlayed),data=lac,xlab="Player Minutes/Game",ylab="Player Field Goals/Game",main="Los Angeles Clippers")
plot(mean(FieldGoals)~mean(MinutesPlayed),data=sac,xlab="Player Minutes/Game",ylab="Player Field Goals/Game",main="Sacramento Kings")
plot(mean(FieldGoals)~mean(MinutesPlayed),data=por,xlab="Player Minutes/Game",ylab="Player Field Goals/Game",main="Portland TrailBlazers")
plot(mean(FieldGoals)~mean(MinutesPlayed),data=uta,xlab="Player Minutes/Game",ylab="Player Field Goals/Game",main="Utah Jazz")
plot(mean(FieldGoals)~mean(MinutesPlayed),data=sas,xlab="Player Minutes/Game",ylab="Player Field Goals/Game",main="San Antonio Spurs")
plot(mean(FieldGoals)~mean(MinutesPlayed),data=gsw,xlab="Player Minutes/Game",ylab="Player Field Goals/Game",main="Golden State Warriors")
plot(mean(FieldGoals)~mean(Minuteslayed),data=den,xlab="Player Minutes/Game",ylab="Player Field Goals/Game",main="Denver Nuggets")
plot(mean(FieldGoals)~mean(MinutesPlayed),data=brk,xlab="Player Minutes/Game",ylab="Player Field Goals/Game",main="Brooklyn Nets")
plot(mean(FieldGoals)~mean(MinutesPlayed),data=bos,xlab="Player Minutes/Game",ylab="Player Field Goals/Game",main="Boston Celtics")
win.graph()
par(mfrow=c(3,5))
title("Player Scoring Efficiency by Team",outer=TRUE)
plot(mean(FieldGoals)~mean(MinutesPlayed),data=orl,xlab="Player Minutes/Game",ylab="Player Field Goals/Game",main="Orlando Magic")
plot(mean(FieldGoals)~mean(MinutesPlayed),data=mem,xlab="Player Minutes/Game",ylab="Player Field Goals/Game",main="Memphis Grizzlies")
plot(mean(FieldGoals)~mean(MinutesPlayed),data=mia,xlab="Player Minutes/Game",ylab="Player Field Goals/Game",main="Miami Heat")
plot(mean(FieldGoals)~mean(MinutesPlayed),data=nol,xlab="Player Minutes/Game",ylab="Player Field Goals/Game",main="New Orleans Pelicans")
plot(mean(FieldGoals)~mean(MinutesPlayed),xlab="Player Minutes/Game",ylab="Player Field Goals/Game",data=atl,main="Atlanta Hawks")
plot(mean(FieldGoals)~mean(MinutesPlayed),xlab="Player Minutes/Game",ylab="Player Field Goals/Game",data=chi,main="Chicago Bulls")
plot(mean(FieldGoals)~mean(MinutesPlayed),xlab="Player Minutes/Game",ylab="Player Field Goals/Game",data=mil,main="Milwaukee Bucks")
plot(mean(FieldGoals)~mean(MinutesPlayed),xlab="Player Minutes/Game",ylab="Player Field Goals/Game",data=det,main="Detroit Pistons")
plot(mean(FieldGoals)~mean(MinutesPlayed),xlab="Player Minutes/Game",ylab="Player Field Goals/Game",data=tor,main="Toronto Raptors")
plot(mean(FieldGoals)~mean(MinutesPlayed),xlab="Player Minutes/Game",ylab="Player Field Goals/Game",data=cle,main="Cleveland Cavaliers")
plot(mean(FieldGoals)~mean(MinutesPlayed),xlab="Player Minutes/Game",ylab="Player Field Goals/Game",data=ind,main="Indiana Pacers")
plot(mean(FieldGoals)~mean(MinutesPlayed),xlab="Player Minutes/Game",ylab="Player Field Goals/Game",data=was,main="Washington Wizards")
plot(mean(FieldGoals)~mean(MinutesPlayed),xlab="Player Minutes/Game",ylab="Player Field Goals/Game",data=cha,main="Charlotte Hornes")
plot(mean(FieldGoals)~mean(MinutesPlayed),xlab="Player Minutes/Game",ylab="Player Field Goals/Game",data=min,main="Minnesota Timberwolves")
plot(mean(FieldGoals)~mean(MinutesPlayed),xlab="Player Minutes/Game",ylab="Player Field Goals/Game",data=pho,main="Phoenix Suns")


