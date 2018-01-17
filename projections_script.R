#Predict Points Scored by each player

library(xgboost)
library(zoo)
library(data.table)
library(dplyr)
library(MatrixModels)
library(data.table)
library(Matrix)
set.seed(8)

#Xgboost Model - eta = .01

today.date <- 20161209
test.date <- 20161209
#Load NBA Game logs
game.logs<- read.csv("NBA Game Logs 12_11_2016.csv",header=TRUE)
playerlist<-fread("FanDuel-NBA-2017-01-14-17652-players-list.csv",header=TRUE,
                  select = c("Position","First Name","Last Name","Salary","Team","Injury Indicator","Opponent"))
team_list<-fread("team_list.csv",header=TRUE,select = c("team","Team"))
PlayerStats<-read.csv("Player Game Logs 12_13_2016.csv",header = TRUE)
Hollinger<-read.csv("Hollinger 1_13_2017.csv",header = TRUE)
Hollinger$city<-NULL
SoS.Data<-read.csv("SoS 1_13_2017.csv", header=TRUE)
SoS.Data$city<-NULL

playerlist<-merge(playerlist,team_list, by ="Team")

playerlist$name<- paste(playerlist$`First Name`,playerlist$`Last Name`," ")
playerlist$`First Name`<-NULL
playerlist$`Last Name`<-NULL
playerlist$date<-today.date
playerlist$season<-2016
playerlist<-subset(playerlist,`Injury Indicator`!="O")
game.logs<-subset(game.logs, playoffs==0)

game.logs$h.points<-game.logs$points
game.logs$points<-NULL

PlayerStats<-bind_rows(PlayerStats,playerlist)

#Add an opponent column to PlayerStats data frame
PlayerStats.home<-merge(PlayerStats,game.logs, by=c("team","date","season"))
PlayerStats.home$site<-1
PlayerStats$o.team<-PlayerStats$team
PlayerStats$team<-NULL

PlayerStats.away<-merge(PlayerStats,game.logs, by=c("o.team","date","season"))
PlayerStats.away$line<-PlayerStats.away$line*-1
PlayerStats.away$site<-0
colnames(PlayerStats.away)
colnames(PlayerStats.away)[1] <- "team"
colnames(PlayerStats.away)[28]<-"o.team"
Data<-rbind(PlayerStats.home,PlayerStats.away)

Data.season<-merge(Hollinger, SoS.Data, by=c("team","season"))#Update to ensure SoS and Hollinger data match
#Merge season total data with game log data for opponent
Data.season$o.team<-Data.season$team
Data.season$team<-NULL
Data<-merge(Data,Data.season,by=c("season","o.team"))

#Create new calculated columns in dataset
#Result of game(Home team point differential and total points)
Data$result<-Data$h.points-Data$o.points
Data$total.pts<-Data$h.points+Data$o.points

#Check if game is 2nd of back to back
Data$b2b.x<-Data$rest==0
Data$b2b.y<-Data$o.rest==0

#Remove blank and other strange name entries

Data$name<-tolower(Data$name)
Data$name<-gsub("[[:punct:]]", "", Data$name)
Data$name<-gsub(" ","",Data$name)
Data$name[Data$name=="nenehilario"]<-"nene"
Data$name[Data$name=="lucrichardmbahamoute"]<-"lucmbahamoute"
Data$name[Data$name=="louwilliams"]<-"louiswilliams"
Data$name[Data$name=="kellyoubrejr"]<-"kellyoubre"
Data$name[Data$name=="jjbarea"]<-"josejuanbarea"

Data<-subset(Data,date<=test.date)
#####Create new variables#####
#Player average points per game for season
p.avg.points<-setNames(aggregate(Data$points,by =list(Data$name,Data$season),mean,na.rm=TRUE),
                    c("name","season","avg.points"))

Data<-Data[order(Data$date),]
Data <- 
  Data %>%
  group_by(name) %>%
  mutate(lag.points=lag(points, 1),lag.points.2=lag(points,2),lag.points.3=lag(points,3),
         lag.points.4=lag(points,4),
         lag.points.5=lag(points,5))

Data<-merge(Data,p.avg.points,by=c("name","season"))
colnames(Data)
Data$points.last5<-rowMeans(Data[69:73],na.rm=TRUE)

#Average defensive rebounds
p.avg.defensive.rebounds<-setNames(aggregate(Data$defensive.rebounds,by =list(Data$name,Data$season),mean,na.rm=TRUE),
                    c("name","season","avg.defensive.rebounds"))
Data<-Data[order(Data$date),]
Data <- 
  Data %>%
  group_by(name) %>%
  mutate(lag.defensive.rebounds=lag(defensive.rebounds, 1),lag.defensive.rebounds.2=lag(defensive.rebounds,2),
         lag.defensive.rebounds.3=lag(defensive.rebounds,3),
         lag.defensive.rebounds.4=lag(defensive.rebounds,4), lag.defensive.rebounds.5=lag(defensive.rebounds,5))
Data<-merge(Data,p.avg.defensive.rebounds,by=c("name","season"))
colnames(Data)
Data$defensive.rebounds.last5<-rowMeans(Data[76:80],na.rm=TRUE)

#Average offensive rebounds
p.avg.offensive.rebounds<-setNames(aggregate(Data$offensive.rebounds,by =list(Data$name,Data$season),mean,na.rm=TRUE),
                         c("name","season","avg.offensive.rebounds"))
Data<-Data[order(Data$date),]
Data <- 
  Data %>%
  group_by(name) %>%
  mutate(lag.offensive.rebounds=lag(offensive.rebounds, 1),lag.offensive.rebounds.2=lag(offensive.rebounds,2),
         lag.offensive.rebounds.3=lag(offensive.rebounds,3),
         lag.offensive.rebounds.4=lag(offensive.rebounds,4), lag.offensive.rebounds.5=lag(offensive.rebounds,5))
Data<-merge(Data,p.avg.offensive.rebounds,by=c("name","season"))
colnames(Data)
Data$offensive.rebounds.last5<-rowMeans(Data[83:87],na.rm=TRUE)

#Average rebounds
Data$avg.rebounds<-Data$avg.offensive.rebounds+Data$avg.defensive.rebounds
Data$rebounds.last5<-Data$offensive.rebounds.last5+Data$defensive.rebounds.last5

#Average assists
p.avg.assists<-setNames(aggregate(Data$assists,by =list(Data$name,Data$season),mean,na.rm=TRUE),
                                   c("name","season","avg.assists"))
Data<-Data[order(Data$date),]
Data <- 
  Data %>%
  group_by(name) %>%
  mutate(lag.assists=lag(assists, 1),lag.assists.2=lag(assists,2),
         lag.assists.3=lag(assists,3),
         lag.assists.4=lag(assists,4), lag.assists.5=lag(assists,5))
Data<-merge(Data,p.avg.assists,by=c("name","season"))
colnames(Data)
Data$assists.last5<-rowMeans(Data[92:96],na.rm=TRUE)

#Average blocks
p.avg.blocks<-setNames(aggregate(Data$blocks,by =list(Data$name,Data$season),mean, na.rm=TRUE),
                        c("name","season","avg.blocks"))
Data<-Data[order(Data$date),]
Data <- 
  Data %>%
  group_by(name) %>%
  mutate(lag.blocks=lag(blocks, 1),lag.blocks.2=lag(blocks,2),
         lag.blocks.3=lag(blocks,3),
         lag.blocks.4=lag(blocks,4), lag.blocks.5=lag(blocks,5))
Data<-merge(Data,p.avg.blocks,by=c("name","season"))
colnames(Data)
Data$blocks.last5<-rowMeans(Data[99:103],na.rm=TRUE)

#Average turnovers
p.avg.turnovers<-setNames(aggregate(Data$turnovers,by =list(Data$name,Data$season),mean,na.rm=TRUE),
                       c("name","season","avg.turnovers"))
Data<-Data[order(Data$date),]
Data <- 
  Data %>%
  group_by(name) %>%
  mutate(lag.turnovers=lag(turnovers, 1),lag.turnovers.2=lag(turnovers,2),
         lag.turnovers.3=lag(turnovers,3),
         lag.turnovers.4=lag(turnovers,4), lag.turnovers.5=lag(turnovers,5))
Data<-merge(Data,p.avg.turnovers,by=c("name","season"))
colnames(Data)
Data$turnovers.last5<-rowMeans(Data[106:110],na.rm=TRUE)

#Average steals
p.avg.steals<-setNames(aggregate(Data$steals,by =list(Data$name,Data$season),mean, na.rm=TRUE),
                          c("name","season","avg.steals"))
Data<-Data[order(Data$date),]
Data <- 
  Data %>%
  group_by(name) %>%
  mutate(lag.steals=lag(steals, 1),lag.steals.2=lag(steals,2),
         lag.steals.3=lag(steals,3),
         lag.steals.4=lag(steals,4), lag.steals.5=lag(steals,5))
Data<-merge(Data,p.avg.steals,by=c("name","season"))
colnames(Data)
Data$steals.last5<-rowMeans(Data[113:117],na.rm=TRUE)

#Average minutes
p.avg.minutes<-setNames(aggregate(Data$minutes,by =list(Data$name,Data$season),mean, na.rm=TRUE),
                       c("name","season","avg.minutes"))
Data<-Data[order(Data$date),]
Data <- 
  Data %>%
  group_by(name) %>%
  mutate(lag.minutes=lag(minutes, 1),lag.minutes.2=lag(minutes,2),
         lag.minutes.3=lag(minutes,3),
         lag.minutes.4=lag(minutes,4), lag.minutes.5=lag(minutes,5))
Data<-merge(Data,p.avg.minutes,by=c("name","season"))
colnames(Data)
Data$minutes.last5<-rowMeans(Data[120:124],na.rm=TRUE)

#Average free throws attempted
p.avg.free.throws.attempted<-setNames(aggregate(Data$free.throws.attempted,by =list(Data$name,Data$season),mean, na.rm=TRUE),
                        c("name","season","avg.free.throws.attempted"))
Data<-Data[order(Data$date),]
Data <- 
  Data %>%
  group_by(name) %>%
  mutate(lag.free.throws.attempted=lag(free.throws.attempted, 1),lag.free.throws.attempted.2=lag(free.throws.attempted,2),
         lag.free.throws.attempted.3=lag(free.throws.attempted,3),
         lag.free.throws.attempted.4=lag(free.throws.attempted,4), lag.free.throws.attempted.5=lag(free.throws.attempted,5))
Data<-merge(Data,p.avg.free.throws.attempted,by=c("name","season"))
colnames(Data)
Data$free.throws.attempted.last5<-rowMeans(Data[127:131],na.rm=TRUE)#Update col numbers

#Average free throws made
p.avg.free.throws.made<-setNames(aggregate(Data$free.throws.made,by =list(Data$name,Data$season),mean, na.rm=TRUE),
                                      c("name","season","avg.free.throws.made"))
Data<-Data[order(Data$date),]
Data <- 
  Data %>%
  group_by(name) %>%
  mutate(lag.free.throws.made=lag(free.throws.made, 1),lag.free.throws.made.2=lag(free.throws.made,2),
         lag.free.throws.made.3=lag(free.throws.made,3),
         lag.free.throws.made.4=lag(free.throws.made,4), lag.free.throws.made.5=lag(free.throws.made,5))
Data<-merge(Data,p.avg.free.throws.made,by=c("name","season"))
colnames(Data)
Data$free.throws.made.last5<-rowMeans(Data[134:138],na.rm=TRUE)#Update col numbers

#Average three pointers attempted
p.avg.three.pointers.attempted<-setNames(aggregate(Data$three.pointers.attempted,by =list(Data$name,Data$season),mean, na.rm=TRUE),
                                 c("name","season","avg.three.pointers.attempted"))
Data<-Data[order(Data$date),]
Data <- 
  Data %>%
  group_by(name) %>%
  mutate(lag.three.pointers.attempted=lag(three.pointers.attempted, 1),lag.three.pointers.attempted.2=lag(three.pointers.attempted,2),
         lag.three.pointers.attempted.3=lag(three.pointers.attempted,3),
         lag.three.pointers.attempted.4=lag(three.pointers.attempted,4), lag.three.pointers.attempted.5=lag(three.pointers.attempted,5))
Data<-merge(Data,p.avg.three.pointers.attempted,by=c("name","season"))
colnames(Data)
Data$three.pointers.attempted.last5<-rowMeans(Data[141:145],na.rm=TRUE)#Update col numbers

#Average three pointers made
p.avg.three.pointers.made<-setNames(aggregate(Data$three.pointers.made,by =list(Data$name,Data$season),mean, na.rm=TRUE),
                                         c("name","season","avg.three.pointers.made"))
Data<-Data[order(Data$date),]
Data <- 
  Data %>%
  group_by(name) %>%
  mutate(lag.three.pointers.made=lag(three.pointers.made, 1),lag.three.pointers.made.2=lag(three.pointers.made,2),
         lag.three.pointers.made.3=lag(three.pointers.made,3),
         lag.three.pointers.made.4=lag(three.pointers.made,4), lag.three.pointers.made.5=lag(three.pointers.made,5))
Data<-merge(Data,p.avg.three.pointers.made,by=c("name","season"))
colnames(Data)
Data$three.pointers.made.last5<-rowMeans(Data[148:152],na.rm=TRUE)#Update col numbers

#Standard deviation
#Fantasy points each game
#Calculate Fan Duel points
assists.mult<-1.5
blocks.mult<-2
points.mult<-1
rebounds.mult<-1.2
steals.mult<-2
turnovers.mult<-(-1)

attach(Data)
Data$fan.points<-assists*assists.mult+blocks*blocks.mult+points*points.mult+rebounds*rebounds.mult+
  steals*steals.mult+turnovers*turnovers.mult

y<-aggregate(fan.points ~ name+season, Data, function(x) mean = mean(x))
y$mean<-y$fan.points
y$fan.points<-NULL
Data<-merge(Data,y,by =c("name","season"))
sd<-aggregate(fan.points ~ name+season, Data, function(x) sd = sd(x))
sd$sd<-sd$fan.points
sd$fan.points<-NULL
Data<-merge(Data,sd,by=c("name","season"))

Data$name.num<-as.numeric(as.factor(Data$name))
#Ensure that line is correct for home and away teams
rows<-nrow(Data)
#Input into sparse matrix 
#Create test dataset 
previous_na_action<- options('na.action')
options(na.action='na.pass')

#Create train dataset
train <- subset(Data,date<test.date)

train.matrix <-sparse.model.matrix(~sd+line+total+site.streak+o.site.streak+streak+o.streak+rest+o.rest+site+
                                     o.wins+o.losses+PACE+AST+TO+DEF.EFF+EFF.FG.+TS.+SOS+ORR+DRR+
                                     points.last5+defensive.rebounds.last5+offensive.rebounds.last5+assists.last5+
                                     blocks.last5+steals.last5+turnovers.last5+minutes.last5+
                                     free.throws.attempted.last5+free.throws.made.last5+three.pointers.made.last5+
                                     three.pointers.attempted.last5+
                                     avg.points+avg.defensive.rebounds+avg.offensive.rebounds+avg.assists+
                                     avg.blocks+avg.steals+avg.turnovers+avg.minutes+
                                     avg.free.throws.attempted+avg.free.throws.made+avg.three.pointers.made+
                                     avg.three.pointers.attempted+
                                     lag.points+lag.points.2+lag.points.3+lag.points.4+lag.points.5+
                                     lag.defensive.rebounds+lag.defensive.rebounds.2+lag.defensive.rebounds.3+lag.defensive.rebounds.4+lag.defensive.rebounds.5+
                                     lag.offensive.rebounds+lag.offensive.rebounds.2+lag.offensive.rebounds.3+lag.offensive.rebounds.4+lag.offensive.rebounds.5+
                                     lag.assists+lag.assists.2+ lag.assists.3+ lag.assists.4+ lag.assists.5+
                                     lag.blocks+lag.blocks.2+lag.blocks.3+lag.blocks.4+lag.blocks.5+
                                     lag.steals+lag.steals.2+lag.steals.3+lag.steals.4+lag.steals.5+
                                     lag.turnovers+lag.turnovers.2+lag.turnovers.3+lag.turnovers.4+lag.turnovers.5+
                                     lag.minutes+lag.minutes.2+lag.minutes.3+lag.minutes.4+lag.minutes.5+
                                     lag.free.throws.attempted+lag.free.throws.attempted.2+lag.free.throws.attempted.3+lag.free.throws.attempted.4+lag.free.throws.attempted.5+
                                     lag.free.throws.made+lag.free.throws.made.2+lag.free.throws.made.3+lag.free.throws.made.4+lag.free.throws.made.5+
                                     lag.three.pointers.attempted+lag.three.pointers.attempted.2+lag.three.pointers.attempted.3+lag.three.pointers.attempted.4+lag.three.pointers.attempted.5+
                                     lag.three.pointers.made+lag.three.pointers.made.2+lag.three.pointers.made.3+lag.three.pointers.made.4+lag.three.pointers.made.5,
                                   data=train,
                                   contrasts.arg = "site",
                                   sparse = FALSE, sci = FALSE)
options(na.action=previous_na_action$na.action)

#Model Parameters
max.depth <-6
eta<-.3
i=1

#RMSE<-vector()
model.list <- vector(mode="list", length=6)
#points, rebounds,assists,blocks,steals,turnovers
pred.cols<-c(17,19,6,7,20,23)
for (i in 1:length(pred.cols)){
  gc()
  label<-train[,pred.cols[i]]
  #Create input for xgboost
  train.DMatrix <-xgb.DMatrix(data = train.matrix,label = label)
  
  #Cross-validation
  xgb.tab<-xgb.cv(data = train.DMatrix, objective = "reg:linear", booster = "gbtree",nrounds = 10000,
                  evaluation = "rmse", nthreads = 10,eta = eta, max_depth = max.depth, nfold = 4,
                  early.stop.round = 10, maximize=FALSE,subsample=.8)
  
  min.error.idx = 10# which.min(xgb.tab[, test.rmse.mean])
  #Build model using xgboost
  model<- xgboost(data = train.DMatrix, objective = "reg:linear", booster = "gbtree",
                  nrounds = min.error.idx, evaluation = "rmse", nthreads = 10, maximize = FALSE,
                  eta = eta, max_depth = max.depth,subsample=.8)
  model.list[[i]]<-model
}

#Save point: Daily Fantasy Model v101
