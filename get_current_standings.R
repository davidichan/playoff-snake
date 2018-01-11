library(RCurl)
library(XML)
library(plyr)
library(gridExtra)

rootp <- "data/"
load(paste0(rootp,"NHL_standings_2016.RData"))
load(paste0(rootp,"NHL_standings_2017.RData"))
load(paste0(rootp,"cgy_points_hist.RData"))
load(paste0(rootp,"season_records_2015.RData"))
load(paste0(rootp,"season_records_2016.RData"))
load(paste0(rootp,"season_records_2017.RData"))

clean_results <- function(results.ls){
  cols <- ncol(results.ls[[1]]) + 1
  results.clean <- data.frame(matrix(NA, nrow = 0, ncol = cols))
  results.clean <- rbind(results.clean, results.ls[[1]])
  #print(results.clean)
  results.clean <- results.clean[results.clean$Opponent!='Opponent',]
  results.clean <- results.clean[,c(1,2,5,6,7,10,11,12)]
  
  results.clean$GP <- as.integer(as.character(results.clean$GP))
  results.clean$Date <- as.Date(as.character(results.clean$Date))
  results.clean$GF <- as.integer(as.character(results.clean$GF))
  results.clean$GA <- as.integer(as.character(results.clean$GA))
  results.clean$W <- as.integer(as.character(results.clean$W))
  results.clean$L <- as.integer(as.character(results.clean$L))
  results.clean$OL <- as.integer(as.character(results.clean$OL))
  
  results.clean$Points <- (results.clean$W) * 2 + results.clean$OL * 1
  return(results.clean)
}

#Specify the current season and teams to grab data for
  seasons <- c("2018")
  #teams <- c("ANA", "CGY")
  teams <- c("ANA", "ARI", "BOS", "BUF", "CAR", "CGY", "CHI", "CBJ", "COL", "DAL", "DET", "EDM", "FLA", "LAK", "MIN", "MTL", "NSH", "NJD", "NYI", "NYR", "OTT", "PHI", "PIT", "SJS", "STL", "TBL", "TOR", "VAN", "WSH", "WPG")
  post_URL <- paste0("/",seasons,"_games.html")
  current_records <- lapply(teams, function(x) readHTMLTable(getURL(paste0("https://www.hockey-reference.com/teams/",x,post_URL,"")), header=T))
  names(current_records) <- paste0(teams, seasons)

#Clean up the table grabbed for each team
  current.records.clean <- lapply(current_records, function(x) clean_results(x))

##Get current points above the snake
current.snake.diff <- lapply(current.records.clean, function(x) x$Points[!is.na(x$Points)] %>% last)
current.snake.diff <- as.data.frame(unlist(current.snake.diff))
names(current.snake.diff) <- "PointsDiff"

  
#Create new columns for each team
  for(i in seq_along(teams)){
    #Add in a column for the number of points over 82 games for each of the preceding years, along with a linear line representing
    #the 96 point pace
    if(nrow(current.records.clean[[i]]) < 82){
      current.records.clean[[i]] <- rbind(current.records.clean[[i]], list(82,NA,NA,NA,NA,NA,NA,NA,NA))
    }
    current.records.clean[[i]]$P2015 <- season_records2015.clean[[i]]$Points
    current.records.clean[[i]]$P2016 <- season_records2016.clean[[i]]$Points
    current.records.clean[[i]]$P2017 <- season2017.records.clean[[i]]$Points
    current.records.clean[[i]]$Snake <- current.records.clean[[i]]$GP * (96/82.0)
    
    #Calculate the point differential to the 96 point pace for each game
    current.records.clean[[i]]$P18_snake <- current.records.clean[[i]]$Points - current.records.clean[[i]]$Snake
    current.records.clean[[i]]$P17_snake <- current.records.clean[[i]]$P2017 - current.records.clean[[i]]$Snake
    current.records.clean[[i]]$P16_snake <- current.records.clean[[i]]$P2016 - current.records.clean[[i]]$Snake
    current.records.clean[[i]]$P15_snake <- current.records.clean[[i]]$P2015 - current.records.clean[[i]]$Snake
    
    ### take out adjusted snake for now     
    #Add in a column to show how many points each opponent got in the preceding season 
    ##current.records.clean[[i]] <- join(current.records.clean[[i]], points16, by = "Opponent")
    ##colnames(current.records.clean[[i]])[colnames(current.records.clean[[i]]) == "PTS"] <- "Pts_ssnake16"
   
    #For the adjusted snake, taking into account the strength of the opponents
    #Game against worst team is worth 96/82 + 1 points, against best team, is worth 96/82 - 1 pts
    #Using a range of +/- 27 points as the min and max points
    ##season2017.records.clean[[i]]$SS17 <- season2017.records.clean[[i]]$Pts_ssnake16/(-27) + 4.726
    
    #Using this method, the Flames faced disproportionately poor opposition and sum of snake points would
    #have been >96 points. Therefore, sum up the points, divide and readjust each point total to sum to 96. 
    ##int_total <- cumsum(season2017.records.clean[[i]]$SS17)[82]
    ##season2017.records.clean[[i]]$SS17 <- season2017.records.clean[[i]]$SS17 / int_total * 96
    ##season2017.records.clean[[i]]$deltaSS17 <- season2017.records.clean[[i]]$Points - cumsum(season2017.records.clean[[i]]$SS17)
  }
  
  names(current.records.clean) <- paste0(teams, seasons)
  save(current.records.clean, file=paste0(rootp,"current_records.RData"))
  