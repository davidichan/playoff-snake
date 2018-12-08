library(RCurl)
library(XML)
library(plyr)
library(gridExtra)
library(dplyr)
library(tibble)

rootp <- "data/"
load(paste0(rootp,"NHL_standings_2016.RData"))
load(paste0(rootp,"NHL_standings_2017.RData"))
load(paste0(rootp,"cgy_points_hist.RData"))
load(paste0(rootp,"season_records_2015.RData"))
load(paste0(rootp,"season_records_2016.RData"))
load(paste0(rootp,"season_records_2017.RData"))
load(paste0(rootp,"season_records_2018.RData"))

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
  seasons <- c("2019")
  #teams <- c("ANA", "CGY", "VEG")
  teams <- c("ANA", "ARI", "BOS", "BUF", "CAR", "CGY", "CHI", "CBJ", "COL", "DAL", "DET", "EDM", "FLA", "LAK", "MIN", "MTL", "NSH", "NJD", "NYI", "NYR", "OTT", "PHI", "PIT", "SJS", "STL", "TBL", "TOR", "VAN", "VEG", "WSH", "WPG")
  pacific <- c("ANA", "ARI", "CGY", "EDM", "LAK", "SJS", "VAN", "VEG")
  central <- c("CHI", "COL" , "DAL", "MIN", "NSH", "STL", "WPG")
  atlantic <- c("BOS", "BUF", "DET", "FLA", "MTL", "OTT",  "TBL", "TOR")
  metropolitan <- c("CAR", "CBJ", "NJD", "NYI", "NYR","PHI", "PIT", "WSH") 
  teams.df <- rbind(cbind(pacific, "pacific", "WES"), cbind(central, "central", "WES"), cbind(atlantic, "atlantic", "EAS"), cbind(metropolitan, "metropolitan", "EAS"))
  colnames(teams.df) <- c("Team", "Division", "Conference")
  teams.df <- as.data.frame(teams.df)
  
  post_URL <- paste0("/",seasons,"_games.html")
  current_records <- lapply(teams, function(x) readHTMLTable(getURL(paste0("https://www.hockey-reference.com/teams/",x,post_URL,"")), header=T))
  names(current_records) <- paste0(teams, seasons)

#Clean up the table grabbed for each team
  current.records.clean <- lapply(current_records, function(x) clean_results(x))

#Create new columns for each team
  for(i in seq_along(teams)){
    #Add in a column for the number of points over 82 games for each of the preceding years, along with a linear line representing
    #the 96 point pace
    t2019 <- paste0(teams[i],2019)
    t2018 <- paste0(teams[i],2018)
    t2017 <- paste0(teams[i],2017)
    t2016 <- paste0(teams[i],2016)
    t2015 <- paste0(teams[i],2015)
    
    if(nrow(current.records.clean[[i]]) < 82){
      current.records.clean[[i]] <- rbind(current.records.clean[[i]], list(82,NA,NA,NA,NA,NA,NA,NA,NA))
    }
    #Exception for Vegas
    if(t2019 == "VEG2019"){
      print(t2019)
      current.records.clean[[t2019]]$P2015 <- 0
      current.records.clean[[t2019]]$P2016 <- 0
      current.records.clean[[t2019]]$P2017 <- 0
      current.records.clean[[t2019]]$P2018 <- season2018.records.clean[[t2018]]$Points
      current.records.clean[[t2019]]$Snake <- current.records.clean[[t2019]]$GP * (96/82.0)
      
      #Calculate the point differential to the 96 point pace for each game
      current.records.clean[[t2019]]$P19_snake <- current.records.clean[[t2019]]$Points - current.records.clean[[t2019]]$Snake
      current.records.clean[[t2019]]$P18_snake <- current.records.clean[[t2019]]$P2018 - current.records.clean[[t2019]]$Snake
      current.records.clean[[t2019]]$P17_snake <- 0
      current.records.clean[[t2019]]$P16_snake <- 0
      current.records.clean[[t2019]]$P15_snake <- 0
      
    }
    else {
      current.records.clean[[t2019]]$P2015 <- season_records2015.clean[[t2015]]$Points
      current.records.clean[[t2019]]$P2016 <- season_records2016.clean[[t2016]]$Points
      current.records.clean[[t2019]]$P2017 <- season2017.records.clean[[t2017]]$Points
      current.records.clean[[t2019]]$P2018 <- season2018.records.clean[[t2018]]$Points
      current.records.clean[[t2019]]$Snake <- current.records.clean[[t2019]]$GP * (96/82.0)
      
      #Calculate the point differential to the 96 point pace for each game
      current.records.clean[[t2019]]$P19_snake <- current.records.clean[[t2019]]$Points - current.records.clean[[t2019]]$Snake
      current.records.clean[[t2019]]$P18_snake <- current.records.clean[[t2019]]$P2018 - current.records.clean[[t2019]]$Snake
      current.records.clean[[t2019]]$P17_snake <- current.records.clean[[t2019]]$P2017 - current.records.clean[[t2019]]$Snake
      current.records.clean[[t2019]]$P16_snake <- current.records.clean[[t2019]]$P2016 - current.records.clean[[t2019]]$Snake
      current.records.clean[[t2019]]$P15_snake <- current.records.clean[[t2019]]$P2015 - current.records.clean[[t2019]]$Snake
    }
  }

##Get current points above the snake
current.snake.diff <- lapply(current.records.clean, function(x) x$P19_snake[!is.na(x$P19_snake)]  %>% last)
current.snake.diff <- as.data.frame(unlist(current.snake.diff))
names(current.snake.diff) <- "PointsDiff"
current.snake.diff <- rownames_to_column(current.snake.diff, "Team") %>% arrange(desc(PointsDiff))
current.snake.diff$Team <- gsub('[[:digit:]]+', '', current.snake.diff$Team)
current.snake.table <- left_join(teams.df, current.snake.diff, by="Team")
  
#Saving files to use in app    
names(current.records.clean) <- paste0(teams, seasons)
save(current.records.clean, file=paste0(rootp,"current_records.RData"))
save(current.snake.table, file=paste0(rootp,"current_snake_diff.RData"))
  
  