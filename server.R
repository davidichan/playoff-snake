library(datasets)
library(ggplot2)
library(RCurl)
library(XML)
library(plyr)
library(gridExtra)
#library(plotly)
#Sys.setenv("plotly_username"="hansvid")
#Sys.setenv("plotly_api_key"="H63Qw0HC3ly2j7JmqPLj")

rootp <- "data/"
load(paste0(rootp,"NHL_standings_2016.RData"))
load(paste0(rootp,"cgy_points_hist.RData"))
load(paste0(rootp,"season_records_2015.RData"))
load(paste0(rootp,"season_records_2016.RData"))

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

# Define a server for the Shiny app
function(input, output) {

#Select team:
  #Get 2017 team stats
  #Clean results 
  #Add  points from previous years, add snake columns
  #Plot


seasons <- c("2017")
teams <- c("ANA", "ARI", "BOS", "BUF", "CAR", "CGY", "CHI", "CBJ", "COL", "DAL", "DET", "EDM", "FLA", "LAK", "MIN", "MTL", "NSH", "NJD", "NYI", "NYR", "OTT", "PHI", "PIT", "SJS", "STL", "TBL", "TOR", "VAN", "WSH", "WPG")
post_URL <- paste0("/",seasons,"_games.html")
season2017_records <- lapply(teams, function(x) readHTMLTable(paste0("http://www.hockey-reference.com/teams/",x,post_URL,""), header=T))
names(season2017_records) <- paste0(teams, seasons)

season2017.records.clean <- lapply(season2017_records, function(x) clean_results(x))

  for(i in seq_along(teams)){
    season2017.records.clean[[i]]$P2015 <- season_records2015.clean[[i]]$Points
    season2017.records.clean[[i]]$P2016 <- season_records2016.clean[[i]]$Points
    season2017.records.clean[[i]]$Snake <- season2017.records.clean[[i]]$GP * (96/82.0)
    
    season2017.records.clean[[i]]$P17_snake <- season2017.records.clean[[i]]$Points - season2017.records.clean[[i]]$Snake
    season2017.records.clean[[i]]$P16_snake <- season2017.records.clean[[i]]$P2016 - season2017.records.clean[[i]]$Snake
    season2017.records.clean[[i]]$P15_snake <- season2017.records.clean[[i]]$P2015 - season2017.records.clean[[i]]$Snake
    
    season2017.records.clean[[i]] <- join(season2017.records.clean[[i]], points16, by = "Opponent")
    colnames(season2017.records.clean[[i]])[colnames(season2017.records.clean[[i]]) == "PTS"] <- "Pts_ssnake16"
    
    #For the adjusted snake, taking into account the strength of the opponents
    #Game against worst team is worth 96/82 + 1 points, against best team, is worth 96/82 + 1 pts
    #Using a range of +/- 27 points as the min and max points
    season2017.records.clean[[i]]$SS17 <- season2017.records.clean[[i]]$Pts_ssnake16/(-27) + 4.726
    #Using this method, the Flames faced disproportionately poor opposition and sum of snake points would
    #have been >96 points. Therefore, sum up the points, divide and readjust each point total to sum to 96. 
    season2017.records.clean[[i]]$SS17 <- season2017.records.clean[[i]]$SS17 / 110.2727 * 96
    season2017.records.clean[[i]]$deltaSS17 <- season2017.records.clean[[i]]$Points - cumsum(season2017.records.clean[[i]]$SS17)
  }

names(season2017.records.clean) <- paste0(teams, seasons)

#Plot params
diff_lims <- ylim(c(-30,20))
snake_lims <- ylim(c(0,120))
snake_labs <- list(xlab("Games Played"), ylab("Points"))
diff_labs <- list(xlab("Games Played"), ylab("Points behind snake"))

output$phonePlot <- renderPlot({ 
    t_name <- paste0(input$chtype, "2017")
    print(t_name)
    trad <- ggplot(data= season2017.records.clean[[t_name]], aes(x=GP, y=Points)) + geom_line(colour="red", size=1) + geom_line(aes(x=GP, y=P2016), colour="purple4", size=0.75) + geom_line(aes(x=GP, y=P2015), colour="forestgreen", size=0.75) + geom_line(aes(x=GP, y=Snake), size=0.75) + snake_labs + snake_lims
    trad_diff <- ggplot(data= season2017.records.clean[[t_name]], aes(x=GP, y=P17_snake)) + geom_line(colour="red", size=1) + geom_line(aes(x=GP, y=P16_snake), colour="purple4", size=0.75) + geom_line(aes(x=GP, y=P15_snake), colour="forestgreen", size=0.75) + diff_labs + geom_hline(yintercept = 0, size=0.75) + diff_lims
    grid.arrange(trad, trad_diff, ncol=1)
})
}