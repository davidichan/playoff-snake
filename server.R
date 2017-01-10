library(datasets)
library(ggplot2)
library(RCurl)
library(XML)
library(plyr)
#library(plotly)
#Sys.setenv("plotly_username"="hansvid")
#Sys.setenv("plotly_api_key"="")

rootp <- "data/"
load(paste0(rootp,"NHL_standings_2016.RData"))
load(paste0(rootp,"cgy_points_hist.RData"))

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
  seasons <- c("2017")
  cgy.records <- lapply(seasons, function(x) readHTMLTable(paste0("http://www.hockey-reference.com/teams/CGY/",x,"_games.html",""), header=T))
  
  #cgy.summary.df <- data.frame(matrix(NA, nrow = 0, ncol = 9))
  #results.clean <- rbind(cgy.summary.df, clean_results(cgy.records[[1]]))
  
  cgy.records.clean <- lapply(cgy.records, function(x) clean_results(x))
  
  cgy.summary <- cgy.records.clean[[1]]
  cgy.summary$P2016 <- cgy.pts.2016$Points
  cgy.summary$P2015 <- cgy.pts.2015$Points
  cgy.summary$Snake <- cgy.summary$GP * (96/82.0)
  
  cgy.summary$P17_snake <- cgy.summary$Points - cgy.summary$Snake
  cgy.summary$P16_snake <- cgy.summary$P2016 - cgy.summary$Snake
  cgy.summary$P15_snake <- cgy.summary$P2015 - cgy.summary$Snake
  
  cgy.summary <- join(cgy.summary, points16, by = "Opponent")
  colnames(cgy.summary)[colnames(cgy.summary) == "PTS"] <- "Pts_ssnake16"
  
  #For the adjusted snake, taking into account the strength of the opponents
  #Game against worst team is worth 96/82 + 1 points, against best team, is worth 96/82 + 1 pts
  #Using a range of +/- 27 points as the min and max points
  cgy.summary$SS17 <- cgy.summary$Pts_ssnake16/(-27) + 4.726
  #Using this method, the Flames faced disproportionately poor opposition and sum of snake points would
  #have been >96 points. Therefore, sum up the points, divide and readjust each point total to sum to 96. 
  cgy.summary$SS17 <- cgy.summary$SS17 / 110.2727 * 96
  
  output$phonePlot <- renderPlot({ 
   if(input$chtype == "Traditional"){
    (ggplot(data= cgy.summary, aes(x=GP, y=Points)) + geom_line(colour="red", size=1) + geom_line(aes(x=GP, y=P2016), colour="purple4", size=0.75) + geom_line(aes(x=GP, y=P2015), colour="forestgreen", size=0.75) + geom_line(aes(x=GP, y=Snake), size=0.75) + xlab("Games Played") + ylab("Points") + xlab("Games Played") + ylab("Points"))
    } else if(input$chtype == "Point difference to snake") {
     ggplot(data= cgy.summary, aes(x=GP, y=P17_snake)) + geom_line(colour="red", size=1) + geom_line(aes(x=GP, y=P16_snake), colour="purple4", size=0.75) + geom_line(aes(x=GP, y=P15_snake), colour="forestgreen", size=0.75) + xlab("Games Played") + ylab("Point behind snake") + scale_colour_manual(values =c("red","green","forestgreen")) + xlab("Games Played") + ylab("Points behind snake") + geom_hline(yintercept = 0, size=0.75)
    } else {
      ggplot(data= cgy.summary, aes(x=GP, y=Points)) + geom_line(colour="red", size=1) + ylim(c(0,100)) + geom_line(aes(x=GP, y=P2016), colour="purple4", size=0.75) + geom_line(aes(x=GP, y=P2015), colour="forestgreen", size=0.75) + geom_line(aes(x=GP, y=cumsum(SS17)), colour="gray", size=0.75) + geom_line(aes(x=GP, y=Snake), colour="black", size=0.75) + xlab("Games Played") + ylab("Points")
    }
  })

}