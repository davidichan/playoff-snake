library(ggplot2)
library(RCurl)
library(XML)
library(plyr)
library(gridExtra)
library(plotly)

rootp <- "data/"
load(paste0(rootp,"NHL_standings_2016.RData"))
load(paste0(rootp,"cgy_points_hist.RData"))
load(paste0(rootp,"season_records_2015.RData"))
load(paste0(rootp,"season_records_2016.RData"))
load(paste0(rootp,"current_records_2017.RData"))

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

  #Plot params
  diff_lims <- ylim(c(-30,20))
  snake_lims <- ylim(c(0,120))
  snake_labs <- list(xlab("Games Played"), ylab("Points"))
  diff_labs <- list(xlab("Games Played"), ylab("Points behind snake"))
  
  output$phonePlot <- renderPlotly({ 
    if(input$snakeType == "t"){
      t_name <- paste0(input$chtype, "2017")
      print(t_name)
      trad <- ggplot(data= season2017.records.clean[[t_name]], aes(x=GP, y=Points)) + geom_line(colour="red", size=1) + geom_line(aes(x=GP, y=P2016), colour="purple4", size=0.75) + geom_line(aes(x=GP, y=P2015), colour="forestgreen", size=0.75) + geom_line(aes(x=GP, y=Snake), size=0.75) + snake_labs + snake_lims
      trad_diff <- ggplot(data= season2017.records.clean[[t_name]], aes(x=GP, y=P17_snake)) + geom_line(colour="red", size=1) + geom_line(aes(x=GP, y=P16_snake), colour="purple4", size=0.75) + geom_line(aes(x=GP, y=P15_snake), colour="forestgreen", size=0.75) + diff_labs + geom_hline(yintercept = 0, size=0.75) + diff_lims
      subplot(trad, trad_diff, nrows = 2, shareX = T, titleX = T, titleY = T)
    } else if(input$snakeType == "s"){
      t_name <- paste0(input$chtype, "2017")
      smart_snake <- ggplot(data= season2017.records.clean[[t_name]], aes(x=GP, y=Points)) + geom_line(colour="red", size=1) + geom_line(aes(x=GP, y=P2016), colour="purple4", size=0.75) + geom_line(aes(x=GP, y=P2015), colour="forestgreen", size=0.75) + geom_line(aes(x=GP, y=cumsum(SS17)), colour="gray", size=0.75) + geom_line(aes(x=GP, y=Snake), colour="black", size=0.75) + snake_labs + snake_lims
      smart_diff <- ggplot(data= season2017.records.clean[[t_name]], aes(x=GP, y=deltaSS17)) + geom_line(colour="red", size=1) + diff_labs + geom_hline(yintercept = 0, size=0.75) + diff_lims #+ geom_line(aes(x=GP, y=P16_snake), colour="purple4", size=0.75) + geom_line(aes(x=GP, y=P15_snake), colour="forestgreen", size=0.75) 
      subplot(smart_snake, smart_diff, nrows = 2, shareX = T, titleX = T, titleY = T)
    }
  })
}