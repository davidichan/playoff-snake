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
load(paste0(rootp,"season_records_2017.RData"))
load(paste0(rootp,"current_records.RData"))
load(paste0(rootp,"current_snake_diff.RData"))

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
  
  output$SnakePlot <- renderPlotly({ 
#    if(input$snakeType == "t"){
      t_name <- paste0(input$chtype, "2018")
      print(t_name)
      #trad <- ggplot(data= season2017.records.clean[[t_name]], aes(x=GP, y=Points)) + geom_line(colour="red", size=1) + geom_line(aes(x=GP, y=P2016), colour="purple4", size=0.75) + geom_line(aes(x=GP, y=P2015), colour="forestgreen", size=0.75) + geom_line(aes(x=GP, y=Snake), size=0.75) + snake_labs + snake_lims
      trad <- plot_ly(data = current.records.clean[[t_name]], x = ~GP, 
        y = ~Points, mode = "lines", name = '2017-18 Season', type = 'scatter', mode = 'lines', line = list(color = 'red', width = 3)) %>% 
        add_trace(y = ~P2017, name = '2016-17 Season', line = list(color = 'blue', width = 2)) %>% 
        add_trace(y = ~P2016, name = '2015-16 Season', line = list(color = 'purple', width = 2), visible = "legendonly") %>% 
        add_trace(y = ~P2015, name = '2014-15 Season', line = list(color = 'green', width = 2), visible = "legendonly") %>% 
        add_trace(y = ~Snake, name = '96 Point pace', line = list(color = 'black', width = 2), showlegend = F) %>%
        layout(xaxis = list(title = "Games Played"), yaxis = list (title = "Points", range = c(0,110)))
      
      trad_diff <- plot_ly(data = current.records.clean[[t_name]], x = ~GP, 
        y = ~P18_snake, mode = "lines", name = '2017-18 differential', type = 'scatter', mode = 'lines', line = list(color = 'red', width = 3)) %>% 
        add_trace(y = ~P17_snake, name = '2016-17 differential', line = list(color = 'blue', width = 2)) %>% 
        add_trace(y = ~P16_snake, name = '2015-16 differential', line = list(color = 'purple', width = 2), visible = "legendonly") %>% 
        add_trace(y = ~P15_snake, name = '2014-15 differential', line = list(color = 'green', width = 2), visible = "legendonly") %>% 
        layout(xaxis = list(title = "Games Played"), yaxis = list (title = "Point diff. to snake", range = c(-15,15))) %>% 
        add_trace(x = c(0, 82), y = c(0, 0), line=list(color='black', width=2), name = 'Snake', showlegend = F)
      subplot(trad, trad_diff, nrows = 2, shareX = T, titleX = T, titleY = T)
#    } else if(input$snakeType == "s"){
#      t_name <- paste0(input$chtype, "2017")

#      smart_snake <- plot_ly(data = season2017.records.clean[[t_name]], x = ~GP, y = ~Points, mode = "lines", name = '2016-17 Season', type = 'scatter', mode = 'lines', line = list(color = 'red', width = 3)) %>% add_trace(y = ~P2016, name = '2015-2016 Season', line = list(color = 'purple', width = 2)) %>% add_trace(y = ~P2015, name = '2015-16 Season', line = list(color = 'green', width = 2)) %>% add_trace(y = ~Snake, name = '96 Point pace', line = list(color = 'black', width = 2), showlegend = F) %>%
#        layout(xaxis = list(title = "Games Played"), yaxis = list (title = "Points", range = c(0,110))) %>% add_trace(y = ~cumsum(SS17), name = 'Smart snake', line = list(color = 'black', width = 2, dash = 'dot'))

#      smart_diff <- plot_ly(data = season2017.records.clean[[t_name]], x = ~GP, y = ~deltaSS17, mode = "lines", name = '2016-17 Season', type = 'scatter', mode = 'lines', line = list(color = 'red', width = 3)) %>% layout(xaxis = list(title = "Games Played"), yaxis = list (title = "Point diff. to snake", range = c(-15,15)))  %>% add_trace(x = c(0, 82), y = c(0, 0), line=list(color='black', width=2), name = 'Snake', showlegend = F)
#      subplot(smart_snake, smart_diff, nrows = 2, shareX = T, titleX = T, titleY = T)
    #}
  })
  output$EAStable <- renderTable(
    current.snake.table  %>% filter(Conference == "EAS") %>% arrange(desc(PointsDiff)) %>% select(Team, PointsDiff)) 
  output$WEStable <- renderTable(
    current.snake.table  %>% filter(Conference == "WES") %>% arrange(desc(PointsDiff)) %>% select(Team, PointsDiff)) 
}
