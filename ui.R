# Rely on the 'WorldPhones' dataset in the datasets
# package (which generally comes preloaded).
library(datasets)

# Use a fluid Bootstrap layout
fluidPage(    
  
  # Give the page a title
  titlePanel("NHL Playoff Snake"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      selectInput("chtype", "Chart type:", 
                  choices=c("ANA", "ARI", "BOS", "BUF", "CAR", "CGY", "CHI", "CBJ", "COL", "DAL", "DET", "EDM", "FLA", "LAK", "MIN", "MTL", "NSH", "NJD", "NYI", "NYR", "OTT", "PHI", "PIT", "SJS", "STL", "TBL", "TOR", "VAN", "WSH", "WPG"),
                  selected = "CGY"),                  
      hr(),
      helpText("Plots how the Calgary Flames are doing in relation to the pace of a 96 point season."),
      br(),
      radioButtons("snakeType", "Snake type",
                   c("Traditional snake"="t", "Smart snake"="s")
      )
    ),
    
    # Create a spot for the barplot
    mainPanel(
      plotlyOutput("phonePlot"),
      helpText("Black - 96 point pace, red - 16-17 season, purple - 15-16 season, green - 14-15 season, (Smart snake only: gray - 96 point pace adjusted for opposition strength)"),
      helpText("Playoff Snake idea adapted from ", a("Calgary Puck Forum", href="http://forum.calgarypuck.com/showthread.php?t=157390"))
      #textOutput("phonePlot")  
    )
    
  )
)
