library(plotly)

# Use a fluid Bootstrap layout
fluidPage(    
  
  # Give the page a title
  titlePanel("NHL Playoff Snake"),
  
  # Generate a row with a sidebar
  sidebarLayout(      
    
    # Define the sidebar with one input
    sidebarPanel(
      selectInput("chtype", "Chart type:", 
                  choices=c("ANA", "ARI", "BOS", "BUF", "CAR", "CBJ", "CGY", "CHI", "COL", "DAL", "DET", "EDM", "FLA", "LAK", "MIN", "MTL", "NSH", "NJD", "NYI", "NYR", "OTT", "PHI", "PIT", "SJS", "STL", "TBL", "TOR", "VAN", "WSH", "WPG"),
                  selected = "CGY"),                  
      hr(),
      helpText("Plots how each team is doing in relation to the pace of a 96 point season."),
      br(),
      radioButtons("snakeType", "Snake type",
                   c("Traditional snake"="t", "Smart snake"="s")
      )
    ),
    
    # Create a spot for the barplot
    mainPanel(
      plotlyOutput("SnakePlot"),
      helpText("Thank you to the ", a("Calgary Puck Forum", href="http://forum.calgarypuck.com/showthread.php?t=157390"), " for the idea for the Playoff Snake.")
      #textOutput("phonePlot")  
    )
    
  )
)
