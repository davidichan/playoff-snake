library(plotly)

# Use a fluid Bootstrap layout
fluidPage(    
  
  # Give the page a title
  titlePanel("NHL Playoff Snake - 2018-19"),
  
  # Generate a row with a sidebar
  fluidRow(
    column(4, 
    
    # Define the sidebar with one input
    wellPanel(
      selectInput("chtype", "Team selection:", 
                  choices=c("ANA", "ARI", "BOS", "BUF", "CAR", "CBJ", "CGY", "CHI", "COL", "DAL", "DET", "EDM", "FLA", "LAK", "MIN", "MTL", "NSH", "NJD", "NYI", "NYR", "OTT", "PHI", "PIT", "SJS", "STL", "TBL", "TOR", "VAN", "VEG", "WSH", "WPG"),
                  selected = "CGY"),            
      helpText("Plots how each team is doing in relation to the pace of a 96 point season as a proxy for whether a team is on track to make the playoffs. While 96 points do not guarantee a playoff birth, at least in the Western Conference, no team has missed the playoffs when they reached that point total."),
      checkboxInput("comp", "Compare teams "),
      conditionalPanel(
        condition = "input.comp == true",
        selectInput("compteam", "Team to compare:", 
                    choices=c("ANA", "ARI", "BOS", "BUF", "CAR", "CBJ", "CGY", "CHI", "COL", "DAL", "DET", "EDM", "FLA", "LAK", "MIN", "MTL", "NSH", "NJD", "NYI", "NYR", "OTT", "PHI", "PIT", "SJS", "STL", "TBL", "TOR", "VAN","VEG", "WSH", "WPG"),
                    selected = "ANA")       
      )
      ),
    
    column(8,
    sliderInput("pace", "Choose points pace:",
                min = 90, max = 100,
                value = 96)),

    fluidRow(
      headerPanel("Standings"),
    column(12,
      helpText("Based on points difference to the 96 Point snake")),
    column(6,
      "Eastern Conference Standings",
      wellPanel( 
      tableOutput('EAStable'))),
    column(6,
      "Western Conference Standings",
      wellPanel(
      tableOutput('WEStable')))
    )),
    
    # Create a spot for the barplot
    column(8,
      plotlyOutput("SnakePlot"),
      helpText("Thank you to the ", a("Calgary Puck Forum", href="http://forum.calgarypuck.com/showthread.php?t=157390"), " for the idea for the Playoff Snake.")
      #textOutput("phonePlot")  
    )
    
  )
)
