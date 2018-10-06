# playoff-snake

[Playoff snake](https://bitly.com/playoff-snake-2018)

Shiny application that displays each NHL team's points and how this compares to the pace of a 96 point season. The idea for the playoff snake was conceived by Squiggs96 and Bunk at [Calgary Puck forums.](http://forum.calgarypuck.com/showthread.php?t=157390) From the forum: "The number of points to make the playoffs may not be 96 this year, but no team in the history of the western conference has missed the playoffs with 96 points. Yes, there is a debate over the 2010-11 and 2014-15 seasons, but no team in the western conference has achieved 96 points and missed the playoffs. Only one team in the east, the 2014-15 Bruins, achieved 96 points and missed." 

The app displays both the total number of points for each team and how far off the pace the team is in the lower panel. The graphs were created using the plotly library in R, which means that they are interactive and various lines can be toggled on and off. The data is updated every hour and grabbed from hockey-reference.com.

In addition to the standard snake, I also calculate an 96 point pace adjusted for the quality of each team's opposition. For example, if the team is playing a team that did well in the previous season (e.g. Washington), the snake moves up less, since you expect to have a lower chance of winning that game compared to when playing a team that did poorly the previous season (e.g. Arizona). The season total for that snake still adds up to 96 points. 
