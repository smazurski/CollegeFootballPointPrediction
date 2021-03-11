#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(lubridate)
library(stringr)
library(ggplot2)
library(DBI)
library(stats)
library(htmltools)
library(shiny)
library(tidyverse)
library(glmnet)
library(plotly)
library(shinythemes)
library(rvest)
library(dplyr)
library(tidyverse)
library(stringr)


oppg <- url("https://www.sports-reference.com/cfb/years/2020-team-offense.html#offense::none")
oppg <- read_html(oppg)


### Offense PPG ###

oppg_teams <- oppg %>%
  html_nodes(xpath = "//td/a") %>% 
  html_text()

oppg_teams <- as.data.frame(oppg_teams)

o_points <- oppg %>%
  html_nodes(".right:nth-child(4)") %>% 
  html_text()


o_points <- as.data.frame(o_points)


o_points$o_points <- as.character(o_points$o_points)
o_points <- o_points[o_points$o_points != "Pts",]
o_points <- as.data.frame(o_points)
o_points <- o_points[1:128,1]
o_points <- as.data.frame(o_points)


oppg_teams$oppg <- o_points$o_points 

oppg_teams <- oppg_teams %>%
  mutate(oppg=as.numeric(levels(oppg))[oppg])

colnames(oppg_teams)[1] <- "teams"


### Offense PPG ###

dppg <- url("https://www.sports-reference.com/cfb/years/2020-team-defense.html#defense::none")
dppg <- read_html(dppg)


dppg_teams <- dppg %>%
  html_nodes(xpath = "//td/a") %>% 
  html_text()

dppg_teams <- as.data.frame(dppg_teams)

d_points <- dppg %>%
  html_nodes(".right:nth-child(4)") %>% 
  html_text()

d_points <- as.data.frame(d_points)
d_points$d_points <- as.character(d_points$d_points)
d_points <- d_points[d_points$d_points != "Pts",]
d_points <- as.data.frame(d_points)
d_points <- d_points[1:128,1]
d_points <- as.data.frame(d_points)


dppg_teams$dppg <- d_points$d_points 

dppg_teams <- dppg_teams %>%
  mutate(dppg=as.numeric(levels(dppg))[dppg])

colnames(dppg_teams)[1] <- "teams"

cfb_teams <- merge(oppg_teams, dppg_teams, by="teams")


### SOS ###

sos <- url("https://www.sports-reference.com/cfb/schools/#schools::none")
sos <- read_html(sos)

sos_teams <- sos %>%
  html_nodes(xpath = "//td/a") %>% 
  html_text()

sos_teams <- as.data.frame(sos_teams)

sos_teams <- sos_teams %>%
  filter(str_detect(sos_teams, "adjusted", negate = TRUE))

sos_teams$sos_teams <- str_replace(sos_teams$sos_teams,"BYU", "Brigham Young")
sos_teams$sos_teams <- str_replace(sos_teams$sos_teams,"Nevada-Las Vegas", "UNLV")


sos_points <- sos %>%
  html_nodes(".right:nth-child(17)") %>% 
  html_text()

sos_points <- as.data.frame(sos_points)
sos_points$sos_points <- as.character(sos_points$sos_points)
sos_points <- sos_points[sos_points$sos_points != "SOS",]
sos_points <- as.data.frame(sos_points)

sos_teams$sos <- sos_points$sos_points 

sos_teams <- sos_teams %>%
  mutate(sos=as.numeric(levels(sos))[sos])

colnames(sos_teams)[1] <- "teams"

cfb_teams <- merge(cfb_teams, sos_teams, by="teams")

cfb_teams <- cfb_teams %>%
  filter(teams!='New Mexico State')


### ESPN FPI  ###
rank <- url("https://www.espn.com/college-football/fpi")
rank <- read_html(rank)

fpi_teams <- rank %>%
  html_nodes(".Table__TD") %>%
  html_text()

fpi_teams <- as.data.frame(fpi_teams)
fpi_teams <- fpi_teams[1:127,]
fpi_teams <- as.data.frame(fpi_teams)


fpi <- rank %>%
  html_nodes(".Table__TD:nth-child(2)") %>%
  html_text()

fpi <- as.data.frame(fpi)

fpi_teams$fpi <- fpi$fpi

fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Alabama Crimson Tide","Air Force")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Air Force Falcons","Akron")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Akron Zips","Alabama")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Appalachian State Mountaineers","Appalachian State")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Arizona State Sun Devils","Arizona State")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Arizona Wildcats","Arizona")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Arkansas Razorbacks","Arkansas")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Arkansas State Red Wolves","Arkansas State")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Army Black Knights","Army")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Auburn Tigers","Auburn")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Ball State Cardinals","Ball State")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Baylor Bears","Baylor")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Boise State Broncos","Boise State")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Boston College Eagles","Boston College")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Bowling Green Falcons","Bowling Green State")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Buffalo Bulls","Buffalo")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"BYU Cougars","Brigham Young")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"California Golden Bears","California")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Central Michigan Chippewas","Central Michigan")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Charlotte 49ers","Charlotte")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Cincinnati Bearcats","Cincinnati")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Clemson Tigers","Clemson")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Coastal Carolina Chanticleers","Coastal Carolina")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Colorado Buffaloes","Colorado")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Colorado State Rams","Colorado State")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Duke Blue Devils","Duke")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"East Carolina Pirates","East Carolina")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Eastern Michigan Eagles","Eastern Michigan")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Florida Atlantic Owls","Florida Atlantic")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Florida Gators","Florida")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Florida International Panthers","Florida International")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Florida State Seminoles","Florida State")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Fresno State Bulldogs","Fresno State")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Georgia Bulldogs","Georgia")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Georgia Southern Eagles","Georgia Southern")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Georgia State Panthers","Georgia State")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Georgia Tech Yellow Jackets","Georgia Tech")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Hawai'i Rainbow Warriors","Hawaii")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Houston Cougars","Houston")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Illinois Fighting Illini","Illinois")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Indiana Hoosiers","Indiana")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Iowa Hawkeyes","Iowa")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Iowa State Cyclones","Iowa State")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Kansas Jayhawks","Kansas")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Kansas State Wildcats","Kansas State")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Kent State Golden Flashes","Kent State")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Kentucky Wildcats","Kentucky")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Liberty Flames","Liberty")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Louisiana Ragin' Cajuns","Louisiana")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Louisiana Tech Bulldogs","Louisiana Tech")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Louisville Cardinals","Louisville")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"LSU Tigers","LSU")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Marshall Thundering Herd","Marshall")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Maryland Terrapins","Maryland")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Memphis Tigers","Memphis")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Miami \\(OH\\) RedHawks","Miami (OH)")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Miami Hurricanes","Miami (FL)")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Michigan State Spartans","Michigan State")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Michigan Wolverines","Michigan")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Middle Tennessee Blue Raiders","Middle Tennessee State")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Minnesota Golden Gophers","Minnesota")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Mississippi State Bulldogs","Mississippi State")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Missouri Tigers","Missouri")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Navy Midshipmen","Navy")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"NC State Wolfpack","North Carolina State")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Nebraska Cornhuskers","Nebraska")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Nevada Wolf Pack","Nevada")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"New Mexico Lobos","New Mexico")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"North Carolina Tar Heels","North Carolina")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"North Texas Mean Green","North Texas")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Northern Illinois Huskies","Northern Illinois")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Northwestern Wildcats","Northwestern")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Notre Dame Fighting Irish","Notre Dame")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Ohio Bobcats","Ohio")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Ohio State Buckeyes","Ohio State")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Oklahoma Sooners","Oklahoma")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Oklahoma State Cowboys","Oklahoma State")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Ole Miss Rebels","Ole Miss")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Oregon Ducks","Oregon")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Oregon State Beavers","Oregon State")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Penn State Nittany Lions","Penn State")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Pittsburgh Panthers","Pitt")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Purdue Boilermakers","Purdue")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Rice Owls","Rice")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Rutgers Scarlet Knights","Rutgers")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"San Diego State Aztecs","San Diego State")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"San José State Spartans","San Jose State")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"SMU Mustangs","SMU")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"South Alabama Jaguars","South Alabama")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"South Carolina Gamecocks","South Carolina")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"South Florida Bulls","South Florida")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Southern Mississippi Golden Eagles","Southern Mississippi")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Stanford Cardinal","Stanford")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Syracuse Orange","Syracuse")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"TCU Horned Frogs","Texas Christian")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Temple Owls","Temple")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Tennessee Volunteers","Tennessee")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Texas A&M Aggies","Texas A&M")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Texas Longhorns","Texas")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Texas State Bobcats","Texas State")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Texas Tech Red Raiders","Texas Tech")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Toledo Rockets","Toledo")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Troy Trojans","Troy")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Tulane Green Wave","Tulane")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Tulsa Golden Hurricane","Tulsa")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"UAB Blazers","UAB")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"UCF Knights","UCF")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"UCLA Bruins","UCLA")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"UL Monroe Warhawks","Louisiana-Monroe")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"UMass Minutemen","Massachusetts")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"UNLV Rebels","UNLV")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"USC Trojans","USC")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Utah State Aggies","Utah")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Utah Utes","Utah State")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"UTEP Miners","UTEP")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"UTSA Roadrunners","UTSA")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Vanderbilt Commodores","Vanderbilt")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Virginia Cavaliers","Virginia")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Virginia Tech Hokies","Virginia Tech")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Wake Forest Demon Deacons","Wake Forest")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Washington Huskies","Washington")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Washington State Cougars","Washington State")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"West Virginia Mountaineers","West Virginia")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Western Kentucky Hilltoppers","Western Kentucky")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Western Michigan Broncos","Western Michigan")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Wisconsin Badgers","Wisconsin")
fpi_teams$fpi_teams <- str_replace(fpi_teams$fpi_teams,"Wyoming Cowboys","Wyoming")


colnames(fpi_teams)[1] <- "teams"

cfb_teams <- merge(cfb_teams, fpi_teams, by="teams")

# Define UI for application that draws a histogram
ui <- fluidPage(
  
  
  # Application title
  titlePanel("CFB Score Prediction"),
  h4("Select Teams for Point Prediction"),
  theme = shinytheme("cyborg"),
  # Sidebar with input for number of bins 
  sidebarLayout(
    sidebarPanel(
      selectInput('home_team', 'Select Home Team', choices = c("Alabama", 
                                                               "Akron", 
                                                               "Air Force", 
                                                               "Appalachian State", 
                                                               "Arizona State", 
                                                               "Arizona", 
                                                               "Arkansas", 
                                                               "Arkansas State", 
                                                               "Army", 
                                                               "Auburn", 
                                                               "Ball State", 
                                                               "Baylor", 
                                                               "Boise State", 
                                                               "Boston College", 
                                                               "Bowling Green State", 
                                                               "Buffalo", 
                                                               "Brigham Young", 
                                                               "California", 
                                                               "Central Michigan", 
                                                               "Charlotte", 
                                                               "Cincinnati", 
                                                               "Clemson", 
                                                               "Coastal Carolina", 
                                                               "Colorado", 
                                                               "Colorado State", 
                                                               "Duke", 
                                                               "East Carolina", 
                                                               "Eastern Michigan", 
                                                               "Florida Atlantic", 
                                                               "Florida", 
                                                               "Florida International", 
                                                               "Florida State", 
                                                               "Fresno State", 
                                                               "Georgia", 
                                                               "Georgia Southern", 
                                                               "Georgia State", 
                                                               "Georgia Tech", 
                                                               "Hawaii", 
                                                               "Houston", 
                                                               "Illinois", 
                                                               "Indiana", 
                                                               "Iowa", 
                                                               "Iowa State", 
                                                               "Kansas", 
                                                               "Kansas State", 
                                                               "Kent State", 
                                                               "Kentucky", 
                                                               "Liberty", 
                                                               "Louisiana", 
                                                               "Louisiana Tech", 
                                                               "Louisville", 
                                                               "LSU", 
                                                               "Marshall", 
                                                               "Maryland", 
                                                               "Memphis", 
                                                               "Miami (OH)", 
                                                               "Miami (FL)", 
                                                               "Michigan State", 
                                                               "Michigan", 
                                                               "Middle Tennessee State", 
                                                               "Minnesota", 
                                                               "Mississippi State", 
                                                               "Missouri", 
                                                               "Navy", 
                                                               "North Carolina State", 
                                                               "Nebraska", 
                                                               "Nevada", 
                                                               "New Mexico", 
                                                               "North Carolina", 
                                                               "North Texas", 
                                                               "Northern Illinois", 
                                                               "Northwestern", 
                                                               "Notre Dame", 
                                                               "Ohio", 
                                                               "Ohio State", 
                                                               "Oklahoma", 
                                                               "Oklahoma State", 
                                                               "Ole Miss", 
                                                               "Oregon", 
                                                               "Oregon State", 
                                                               "Penn State", 
                                                               "Pitt", 
                                                               "Purdue", 
                                                               "Rice", 
                                                               "Rutgers", 
                                                               "San Diego State", 
                                                               "San Jose State", 
                                                               "SMU", 
                                                               "South Alabama", 
                                                               "South Carolina", 
                                                               "South Florida", 
                                                               "Southern Mississippi", 
                                                               "Stanford", 
                                                               "Syracuse", 
                                                               "Texas Christian", 
                                                               "Temple", 
                                                               "Tennessee", 
                                                               "Texas A&M", 
                                                               "Texas", 
                                                               "Texas State", 
                                                               "Texas Tech", 
                                                               "Toledo", 
                                                               "Troy", 
                                                               "Tulane", 
                                                               "Tulsa", 
                                                               "UAB", 
                                                               "UCF", 
                                                               "UCLA", 
                                                               "Louisiana-Monroe", 
                                                               "Massachusetts", 
                                                               "UNLV", 
                                                               "USC", 
                                                               "Utah", 
                                                               "Utah State", 
                                                               "UTEP", 
                                                               "UTSA", 
                                                               "Vanderbilt", 
                                                               "Virginia", 
                                                               "Virginia Tech", 
                                                               "Wake Forest", 
                                                               "Washington", 
                                                               "Washington State", 
                                                               "West Virginia", 
                                                               "Western Kentucky", 
                                                               "Western Michigan", 
                                                               "Wisconsin", 
                                                               "Wyoming"), selectize=TRUE) ,
      
      selectInput('away_team', 'Select Away Team', choices = c("Auburn", 
                                                               "Akron", 
                                                               "Alabama", 
                                                               "Appalachian State", 
                                                               "Arizona State", 
                                                               "Arizona", 
                                                               "Arkansas", 
                                                               "Arkansas State", 
                                                               "Army", 
                                                               "Air Force", 
                                                               "Ball State", 
                                                               "Baylor", 
                                                               "Boise State", 
                                                               "Boston College", 
                                                               "Bowling Green State", 
                                                               "Buffalo", 
                                                               "Brigham Young", 
                                                               "California", 
                                                               "Central Michigan", 
                                                               "Charlotte", 
                                                               "Cincinnati", 
                                                               "Clemson", 
                                                               "Coastal Carolina", 
                                                               "Colorado", 
                                                               "Colorado State", 
                                                               "Duke", 
                                                               "East Carolina", 
                                                               "Eastern Michigan", 
                                                               "Florida Atlantic", 
                                                               "Florida", 
                                                               "Florida International", 
                                                               "Florida State", 
                                                               "Fresno State", 
                                                               "Georgia", 
                                                               "Georgia Southern", 
                                                               "Georgia State", 
                                                               "Georgia Tech", 
                                                               "Hawaii", 
                                                               "Houston", 
                                                               "Illinois", 
                                                               "Indiana", 
                                                               "Iowa", 
                                                               "Iowa State", 
                                                               "Kansas", 
                                                               "Kansas State", 
                                                               "Kent State", 
                                                               "Kentucky", 
                                                               "Liberty", 
                                                               "Louisiana", 
                                                               "Louisiana Tech", 
                                                               "Louisville", 
                                                               "LSU", 
                                                               "Marshall", 
                                                               "Maryland", 
                                                               "Memphis", 
                                                               "Miami (OH)", 
                                                               "Miami (FL)", 
                                                               "Michigan State", 
                                                               "Michigan", 
                                                               "Middle Tennessee State", 
                                                               "Minnesota", 
                                                               "Mississippi State", 
                                                               "Missouri", 
                                                               "Navy", 
                                                               "North Carolina State", 
                                                               "Nebraska", 
                                                               "Nevada", 
                                                               "New Mexico", 
                                                               "North Carolina", 
                                                               "North Texas", 
                                                               "Northern Illinois", 
                                                               "Northwestern", 
                                                               "Notre Dame", 
                                                               "Ohio", 
                                                               "Ohio State", 
                                                               "Oklahoma", 
                                                               "Oklahoma State", 
                                                               "Ole Miss", 
                                                               "Oregon", 
                                                               "Oregon State", 
                                                               "Penn State", 
                                                               "Pitt", 
                                                               "Purdue", 
                                                               "Rice", 
                                                               "Rutgers", 
                                                               "San Diego State", 
                                                               "San Jose State", 
                                                               "SMU", 
                                                               "South Alabama", 
                                                               "South Carolina", 
                                                               "South Florida", 
                                                               "Southern Mississippi", 
                                                               "Stanford", 
                                                               "Syracuse", 
                                                               "Texas Christian", 
                                                               "Temple", 
                                                               "Tennessee", 
                                                               "Texas A&M", 
                                                               "Texas", 
                                                               "Texas State", 
                                                               "Texas Tech", 
                                                               "Toledo", 
                                                               "Troy", 
                                                               "Tulane", 
                                                               "Tulsa", 
                                                               "UAB", 
                                                               "UCF", 
                                                               "UCLA", 
                                                               "Louisiana-Monroe", 
                                                               "Massachusetts", 
                                                               "UNLV", 
                                                               "USC", 
                                                               "Utah", 
                                                               "Utah State", 
                                                               "UTEP", 
                                                               "UTSA", 
                                                               "Vanderbilt", 
                                                               "Virginia", 
                                                               "Virginia Tech", 
                                                               "Wake Forest", 
                                                               "Washington", 
                                                               "Washington State", 
                                                               "West Virginia", 
                                                               "Western Kentucky", 
                                                               "Western Michigan", 
                                                               "Wisconsin", 
                                                               "Wyoming"), selectize=TRUE)
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      tabsetPanel(tabPanel("Prediction", 
                           br(), 
                           tableOutput("distPlot"), 
                           tableOutput("distPlot2"), 
                           tableOutput("distPlot3"),
                           br(),
                           "Offensive and Defense PPG are the strongest predictors of predicted points scored.",
                           br(),
                           plotlyOutput("plot1"), 
                           br(),
                           "More points are given to teams with difficult schedules.",
                           br(), 
                           plotlyOutput("plot2"), 
                           br(),),
                  
      tabPanel("Data",tableOutput("distPlot4")))),
      ))


# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  output$distPlot <- renderText({
    
    home_display <- cfb_teams %>%
      filter(teams == input$home_team)

    home_display$oppg <- as.double(home_display$oppg)
    home_display$sos <- as.double(home_display$sos)
    home_display$fpi <- as.double(home_display$fpi)
    
    
    away_display <- cfb_teams %>%
      filter(teams == input$away_team)
    
    away_display$oppg <- as.double(away_display$oppg)
    away_display$sos <- as.double(away_display$sos)
    away_display$fpi <- as.double(away_display$fpi)
    away_display$dppg <- as.double(away_display$dppg)
        
    test <- (-25.02896+2.68273+(home_display$oppg*.87188)+(home_display$sos*.63419)+(home_display$fpi*.13285)+
               (away_display$dppg*.96935)+(away_display$sos*-.72343)+(away_display$fpi*-.06323))
  
    
    cbind(round(test, digits = 2)," Home Team Points")  
  })
  

  
  output$distPlot2 <- renderText({
    
    home_display <- cfb_teams %>%
      filter(teams == input$home_team)
    
    home_display$oppg <- as.double(home_display$oppg)
    home_display$sos <- as.double(home_display$sos)
    home_display$fpi <- as.double(home_display$fpi)
    
    
    away_display <- cfb_teams %>%
      filter(teams == input$away_team)
    
    away_display$oppg <- as.double(away_display$oppg)
    away_display$sos <- as.double(away_display$sos)
    away_display$fpi <- as.double(away_display$fpi)
    away_display$dppg <- as.double(away_display$dppg)
    
    test <- (-25.02896+(away_display$oppg*.87188)+(away_display$sos*.63419)+(away_display$fpi*.13285)+
               (home_display$dppg*.96935)+(home_display$sos*-.72343)+(home_display$fpi*-.06323))
    
    
    cbind(round(test, digits = 2)," Away Team Points")  
  })
  

  output$distPlot3 <- renderText({
    
    home_display <- cfb_teams %>%
      filter(teams == input$home_team)
    
    home_display$oppg <- as.double(home_display$oppg)
    home_display$sos <- as.double(home_display$sos)
    home_display$fpi <- as.double(home_display$fpi)
    
    
    away_display <- cfb_teams %>%
      filter(teams == input$away_team)
    
    away_display$oppg <- as.double(away_display$oppg)
    away_display$sos <- as.double(away_display$sos)
    away_display$fpi <- as.double(away_display$fpi)
    away_display$dppg <- as.double(away_display$dppg)
    
    
    homepoints <- (-25.02896+2.68273+(home_display$oppg*.87188)+(home_display$sos*.63419)+(home_display$fpi*.13285)+
               (away_display$dppg*.96935)+(away_display$sos*-.72343)+(away_display$fpi*-.06323))
    
    
    awaypoints <- (-25.02896+(away_display$oppg*.87188)+(away_display$sos*.63419)+(away_display$fpi*.13285)+
               (home_display$dppg*.96935)+(home_display$sos*-.72343)+(home_display$fpi*-.06323))
    
    
    test <- homepoints+awaypoints
    
    
    cbind(round(test, digits = 2)," Projected Over/Under")  

      })
  
    
  
  
  output$plot1 <- renderPlotly({
    
    plot1<-plot_ly(data = cfb_teams, 
                   x=cfb_teams$oppg,
                   y=cfb_teams$dppg,
                   color = cfb_teams$teams) %>%
      layout(xaxis = list(title = "Offensive Points Per Game", titlefont = list(family = "Courier New, monospace", size = 18, color = "#7f7f7f")), 
             yaxis = list(title = "Defensive Points Per Game", titlefont = list(family = "Courier New, monospace", size = 18, color = "#7f7f7f")))
    
  })
  
  
  
  output$plot2 <- renderPlotly({
    
    plot2<-plot_ly(data = cfb_teams, 
                   x=cfb_teams$oppg,
                   y=cfb_teams$sos,
                   color = cfb_teams$teams)
    
  })
  
  
  
  output$distPlot4 <- renderTable({
    
  cfb_teams
    
  })
  
    
    
  
}

# Run the application 
shinyApp(ui = ui, server = server)
